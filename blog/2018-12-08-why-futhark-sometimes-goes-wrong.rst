---
title: Why Futhark (sometimes) goes wrong
author: Troels Henriksen
description: The Futhark compiler will sometimes refuse to compile a program, possibly with an incomprehensible apology about some compiler limitation. In this post, I will try to explain what exactly that means, why it's not easy to fix, and how we might fix it eventually.
---

Most Futhark programmers have probably happened upon the dreaded
*compiler limitation encountered* message, where the compiler simply
refuses to compile an otherwise correct program.  The compiler will
sometimes suggest a workaround, but in most cases it is not easy to
understand what went wrong.  In this post I will explain why we find
it acceptable to distribute a compiler that will refuse to compile
valid code, how we hope to eventually fix the situation, and provide
hints on writing your code to avoid the compiler restrictions in the
first place.

First, let me clarify that we *do* actually have a compiler that will
correctly compile any Futhark program (modulo compiler bugs):
``futhark-c``, which compiles Futhark to sequential C code.
Similarly, we also have a complete interpreter, ``futharki``.
Fundamentally, Futhark is a quite simple language - much simpler than
its cousins SML, OCaml, and Haskell - and it is quite straightforward
to compile it to any reasonably sensible target, like for example a
modern CPU.  Unfortunately, the target being "reasonably sensible" is
*not* a valid assumption for ``futhark-opencl``, which wants to
generate efficient code for GPUs.  Operations that we take for
granted, such as "allocate some memory" or "terminate the program with
an error message" are not quite as straightforward in high performance
GPU code.  To explain why, we will have to look at the basics of the
GPU execution model.

In the following, when I write that something is *impossible*, that
might not literally be the case.  Really, I mean that it is
*inefficient in general*, which makes it unsuitable for our purposes.
There are lots of convenient, flexible and powerful programming
languages out there, while Futhark's `only selling point is
performance <2016-09-03-language-design.html>`_.  Hence, it makes no
sense for us to make implementation choices that might result in
correct execution, but would make the code run slowly.

The GPU Execution Model
-----------------------

A typical GPU functions as a co-processor that carries out orders sent
to it from a controlling CPU.  Execution takes place when the CPU
enqueues a GPU function (confusingly called a *kernel*, but completely
unrelated to operating systems) along with how many threads to use.
Each thread executes the same sequential code (the body of the kernel
function), but each has a distinct *thread ID*, which can be used to
operate on different data or take different control flow decisions.
Kernels are *transient*: they run only long enough to complete their
work, then stop, at which point a new kernel can start.

Interaction with the GPU is done through an API that we call from
ordinary CPU code, and which then talks to the GPU driver, in practice
CUDA for NVIDIA GPUs and OpenCL for the Intel and AMD GPUs.  These are
mostly equivalent for our purposes, and the programming models are
almost identical.

Now let us look at the two most common compiler limitations
encountered by Futhark programmers.

"Cannot allocate memory block in kernel"
----------------------------------------

A kernel can write only to memory physically residing to the GPU,
which is distinct from CPU memory.  We have to use the API to allocate
enough memory on the GPU to fit both our input and output, and
manually copy data between GPU and CPU as necessary.  In CUDA, this is
done with ``cudaMalloc()`` and in OpenCL with ``clCreateBuffer()``.
This memory is not directly accessible to CPU code, but must be
written or read using special API functions (in OpenCL these are for
example ``clEnqueueReadBuffer()`` and ``clEnqueueWriteBuffer()``).
Most significantly, we cannot call these memory allocation functions
from within kernel code.  This means that all memory a kernel needs
must be allocated in advance before it starts, which requires that it
can be computed in advance.

Consider the following Futhark expression::

  map (\x -> let ys = replicate 10 x
             in ...)
      xs

For each element ``x`` of the array ``xs``, we apply an anonymous
function that creates an array of ``10`` copies of ``x``, then does
some other work that we don't worry more about, and ultimately returns
a value.  The result is an array of those values that is as big as the
``xs`` array.  In general, a ``map`` is executed by launching a kernel
with one thread per element of the array (things are more interesting
when the ``map`` itself contains more parallelism, but let us leave
that for another post).

Each iteration the ``map`` produces an array (``ys``).  The compiler
has to find space for that array in memory.  Since the size of the
array is the same for each iteration of the array, it can precompute
how much memory is needed in total by multiplying with the number of
threads.  Let us assume each ``x`` requires 4 bytes::

  let mem = alloc (10 * length xs * 4) in
  map (\x -> -- y@m+tid*(length xs)*4
             let ys = replicate 10 x
             in ...)
      xs

The compiler annotates the variable ``ys`` with the information that
it will be located in the memory block ``mem``, at an offset derived
by the thread ID.  (In practice, this is not how the compiler will lay
out the memory.  Instead, ``ys`` arrays for different threads will be
interleaved in memory to ensure `coalesced memory accesses
<https://cs.stackexchange.com/questions/18229/what-is-memory-coalescing>`_.)
Note that we do no need to know the exact size of ``ys`` at
compile-time; it is enough that we have a symbolic value that is
invariant to the values of the array ``xs``.

Many programs have different array *values* for each iteration of a
``map``, but the same array *sizes*, so this works quite well in
practice.  However, consider this program::

  map (\x -> let ys = replicate (1+x) 0
             in ...)
      xs

Each thread creates an array of ``1+x`` elements, but each thread may
have a different value for ``x``!  This means there is no closed form
expression we can use to determine the memory requirements.  When the
Futhark compiler cannot predict the memory requirements of a kernel,
it will refuse to compile the program and crash with an error message.

For this specific example, though, that is not what happens.  The
Futhark compiler has a limited ability to estimate memory usage by
extracting a *size-slice* of the ``map`` function that computes the
*sizes* of any intermediate arrays used.  In the above case, this is
just the value ``x``.  This is then used to determine, for each
intermediate array, its *maximum size* for any thread, and this is
then used to allocate a block of memory that is guaranteed to be large
enough::

  let x_bound = i32.maximum (map (\x -> 1+x) xs)
  let mem = alloc (10 * x_bound * 4) in
  map (\x -> -- y@m+tid*x_bound*4
             let ys = replicate (1+x) 0
             in ...)
      xs

Of course, the risk here is that we end up grossly over-allocating in
the case that one ``x`` is much larger than the others.  Another
approach would use a ``scan`` to compute precisely how much memory is
needed for each thread.  The problem with that approach is that it
makes it harder to interleave the per-thread chunks in memory (because
they do not have the same size), which is in practice needed for good
performance.  It's a difficult trade-off, and this part of the compiler
is still quite experimental.  Another problem is that it may not
always be easy or possible to extract a size-slice, in particular if
the ``map`` contains a sequential loop that contains intermediate
arrays of loop-variant sizes.

The best way to avoid this limitation is to avoid variantly-sized
arrays inside parallel operations.  This also involves avoiding arrays
that you *know* will always have an invariant size, but where the
compiler cannot statically figure it out (this occurs most often when
using concatenation unnecessarily).  For example, consider this program::

  let main (n: i32): [][]i32 =
    map (\i -> let a = 0..<i
               let b = 0..<n-i
               in a ++ b)
         (0..<n)

The expression ``0..<i`` constructs an array of the elements from
``0`` to ``i`` (not exclusive).  This specific program will actually
compile due to the size slicing trick mentioned above, but in its
absence we could rewrite it as follows, to make the sizes completely
explicit to the compiler::

  let main(n: i32): [][]i32 =
    let scratch = 0..<n
    in map (\i ->
              let res = 0..<n
              let res[i:n] = scratch[0:n-i]
              in res)
           (0..<n)

This exploits the fact that the compiler does not generate allocations
for array slices or in-place updates. The only allocation is of the
initial ``scratch``, the size of which can be computed before entering
the ``map``.

"Cannot compile assertion inside parallel kernel"
-------------------------------------------------

The allocation issue is rooted deeply in performance concerns
(allocation *is* an expensive operation), reflected in the GPU hardware
architecture, awkward to work around, and require both trickery and
subtlety in the compiler to work around.  The other issue I will talk
about has none of these properties.  It is only mildly related to
performance, unlikely to be rooted in essential architectural
concerns, trivial to work around, and could be easily implemented in
the compiler if GPUs permitted it.  Specifically, the problem concerns
programs like this::

  let main (xs: []i32) (is: []i32) =
    map (\i -> xs[i]) is

This program contains an array index operation ``xs[i]``.  Since
Futhark is a safe high-level language, we insert a check to ensure
that the index ``i`` refers to a valid position in the array ``xs``.
And of course, we want to execute the ``map`` on the GPU as a kernel.
However, if you try to compile the above program with
``futhark-opencl``, the compiler will crash during final code
generation and complain that it cannot *"compile assertion inside
parallel kernel"*.  This means that the compiler found a bounds check
inside kernel code that it cannot resolve statically.

So, why does the compiler refuse to do bounds checking on the GPU?
The problem is not the check itself: it's just two integer comparisons
and a branch.  The problem is how to handle the case of a *failed*
bounds check, since neither CUDA nor OpenCL provide straightforward
ways for a thread to terminate the entire kernel.  A *single thread*
can terminate itself (just ``return`` from the kernel function), but
this can result in deadlocks if the kernel is doing complicated
synchronisation.  An infinite loop is not an appropriate way to report
the presence of an out-of-bounds access.

CUDA does provide an ``assert()`` mechanism, but `according to the
documentation
<https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#assertion>`_
it also poisons the entire CUDA context, including all memory objects,
which makes it impossible to retrieve diagnostic information about
what went wrong, such that it can be reported in a structured way.  I
am fine with abnormal kernel termination having serious consequences
(or being slow, since bounds checks are hopefully rare), but this is
too much.

This issue is particularly frustrating since I cannot think of a
reason why the hardware should not be able to support sudden kernel
termination.  CUDA's ``assert()`` proves that it does actually support
it, and AMD and Intel GPUs are certainly able to terminate kernels
that try to access unaligned memory.

Fortunately, there is a simple workaround, that the compiler even
tells you about when this limitation is encountered: wrap the
problematic access in ``unsafe``::

  let main (xs: []i32) (is: []i32) =
    map (\i -> unsafe xs[i]) is

This simply makes the compiler not generate any dynamic checks in the
enclosed expression.  This includes not just bounds checks, but also
size checks for e.g. ``unflatten`` and ``zip``.  Now, three pieces of
advice when doing this:

  * **``unsafe`` means unsafe**!  The index will *not* be checked.  Make
    sure you understand what this means.  If you use ``unsafe`` in
    conjunction with in-place updates, then you may perform arbitrary
    memory corruption.  In the worst case, this can result in
    exploitable bugs!  Only use ``unsafe`` as a last resort, and when
    you are really sure that the index will be valid.

  * **Scope your ``unsafe`` as narrowly as possible**.  Since
    ``unsafe`` can easily hide real bugs, it is a really bad idea to
    e.g. put an ``unsafe`` at the beginning of your ``main`` function
    to completely disable all dynamic checks.

  * **Test your program with ``futhark-c --safe``**.  Again,
    ``futhark-c`` can handle checks in any location.  The ``--safe``
    option makes it disregard any ``unsafe``s in the program.

Some years ago we `wrote a paper
<https://futhark-lang.org/docs.html#bounds-checking-an-instance-of-hybrid-analysis>`_
on a complex mechanism for slicing bounds checks (similar to what we
do for array sizes).  In practice it was too complex to implement to
be practical, and still required dynamic checks as a fallback in
pathological cases.  The ease of implementing the ``unsafe``
workaround won out.  I still want to come up with a good solution for
this, but ideally I just want the GPUs to support it properly.

The Universal Solution
----------------------

So why do these limitations even exist?  It has been over 25 years
since `Guy Blelloch showed how to handle arbitrary nested parallelism
via the flattening algorithm
<https://www.cs.cmu.edu/~guyb/papers/Ble90.pdf>`_, and in particular
flattening also produces code of a simple flat form that removes any
irregular allocations, and makes it easy to performs bound checks.
Further, the current Futhark compilation model only handles *regular
parallelism*, while Blelloch's flattening can handle even irregular
parallelism.  Why are we making life hard for ourselves and our users?

The main reason is that while flattening preserves the `asymptotic
work/span cost
<https://en.wikipedia.org/wiki/Analysis_of_parallel_algorithms>`_ of
the parallelism, it will in *practice* often generate very slow code
because it often results in polynomial increases in space usage due to
over-parallelisation, and removes information necessary for crucial
optimisations like `loop tiling
<https://en.wikipedia.org/wiki/Loop_nest_optimization>`_.  The entire
hypothesis behind Futhark is that we can use a somewhat less general
model to achieve better performance in practice.  Indeed, for fully
regular programs, all allocations can be predicted in advance, so the
problems only arise due to your attempt to move outside of Futhark's
core sweet spot.  Still, we are steadily working towards being able to
promise that any Futhark program that passes the type checker can be
compiled to reasonably efficient parallel code.  A core technique to
bring us nearer is multi-versioned code, which is the subject of our
upcoming `PPOPP'19 <https://ppopp19.sigplan.org/>`_ paper.
