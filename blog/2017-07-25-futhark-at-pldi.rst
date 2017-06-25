---
title: Streaming Combinators and Extracting Flat Parallelism
author: Troels Henriksen
description: The presentation given at PLDI 2017 turned into a blog post.  Basic introduction to Futhark, including novel parallel combinators used for efficient sequentialisation, as well as Futhark's approach to handling nested parallelism.
---

This Tuesday I presented Futhark at `PLDI`_, a large academic
conference on programming language design and implementation.  The
`associated paper`_ is available (`paper PDF`_), but makes for
somewhat dry reading.  This blog post covers the main points of the
presentation, demonstrated by examples, in a way that may be more
accessible than the paper.  This is a bit of a long post, but
logically divided into two parts.  The first half introduces the
basics of the Futhark language and shows its application to a small
problem.  The second half contains a more complicated example of how
nested parallelism is turned into GPU-friendly flat parallelism.
There is also a short conclusion at the end with measurements of how
Futhark-generated code compares in performance to hand-written GPU
programs.  Scroll to the end if you enjoy looking at numbers and
graphs.

.. _`PLDI`: http://conf.researchr.org/home/pldi-2017
.. _`associated paper`_: http://dl.acm.org/citation.cfm?doid=3062341.3062354
.. _`paper PDF`: publications/pldi17.pdf

Problem Statement
-----------------

It is old news that parallel computers have become mainstream.  It is
also widely acknowledged that we still lack good models and languages
for programming them.  The problem becomes even more severe when we
leave the relatively benign world of multi-core CPUs and begin to
consider such devices as GPUs, which have evolved into massively
parallel machines for general-purpose computation.  Saturating a GPU
typically requires tens of thousands of lightweight threads with
minimal communication or shared state.  Traditional sequential and
imperative languages are a poor fit.

Functional languages are often mentioned as a solution to the
parallelism problem, as they do not rely as much on evaluation order
or side effects.  For example, a ``map`` function can process each
element in parallel, and a ``reduce`` can shrink an array to one
element through repeated parallel application of a function to pairs
of elements in the input array.

Unfortunately, life is not that easy.  While the functional style
indeed makes parallelism explicit, safe and correct, it is not
automatically a good fit for a GPU.  One major problem is that GPUs
support only a restricted form of parallelism (efficiently, at least).
The parallelism must be *flat*, meaning that one parallel thread may
not spawn more threads.  Further, code running on the GPU may not
allocate memory - all allocation must be done prior to starting GPU
execution, by controller code running on the CPU.  There are further
restrictions on how memory must be accessed to obtain high efficiency,
and a myriad of other hindrances such as too little stack to implement
recursion, and the absence of function pointers.  By itself,
functional programming does not address these problems, and existing
functional languages like `OCaml`_ or `Haskell`_ are too large and
expressive to ever run well on a device as restricted as a GPU.

.. _`OCaml`: http://ocaml.org/
.. _`Haskell`: https://haskell-lang.org/

But all is not lost: the right solution, we believe, is to carefully
design a small functional language that has just the features and
restrictions to enable the construction of an aggressively optimising
compiler capable of generating GPU code.  This compiler will worry
about all the low-level details, leaving the programmer solely
concerned with how best to express the parallelism in their algorithm.
We have spent the last four years constructing such a language and
compiler, and we call it *Futhark*.

The Language
------------

Futhark is a small data-parallel purely functional array language that
resembles a cross between Haskell and OCaml.  Parallelism is expressed
through built-in *second-order array combinators* (SOACs) that
resemble higher-order functions known from functional programming.
For example, we can define a function that takes a one-dimensional
array of integers of input and increments each by one::

  let add_two [n] (a: [n]i32): [n]i32 = map (+2) a

Note the shape declarations: we indicate that there is some size
``n``, and that both the result and input to the function are
one-dimensional arrays of size ``n``.  We do not have to pass the size
``n`` explicitly when we call the function; rather it is automatically
inferred when we pass the array argument for ``a``.

As another example, this function sums an integer array::

  let sum [n] (a: [n]i32): i32 = reduce (+) 0 a

The ``reduce`` construct takes a function that must be `associative`_,
and possess an `identity element`_ (0 is the identity for addition).

.. _`associative`: https://en.wikipedia.org/wiki/Associative_property
.. _`identity element`: https://en.wikipedia.org/wiki/Identity_element

We can write a function that uses ``sum`` to sum the rows of a
two-dimensional array::

  let sum_rows (as: [n][m]i32): [n]i32 = map sum as

The ``sum_rows`` function makes use of *nested parallelism*, as a
``reduce`` (via ``sum``) is used inside of a ``map``.  Mapping nested
parallelism to efficient flat parallelism is one of the most important
tasks performed by the Futhark compiler, and the last part of this
post concerns itself with how that is done.

A few more constructs must be introduced before we can look at real
Futhark code.  First, we can use ``iota`` and ``replicate`` to create
new arrays::

  iota 5           ⇒        [0,1,2,3,4]
  replicate 3 1337 ⇒ [1337, 1337, 1337]

Futhark has an important restriction regarding arrays: only *regular
arrays* are permitted.  An array is regular if all its elements have
the same shape.  Thus, ``[[1,2],[3,4]]`` is regular, but ``[[1,2],
[3]]`` is not.

Second, Futhark provides special syntax for expressing sequential
loops::

  loop (x = 1) for i < n do
    x * (i + 1)

The meaning of the above expression is to first assign ``x`` the value
1, then execute the loop body ``n`` times, each time binding ``x`` to
the result, and eventually return the final value of ``x`` as the
result of the entire loop.  There is nothing magical about
``for``-loops.  They are equal in power to the tail-recursive
functions we are used to from other functional languages, but they
make life a little easier for the compiler, as we shall see.

Case Study: K-Means Clustering
------------------------------

To introduce some of Futhark's more novel constructs, as well as the
transformations performed by the compiler, let us take a look at
*k*-means clustering.  The problem is to assign a collection of *n*
points in some *d*-dimensional space to *k* clusters, such that the
distance from a point to the centre of its cluster is minimised.

`According to Wikipedia`_, the overall algorithm is as follows (images
by I, Weston.pace, CC BY-SA 3.0):

.. _`According to Wikipedia`: https://en.wikipedia.org/wiki/K-means_clustering

(1) *k* initial cluster means (here *k=3*) are randomly generated
    within the data domain:

    .. image:: /images/K_Means_Example_Step_1.svg
       :alt: The first step of K-means.
       :class: centre

(2) *k* clusters are created by associating every observation with the
    nearest cluster mean:

   .. image:: /images/K_Means_Example_Step_2.svg
      :alt: The second step of K-means.
      :class: centre

(3) For each of the *k* clusters, compute the average of all points in
    the cluster, which then becomes the new mean for the cluster:

   .. image:: /images/K_Means_Example_Step_3.svg
      :alt: The third step of K-means.
      :class: centre

(4) Steps (2) and (3) are repeated until convergence is reached:

   .. image:: /images/K_Means_Example_Step_4.svg
      :alt: The fourth step of K-means.
      :class: centre

For this post, we shall focus on the main computation of the third
step.  Each point has been associated with a cluster (given by an
integer), and we already know the number of points in each cluster.
The problem is thus: given *n* points and assignments of each point to
one of *k* clusters, compute the mean of each cluster.  We can do this
sequentially by keeping a running tally of cluster means in the form
of a *k×d* array, traversing the *n* input points, and updating the
tally at the appropriate index.  We begin by defining a function for
adding two points::

  let add_points(x: [d]f32) (y: [d]f32): [d]f32 =
    map (+) x y

This is just vector addition.  There is one quirk: in Futhark, the
``map`` construct can operate on any number of array inputs, somewhat
resembling the ``zipWith`` of Haskell.  We can now sequentially
compute cluster means like this::

  let cluster_means_seq (cluster_sizes: [k]i32)
                        (points: [n][d]f32)
                        (membership: [n]i32): [k][d]f32 =
    loop (acc = replicate k (replicate d 0.0)) for i < n do
      let p = points[i]
      let c = membership[i]
      let c_size = f32 cluster_sizes[c]
      let p' = map (/c_size) p
      in acc with [c] <- add_points acc[c] p'

The most interesting part here is the update of the ``acc`` array,
which is done with an *in-place update*.  Semantically, the construct
``a with [i] <- b`` produces an array with the same elements as ``a``,
except with the value ``b`` at index ``i``.  In most purely functional
languages, this would require a copy of the array ``a``, but Futhark
uses a mechanism based on uniqueness types to permit the update to be
in-place.  Essentially, the type checker verifies that the "source"
array ``a`` is never used on any execution path following the in-place
update.  This permits reuse of the memory where ``a`` is stored,
without violating referential transparency.  Thus, Futhark permits
some (simple) sequential code to be expressed efficiently.

The ``cluster_means_seq`` function performs *O(n×d)* work, but has
little parallelism.  Let's try doing a better job.  The idea is to map
each point to a partial accumulator - a *k×d* array that is all zero
except at the position corresponding to the cluster of the point.  We
then perform a reduction of all the partial accumulators, using matrix
addition as the operator::

  let matrix_add (xss: [k][d]f32) (yss: [k][d]f32): [k][d]f32 =
    map (\xs ys -> map (+) xs ys) xss yss

  let cluster_means_par(cluster_sizes: [k]i32)
                       (points: [n][d]f32)
                       (membership: [n]i32): [k][d]f32 =
    let increments : [n][k][d]i32 =
      map (\p c ->
             let a = replicate k (replicate d 0.0)
             let c_size = f32 cluster_sizes[c]
             let p' = map (/c_size) p
             in a with [c] <- p')
          points membership
    in reduce matrix_add (replicate k (replicate d 0.0)) increments

(In Futhark, an anonymous function is written as ``(\x -> ...)``, just
as in Haskell.)

This version is fully parallel, which is great, but not *work
efficient*, as it requires *O(k×n×d)*, compared to the *O(n×d)* of the
sequential version.  The core issue is that ``cluster_means_par`` is
*too parallel*.  Real machines are not infinitely parallel, but have
some maximum parallel capacity beyond which adding more threads simply
means more overhead, without obtaining better hardware utilisation.
Ideally, each thread sequentially processes some chunk of the input,
followed by a parallel combination of the per-thread partial results.
Thus, we pay only for the parallelism that we can profitably use:

.. image:: /images/chunking.svg
   :alt: A visualisation of how each thread processes a sequential
         chunk of the input, followed by each per-thread result being
         combined into a single result.
   :width: 80%
   :class: centre

However, the ideal number of threads depends on the concrete hardware
(and other runtime factors), and therefore should not be baked into
the program by the programmer.  The solution is to provide a language
construct that encapsulates both efficient sequential execution, as
well as how to combine per-thread results.  In Futhark, this
constructed is called ``stream_red`` for *stream reduction*, and is
used like this::

  let cluster_means_stream(cluster_sizes: [k]i32)
                          (points: [n][d]f32)
                          (membership: [n]i32): [k][d]f32 =
    let on_chunk [chunk_size]
                 (points':     [chunk_size][d]f32)
                 (membership': [chunk_size]i32) =
          cluster_means_seq cluster_sizes points' membership'
    in stream_red
         matrix_add on_chunk
         points membership

The combination function (which must be associative, as with
reduction) is matrix addition.  The local function ``on_chunk`` is
called to sequentially process each chunk within a thread, and itself
merely calls the ``cluster_means_seq`` function we defined above.  The
``chunk_size`` may vary freely between threads, and is not known until
runtime.

One nice property about the ``stream_red`` construct is that, if
deemed necessary, the compiler can "recover" the fully parallel
implementation by using ``n`` threads with a chunk size of ``1``, or
the fully sequential implementation by setting the chunk size to
``n``.  In essence, ``stream_red`` provides a "dialable" amount of
parallelism.

During compilation, the compiler will break up the ``stream_red`` into
a per-thread part and an ordinary reduction::

  let per_thread_results : [num_threads][k][d]f32 =
    ...
  -- combine the per-thread results
  let cluster_means =
    reduce (map (map (+)))
           (replicate k (replicate d 0.0))
           per_thread_results

(The ``(map (map (+)))`` part is presently not valid Futhark syntax,
but is used here for simplicity - it's just the matrix addition from
above.)

I have left out the expression that computes ``per_thread_results``,
as it depends on the internal compiler representation.  The
``num_threads`` variable is some value computed at run-time based on
the hardware (and can be subject to tuning).

The reduction with ``(map (map (+)))`` is not great, as the
intermediate ``k×d`` matrices are too large to fit in the GPUs fast
on-chip memory (a kind of manually managed cache).  Thus, the Futhark
compiler will perform a transformation called *Interchange Reduce With
Inner Map* (IRWIM), which moves the reduction inwards at the cost of a
transposition::

  let per_thread_results' : [k][d][num_threads]f32 =
    rearrange (1,2,0) per_thread_results
  let cluster_means =
    map (map (reduce (+) 0.0)) per_thread_results'

The ``rearrange`` construct permutes the dimensions of an array, here
transposing the outermost dimension of ``per_thread_results``
innermost - see how the type changes from ``[num_threads][k][d]f32``
to ``[k][d][num_threads]f32``.

The only problem now is that the two ``map``-parallel dimensions are
of size ``k`` and ``d``, which is likely not enough to fully saturate
the GPU.  Fortunately, the compiler is smart enough to recognise that
a ``reduce`` inside of ``map``s corresponds to a pattern called a
*segmented reduction*, which has an efficient implementation on GPUs
(details in an upcoming paper!).

The implementation based on reduction streams is significantly faster
than the fully parallel one.  On an NVIDIA Tesla K40 GPU with ``k=5``,
``n=10,000,000``, ``d=3``, the function ``cluster_means_par`` executes
in 131.1ms, while ``cluster_means_stream`` executes in 17.6ms - a
speedup of 7.6×.

Improving Available Parallelism via Loop Distribution and Interchange
---------------------------------------------------------------------

Futhark as a language supports (regular) nested data parallelism, but
GPUs prefer flat parallelism.  A *GPU kernel* is lingo for a GPU
program, which we can think of as a perfect ``map`` nest containing
either sequential code, or specific known patterns of parallelism,
like ``reduce``.  Only such known patterns can be executed by the GPU.
The Futhark compiler therefore has the job of turning complicated
nestings of parallel constructs into perfectly nested ``map``s, each
corresponding to a single GPU kernel.  As an example, consider this
fragment of code::

  map (\xs -> let y = reduce (+) 0 xs
              in map (+y) xs)
      xss

We have an outermost ``map``, the body of which contains further
parallelism in the form of a ``reduce`` and another ``map``.  If we
wish, we can simply parallelise the outermost ``map`` and compile the
inner parallel operators to sequential code, thus producing one GPU
kernel.  This will limit the amount of parallelism we extract from the
program, but if the outer ``map`` operates on enough elements, then
that may well be the right choice.  Alternatively, we can *distribute*
each of the two inner SOACs to their own map nest::

  let ys = map (\xs -> reduce (+) 0 xs) xss
  in map (\xs y -> map (+y) xs) xss ys

Note how the intermediate result ``y`` has now been lift to an array
of intermediate results ``ys``, which is passed into the second map
nest.  This form corresponds to two GPU kernels, each providing more
parallelism than the single one from before.  Since a GPU typically
requires kernels to contain tens of thousands of threads in order to
fully utilise the hardware, this transformation is sometimes
necessary.

For functional languages, this problem was in principle solved in the
early 90s in `NESL`_ by Guy Blelloch.  NESL defines a *flattening
algorithm* (sometimes called *vectorisation*) that describes how to
turn arbitrary nested data-parallelism into flat data-parallel
operations.  The flattening algorithm is universal, in that it always
works (provided the language fulfils a few criteria, such as purity).
Unfortunately, full flattening has a few problems:

  1. Always maximises available parallelism, even when not worthwhile
     (e.g innermost loops in a matrix multiplication).

  2. Wasteful of memory (fully flattened matrix multiplication
     requires *O(n³)* space).

  3.  Destroys access pattern information, rendering
      locality-of-reference optimisations such as loop tiling hard or
      impossible.

.. _`NESL`: http://www.cs.cmu.edu/~scandal/nesl.html

Thus, the Futhark compiler takes a step back and uses a kernel
extraction algorithm based on *limited flattening*.  It's not as
universal as full flattening, but for those cases where you don't need
to fully maximise parallelism, it can generate substantially faster
code (and the Futhark compiler could always fall back to full
flattening if necessary).

The algorithm is based on the rich set of rewrite rules permitted by
functional languages.  For example, there is a well-known rule
describing how to compose two ``map``s into one:

.. class:: centre

map f ◦ map g ⇒ map (f ◦ g)

This rule is used in the Futhark compiler to perform `loop fusion`_,
but it can also be reversed to obtain *fission*:

.. _`loop fusion`: http://www.compileroptimizations.com/category/loop_fusion.htm

.. class:: centre

map (f ◦ g) ⇒ map f ◦ map g

This, along with other higher-order rules (details in the paper), are
applied by the compiler to extract perfect ``map`` nests.  When and
how to apply the rules is currently determined by heuristics in the
compiler.  As an example, let us consider the following contrived
program::

  let bss: [m][m]i32 =
    map (\(ps: [m]i32) (ps: [m]i32) ->
          loop (ws=ps) for i < n do
            map (\w -> w * 2) ws)
        pss

Let us assume that the array ``pss`` (the outermost input array) has
type ``[m][m]f32``, for some ``m``.  We could choose to simply
parallelise the outermost ``map`` as a single kernel with ``m``
threads.  Depending on the data set, this may be the best choice, but
in this case the compiler will try to improve the amount of exposed
parallelism.  Specifically, the compiler will *interchange* the outer
parallel ``map`` and the inner sequential ``loop``::

  let bss: [m][m]i32 =
    loop (wss=pss) for i < n do
      map (\ws ->
            map (\w -> w * 2) ws)
          wss

This interchange has made a perfect ``map`` nest (of size ``m``×``m``)
visible, which can be turned into a fully parallel GPU kernel.  This
kernel will be executed ``n`` times in total because of the
now-outermost sequential loop.  The question becomes: is executing an
``m``×``m`` kernel ``n`` times better than executing a size ``m``
kernel once, if each of those ``m`` threads run ``n`` iterations of a
sequential loop?  The answer depends on the exact values of ``n`` and
``m``.  If ``m`` is sufficiently large, then the GPU can be fully
utilised with just ``m`` threads, but otherwise, full utilisation
requires us to also exploit the innermost parallelism, even if it
comes at the overhead of launching more kernels.

In the future, we intend to have the Futhark compiler generate several
different *versions* of the program, based on different
parallelisation decisions, and choose the best one at run-time, based
on characteristics of the actual input data.  For now, hand-picked
heuristics are used.

Gotta Go Fast!
--------------

Futhark is based on the idea that a restricted language permits a more
powerful compiler.  However, we must be careful not to restrict the
language so much that it becomes useless for its intended purpose.
While Futhark is not designed for full application programming, it
should be able to efficiently represent a broad set of parallel
algorithms.  To demonstrate Futhark's power and flexibility, we have
ported various published benchmarks and examples from hand-written
OpenCL and CUDA GPU code to Futhark.  The graphs below demonstrate the
speedup of Futhark implementations of nine benchmarks ported from
`Rodinia`_, running on both an NVIDIA GTX 780 Ti GPU and an AMD W8100
GPU:

.. _`Rodinia`: http://www.cs.virginia.edu/~skadron/wiki/rodinia/index.php/Rodinia:Accelerating_Compute-Intensive_Applications_with_Accelerators

.. image:: /images/pldi-speedup0.svg
   :alt: Speedup graphs.
   :class: centre

.. image:: /images/pldi-speedup1.svg
   :alt: Speedup graphs.
   :class: centre

In all cases, Futhark performs acceptably.  The point of a language
such as Futhark is not to exceed the performance of highly optimised
code painstakingly tuned by experts - that's not really realistic -
but instead to provide easily accessible performance that is ideally
within a factor of two of the performance of hand-written code.  The
benchmark implementations, as well as several more, can be seen in our
`futhark-benchmarks`_ repository.

.. _`futhark-benchmarks`: https://github.com/HIPERFIT/futhark-benchmarks

Somewhat surprisingly, as we can see, code generated by the Futhark
compiler is often faster than hand-written code.  One reason is that
GPU performance is a sensitive thing, in particular when it comes to
memory access patterns.  The transformations needed to obtain optimal
performance are error-prone (and very tedious) to do by hand, but
feasible to automate in a compiler.  Another reason is that
parallelisation opportunities are sometimes missed by the human
programmer.  This is the case for the NN benchmark, where a reduction
was left sequential in the reference implementation, but parallelised
in Futhark - the result is that the Futhark program is 16.3× faster on
the GTX 780 Ti than the hand-written program.

In Conclusion
-------------

This post skips many details (read the paper!), but hopefully managed
to communicate two main points:

  1. A restricted/specialised language permits a clever compiler that
     saves the programmer from worrying about low-level details

  2. An idea of *what* such an approximation to a `sufficiently smart
     compiler`_ actually does.

.. _`sufficiently smart compiler`: http://wiki.c2.com/?SufficientlySmartCompiler

And here is a bonus third point:

  3. Futhark is a simple but fast language that you can try out
     yourself.  We have previously written about how to `inter-operate
     Futhark with Python`_, which can be used to create fancy
     interactive Futhark programs.

.. _`inter-operate Futhark with Python`: http://futhark-lang.org/blog/2016-04-25-futhark-and-pygame.html

If you are curious about seeing more Futhark code, we have more
`examples`_ to peruse.

.. _`examples` /examples.html

