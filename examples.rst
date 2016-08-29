---
title: Code Examples
---

Futhark still lacks many of the syntactical niceties that one might
desire in a programming language.  Therefore, the examples here are
more verbose than ideal.  Still, they should provide a taste of what
(simple) Futhark programs look like.  If you have ideas for syntax
improvements, you can always `contribute`_!

As Futhark is a functional language, we will start with the obligatory
factorial program::

  fun fact(n: int): int = reduce (*) 1 (map (1+) (iota n))

  fun main(n: int): int = fact n

The function call ``fact n`` creates an array of the integers
``0..n-1``, adds one to each element of the array, then computes the
product of all elements in the array.  The Futhark compiler employs
*loop fusion* to remove the need for any of these temporary arrays to
be actually created.  Technically ``fact n`` does not compute ``n!``,
but rather ``n!  mod 2**32``, as ``int``s are 32 bit in size and will
rapidly overflow for large ``n``.

If we put the above program in a file ``fact.fut``, we can compile it
using the OpenCL backend as such::

  $ futhark-opencl fact.fut

If all goes well, this produces an executable program ``fact`` in the
current directory.  Similarly, we can compile to sequential C code
with ``futhark-c``.  As mentioned in the `introduction`_, Futhark is
not intended to be used for writing standalone programs, but it is
supported in order to enable testing and benchmarking.  A standalone
program will expect to be given its arguments on standard input::

  $ echo 2000000000 | ./fact

This may produce a bunch of diagnostics on standard error, but will
also write the result on standard output (``0i32`` - a 32-bit zero).
We can use the built-in instrumentation to determine how long the
computation took, not counting GPU context initialisation and the
like::

  $ echo 2000000000 | ./fact -t runtime.txt

The file ``runtime.txt`` will contain the wall time in microseconds.
On a GTX 780 Ti GPU, ``fact(2000000000)`` (two billion!) runs in 7.0ms
.  A `sequential C program`_ using a ``for``-loop to compute the same
thing takes 1335.3ms on an Intel Xeon E5-2650 CPU.  Of course, this is
not a realistic performance comparison, as neither program accesses
memory, but it shows how easy it is to obtain parallel execution in
Futhark.  If we ask the Futhark compiler to generate sequential C code
(with ``futhark-c``), the resulting program runs in exactly the same
time as the hand-written C program.

A More Complex Example
----------------------

A more interesting example is the *maximum segment sum problem*
(*MSS*), where we wish to determine the maximum sum of any contiguous
subsequence of an array of integers.  We can implement this in Futhark
using a combination of ``map`` and ``reduce``::

  fun max (x: int) (y: int): int =
    if x > y then x else y

  fun redOp (x: (int,int,int,int))
            (y: (int,int,int,int)): (int,int,int,int) =
    let (mssx, misx, mcsx, tsx) = x
    let (mssy, misy, mcsy, tsy) = y
    in ( max mssx (max mssy (mcsx + misy))
       , max misx (tsx+misy)
       , max mcsy (mcsx+tsy)
       , tsx + tsy )

  fun mapOp(x: int): (int,int,int,int) =
    ( max(x,0), max(x,0), max(x,0), x )

  fun main(xs: []int): int =
    let (x, _, _, _) =
      reduce redOp (0,0,0,0) (map mapOp xs)
    in x

One interesting aspect about this program is that it involves a
reduction with an operator that is associative_, but not commutative_.
Associativity is a requirement for the parallel execution of
reductions, but commutativity is not required.  Yet, for reasons of
implementation difficulty, many parallel languages and libraries will
malfunction if the reduction operator is not commutative.  Futhark
supports non-commutative operators, as we have found that many
interesting problems (such as *MSS* above) cannot be solved
efficiently with just commutative reductions.

On a GTX 780 Ti GPU, Futhark can compute the MSS of ten million
integers in 1.2ms.  Much of the runtime is spent transposing the input
array in order to ensure optimal memory access patterns during the
actual reduction, which is necessary when compiling non-commutative
reductions.  The performance benefit compared to just using a scan
(which never assumes commutative operators) is illustrated
`elsewhere`_.

For more examples, you can look at our general benchmarks_ repository
on Github.

Gaussian Blur Stencil
---------------------

One common pattern of array computation is the so-called stencil_,
where we change the value of an element in the array based on its
neighbours.  For example, we might implement image blurring by
assigning each pixel the average value of all of its neighbors.
Futhark does not have a special-purpose stencil language construct.
Instead, stencil computations are expressed as ``map``s on the index
space, using explicit array indexing to access the stencil source
array and returning the new value for the index.  While this is rather
verbose, at least until Futhark grows more syntactical conveniences,
it works and performs well.  Let's look at how to implement a simple
image blurring program.

We will represent an image as a three-dimensional array
``[rows][cols][3]u8``.  The innermost size-3 dimension encodes the
three colour channels for red, green, and blue, respectively.  When
blurring, it is useful to operate on each colour channel separately.
Furthermore, instead of the colour being a number from 0 to 255, it is
more convenient to store it as a floating-point number between 0 and
1.0.  Therefore, we define a function that transforms an array of type
``[rows][cols][3]u8`` into three arrays of type
``[rows][cols]f32`` each.  The result is that we have one array for
each of the three colour channels::

  fun splitIntoChannels(image: [rows][cols][3]u8): ([rows][cols]f32,
                                                    [rows][cols]f32,
                                                    [rows][cols]f32) =
    unzip(map(fn (row) =>
                map(fn (pixel) =>
                      (f32(pixel[0]) / 255f32,
                       f32(pixel[1]) / 255f32,
                       f32(pixel[2]) / 255f32))
                    row)
              image)

The function ``splitIntoChannels`` maps across each inner ``[3]u8``
element (``pixel``), turns this into a triple instead of a
three-element array, then uses ``unzip`` to turn the resulting
array-of-triples into a triple-of-arrays, which is then returned.  For
readability, we could have chosen to explicitly indicate the return
and parameter types of the anonymous function, but in the interest of
brevity we have left them for the compiler to infer.  It is only
required to explicitly indicate the types of all top-level functions.

We will also need to re-combine the colour channel arrays into a
single array.  That function looks like this::

  fun combineChannels(rs: [rows][cols]f32,
                      gs: [rows][cols]f32,
                      bs: [rows][cols]f32): [rows][cols][3]u8 =
    zipWith(fn (rs_row, gs_row, bs_row) =>
              zipWith(fn (r,g,b)  =>
                        [u8(r * 255f32),
                         u8(g * 255f32),
                         u8(b * 255f32)])
                      rs_row gs_row bs_row)
            rs gs bs

Another thing we will need is the actual stencil function.  That is,
the function we wish to apply to every pixel in the image.  For
blurring, we will take the average value of the pixel itself plus each
of its eight neighbors (nine values in total)::

  fun newValue(image: [rows][cols]f32, row: int, col: int): f32 =
    unsafe
    let sum =
      image[row-1,col-1] + image[row-1,col] + image[row-1,col+1] +
      image[row,  col-1] + image[row,  col] + image[row,  col+1] +
      image[row+1,col-1] + image[row+1,col] + image[row+1,col+1]
    in sum / 9f32

The function call ``newValue(image, row, col)`` computes the new value
for the pixel at position ``(row, col)`` in ``image``.

The alert reader will have noticed that ``newValue`` cannot be applied
to pixels on the edge of the image - doing so would result in
out-of-bounds accesses to the ``image`` array.  We will take care to
only call the ``newValue`` function with safe indices, but the Futhark
compiler is sadly not yet smart to realise this - thus we are forced
to use the ``unsafe`` keyword to prevent the insertion of bounds
checks that would otherwise hinder parallelisation.  If we did not use
``unsafe``, the Futhark compiler would fail with an error message
pointing at the problematic array access.

Now we can write the actual stencil function, which applies
``newValue`` to every inner element of a colour channel array.  The
edges are left unchanged::

  fun blurChannel(channel: [rows][cols]f32): [rows][cols]f32 =
    map(fn (row) =>
          map(fn (col) =>
                if row > 0 && row < rows-1 && col > 0 && col < cols-1
                then newValue(channel, row, col)
                else channel[row,col])
              (iota cols))
        (iota rows)

You may have heard that branches are expensive on a GPU.  While this
is a good basic rule of thumb, what is actually expensive is *branch
divergence* - that is, when neighboring threads take *different* paths
through a branch.  In our stencil, only the edge elements will take
the false branch, and these are few in number compared to the
interior.

Stencil computations usually have an outer (sequential) loop for
applying the stencil several times.  Our program is no different - we
will apply the blurring transformation a user-defined number of times.
The more iterations we run, the more blurred the image will become::

  fun main(iterations: int, image: [rows][cols][3]u8): [rows][cols][3]u8 =
    let (rs, gs, bs) = splitIntoChannels(image)
    loop ((rs, gs, bs)) = for i < iterations do
      let rs = blurChannel(rs)
      let gs = blurChannel(gs)
      let bs = blurChannel(bs)
      in (rs, gs, bs)
    in combineChannels(rs, gs, bs)

Our ``main`` function is quite simple.  We split the input image into
three different channels, use a sequential loop to blur each colour
channel the requested number of times, then recombine the resulting
channel arrays into a single final image.

The Futhark ``loop`` construct merits an explanation: in the above
function, we declare three *loop variant variables*, ``rs``, ``gs``,
and ``bs``.  These take their initial values from the incidentally
identically named variables in scope (but this is not in general
requirement).  The *loop body* then returns three values that become
the values of the loop variant variables in the next iteration of the
loop.  In essence, the ``loop`` construct is just syntactical suger
for a particularly simple (but common) pattern of tail-recursive
function.  However, the Futhark compiler is able to perform
transformations involving ``loop``s that it cannot for recursive
functions (although it does not perform any such for this simple
program).

The three separate calls to ``blurChannel`` may seem wasteful, but the
Futhark compiler is smart enough to fuse them together into a single
GPU kernel that traverses the three colour channel arrays
simultaneously.  This is an instance of *horisontal fusion*.

Our Futhark program is now done.  The full commented source code is
located here: `blur.fut </static/blur.fut>`_.  We can make it a little
more useful by writing a small Python wrapper program for reading and
writing PNGs: `blur-png.py </static/blur-png.py>`_.  We must compile
``blur.fut`` using the PyOpenCL backend::

  $ futhark-pyopencl --library blur.fut

This produces a Python module ``blur.py`` which is then imported by
``blur-png.py``.  We can try it out on any PNG image, say, this
`illustration of the spirit of Futhark <images/gottagofast.png>`_::

  $ python blur-png.py gottagofast.png --output-file gottagofast-blurred.png

Which produces `this slightly smushed image
<images/gottagofast-blurred.png>`_.  We can also ask for a hundred
iterations::

  $ python blur-png.py gottagofast.png --output-file gottagofast-blurred.png --iterations 100

Which produces `this blurry mess
<images/gottagofast-veryblurred.png>`_.  Notice the edges - perhaps
simply keeping them unchanged is not the best way to implement image
blurring.  Still, this program is a decent description of how to
implement stencils in Futhark.  For performance measurements on a
slightly more complicated stencil, see `HotSpot on the performance
page`_.

.. _`contribute`: /getinvolved.html
.. _`introduction`: /
.. _`sequential C program`: /static/sequential-fact.c
.. _associative: https://en.wikipedia.org/wiki/Associative_property
.. _commutative: https://en.wikipedia.org/wiki/Commutative_property
.. _elsewhere: /performance.html#mss-futhark-thrust
.. _benchmarks: https://github.com/HIPERFIT/futhark-benchmarks
.. _stencil: https://en.wikipedia.org/wiki/Stencil_code
.. _`HotSpot on the performance page`: /performance.html#hotspot-futhark-rodinia
