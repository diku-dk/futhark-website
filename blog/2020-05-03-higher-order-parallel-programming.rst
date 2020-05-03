---
title: Higher-order parallel programming
author: Troels Henriksen
description: An explanation of the higher-order parallel programming model used by Futhark, and why we think it is superior to simpler first-order models.
---

Whenever I explain parallel functional programming, whether to
students or the barista at a coffee shop, one thing I must contend
with is people's prior experience with parallel programming.  Quite
often, these experiences are with low-level multi-threaded
programming, fraught with race conditions and other hazards.  Their
experience is that parallel programming is difficult and frustrating.
And who can blame them? Shared-state multi-threaded programming is
certainly one of the most difficult forms of programming I know of.
However, this style of programming is neither necessary nor sufficient
for parallelism.  Concurrent programming can be useful with just a
single processor, after all.

But still, such preconceptions present a barrier when I have to
explain the kind of parallel programming we support in a language such
as Futhark.  My strategy has become to point at `NumPy
<https://numpy.org/>`_, the Python array library, as an example of a
widely used parallel programming model; one that has shown that
high-level parallel programming can be just as accessible as
sequential programming.  In this post, I will elaborate on this theme,
and show the limitations of NumPy's *first-order* model, compared to
Futhark's *higher-order* model.

NumPy as a bulk-parallel programming model
------------------------------------------

NumPy is a Python library that makes available an `array type
<https://numpy.org/doc/stable/reference/generated/numpy.ndarray.html>`_,
along with various various functions and operators for manipulating
such arrays.  The key feature is that most operations are implicitly
lifted to operate on *entire* arrays, rather than single elements at a
time.  For example, if ``x`` is a NumPy array, then ``x+1`` produces
an array of the same size and type as ``x``, but with ``1`` added to
each element.  Similarly, if ``x`` and ``y`` are arrays, then ``x+y``
produces their pairwise sum, corresponding to vector addition if ``x``
and ``y`` are one-dimensional, matrix addition if they are
two-dimensional, and so on.  Good NumPy programming is based on these
*bulk operations* that operate on entire arrays, rather than on
individually manipulating elements.

The main advantage in NumPy is that these primitive operations are
implemented in efficient languages, such as C or Fortran, which will
run *much* faster than corresponding Python loops.  But the advantage
goes deeper than that: an array addition ``x+y`` is *potentially
parallel*.  Without knowledge about anything else going on in the
program, we know that we can safely execute this addition in a
parallel manner, for example by using one thread for each element in
the result.  Of course, literally launching one thread per element
would not be efficient, as a lone addition is far too little work to
amortise the cost of thread creation.  But note how we are already
discussing how to make the parallelism *efficient*, not whether it is
safe or correct to parallelise in the first place!

Now, despite this potential parallelism, stock NumPy does not to my
knowledge execute any of its operations in multiple threads.  However,
other implementations of the NumPy API, such as `Numba
<http://numba.pydata.org/>`_ or `CuPy
<https://github.com/cupy/cupy>`_, do!  Once we are working with a
parallel programming model to start with, actually exploiting parallel
execution becomes an engineering problem that is well within reach.

The thing that makes NumPy work as a parallel programming model is the
emphasis on bulk operations that operate on entire arrays.  As long as
we have a sequentially consistent view of data *between* these
operations, the NumPy implementation do whatever parallel tricks it
wishes *inside* them.  We get the best of both worlds: sequential,
fully deterministic, line-at-a-time semantics, but (potentially)
efficient parallel execution.  As human programmers, we have to
express our code in terms of bulk operations, but we *never* have to
worry about race conditions or nondeterminism.

Limitations of first-order parallel programming
-----------------------------------------------

For all its advantages, the NumPy model has weaknesses shared with all
similar array programming models (such as the venerable `APL
<https://en.wikipedia.org/wiki/APL_(programming_language)>`_).
Ultimately, in these models, all you have available as a programmer is
a large set of builtin array operations.  If you want your own code to
be efficient, in must be expressible in terms of these primitives.  In
some cases, they are insufficiently flexible.  In particular, it is
tricky to define bulk operations that require per-element control
flow.

As a somewhat contrived example, consider the problem of applying the
following Python function to every element of a NumPy array:

.. code-block:: Python

   def f(x):
     if 0 < x:
       return sqrt(x)
     else:
       return x

Now, while NumPy does provide a ``map`` function, using it is rarely a
good idea.  Since the function we apply to each element is an
arbitrary Python function, NumPy is no longer able to dispatch to some
highly-tuned native implementation.  Worse, by providing an arbitrary
function, parallelisation is no longer guaranteed to be safe, as that
function may modify global data!  To take advantage of the efficient
primitive operations, we end up having to encode the control flow as
data:

.. code-block:: Python

   def sqrt_when_pos_1(x):
       x = x.copy()
       x_nonneg = x >= 0
       x[x_nonneg] = np.sqrt(x[x_nonneg])
       return x

This is not great.  In particular, the original function ``f`` is
completely gone, so there is not any code re-use going on.  Further,
those indexings are rather complex to parallelise, so we probably want
an even nastier formulation (which runs almost twice as fast, even
sequentially):

.. code-block:: Python

   def sqrt_when_pos_2(x):
       x_nonneg = x >= 0
       x_neg = x < 0
       x_zero_when_neg = x * x_nonneg.astype(int)
       x_zero_when_nonneg = x * x_neg.astype(int)
       x_sqrt_or_zero = np.sqrt(x_zero_when_neg)
       return x_sqrt_or_zero + x_zero_when_nonneg

Nasty stuff.  NumPy has a few ad-hoc mechanisms for "masked
execution", but it doesn't change the fundamental fact that this is
probably not how we like to think about our algorithms.  Also, it gets
worse.  Let us consider the task of computing a `Mandelbrot fractal
<https://en.wikipedia.org/wiki/Mandelbrot_set>`_, which essentially
boils down to applying the following function to a bunch of
independent complex numbers:

.. code-block:: Python

   def divergence(c, d):
     i = 0
     z = c
     while i < d and np.dot(z) < 4.0:
       z = c + z * z
       i = i + 1
     return i

So how do we apply this function to every element of a NumPy array?
Handling ``if`` in the previous example was bad enough.  Handling a
``while`` loop is worse:

.. code-block:: Python

   def mandelbrot(c, d):
     output = np.zeros(c.shape)
     z = np.zeros(c.shape, np.complex32)
     for i in range(d):
         notdone =
           (z.real*z.real + z.imag*z.imag) < 4.0
         output[notdone] = i
         z[notdone] = z[notdone]**2 + c[notdone]
     return output

While this program is certainly expressed in terms of parallel bulk
operations, it does not spark joy.  The control flow is obscured, it
always runs for ``d`` iterations, and it causes a *lot* of memory
traffic, as the intermediate ``output`` and ``z`` arrays must be
manifested in memory.  Compare this to the original ``divergence``
function, which just involves a bunch of scalars that could in
principle be stored entirely in registers!

The problem is that NumPy is (practically) a *first-order* programming
model, in the sense that its operations are parameterised by values
(arrays and scalars), not functions.  Put simply, NumPy lacks an
efficient ``map``.

Futhark as a higher-order programming model
-------------------------------------------

I am now going to show how Futhark allows us to expose parallelism
with nested control flow in a natural way.  This is not intended as a
criticism of NumPy - higher-order parallel programming is a *very*
tricky thing to implement efficiently, and to a large extent it is
still an active research area, with implementations that are not as
robust as NumPy.  In a metaphorical sense, Futhark is balancing on a
knife's edge on promising more than the compiler can deliver.

But it does deliver here.  For the square root problem, we just define
our arbitrary scalar function, which looks like this in Futhark:

.. code-block:: Futhark

   let f x = if 0 < x then f32.sqrt x
                      else x

In Futhark we can ``map`` almost any function:

.. code-block:: Futhark

   let sqrt_when_pos xs = map f xs

It just works, and will run quite fast too.  What about Mandelbrot?
Just as simple:

.. code-block:: Futhark

   let divergence (c: complex) (d: i32): i32 =
     let (_, i') =
       loop (z, i) = (c, 0)
       while i < d && dot(z) < 4.0 do
         (add_complex c (mult_complex z z),
          i + 1)
     in i'

   let mandelbrot [n][m] (c: [n][m]complex) (d: i32) : [n][m]i32 =
     map (map (\x -> divergence x d)) c

For simplicity, I'm not using a complex number library, so things look
a bit more awkward than they have to.  `The full code is available
here <../static/higher_order_mandelbrot.fut>`_.

What about performance?  I mentioned that the NumPy-style Mandelbrot
is inefficient because of excessive memory traffic, but how bad is it
really?  Comparing GPU-accelerated Futhark with sequential NumPy isn't
fair, but I can implement the NumPy approach in Futhark:

.. code-block:: Futhark

   let numpy_mandelbrot [n][m] (c: [n][m]complex) (d: i32) : [n][m]i32 =
     let nm = n*m
     let c' = flatten_to nm c
     let output = replicate nm 0
     let z = replicate nm (0,0)
     let (output, _) =
       loop (output, z) for i < d do
       let notdone = map (\(a,b) -> (a*a + b*b) < 4) z
       let is = map2 (\b i -> if b then i else -1) notdone (iota nm)
       let inc = map2 add_complex (map (\x -> mult_complex x x) z) c'
       in (scatter output is (replicate nm i),
           scatter z is inc)
     in unflatten n m output

This is actually a bit more efficient than the original NumPy
formulation, as I'm avoiding some expensive filters.  It sure looks
nasty, but how fast is it?  On my AMD Vega 64 GPU and for a 300x300
array, ``numpy_mandelbrot`` runs in **7846 microseconds**, while
``mandelbrot`` runs in **110 microseconds**.  That's approaching two
orders of magnitude faster!  This is entirely down to ``mandelbrot``
being able to keep all its intermediate results in registers, and GPUs
are *ludicrously* fast when they never have to touch memory.  In
contrast, ``numpy_mandelbrot`` constantly has to shuffle data across
the relatively slow memory bus (350GiB/s), not to mention a lot of
extra synchronisation because many more discrete GPU kernels are
involved.

In conclusion, higher-order parallelism programming is just as easy as
first-order parallel programming, because it is still race-free and
fully deterministic.  But it allows us not just more powerful methods
of abstraction, but also potentially much better performance.
