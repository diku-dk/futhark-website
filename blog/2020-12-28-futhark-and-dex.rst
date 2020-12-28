---
title: A comparison of Futhark and Dex
author: Troels Henriksen
description: Dex is another interesting functional array language, and this post looks at how it compares to Futhark.
---

`Dex <https://github.com/google-research/dex-lang>`_ is a functional
array programming language developed by a team of researchers at
Google.  I recently re-read `their paper
<https://openreview.net/pdf?id=rJxd7vsWPS>`_, which got me excited
enough to want to take a closer look.  Dex and Futhark are more or
less aimed at the same kinds of problems, so my interpretation of Dex
is rooted in how it differs from Futhark.  In this post I will
describe some of the interesting differences based on `translating
five Dex example programs to Futhark
<../examples.html#examples-from-dex>`_.  I'm not a Dex expert, so
maybe I've missed a thing here or there.

Futhark wasn't originally designed to be a user-facing programming
language.  We were doing research in compiler optimisations for
parallel computers, and the language was just a crude little thing so
we could write programs for our optimiser to work on.  Over time the
language grew and eventually became fairly pleasant to use (`full
story here
<2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html>`_),
but it was still never designed as a cohesive or novel approach to
array programming.  That also means it's fairly conventional or even
old-fashioned, as functional languages go.  In contrast, Dex's authors
had more imagination and designed their language from the start with
novel ideas, chief of which is to consider *index sets as types*.  To
illustrate the idea, here is how to compute all-pairs L₁ distances in
Dex::

  pairwiseL1 ::  n=>d=>Real -> n=>n=>Real
  pairwiseL1 x = for i j.sum (for k. abs (x.i.k - x.j.k))

The ``n=>d=>Real`` is the type of an ``n`` by ``d`` array of
``Real``s.  Dex leans heavily on an analogy between arrays and
functions, as arrays can be seen as merely functions from indexes to
values.  In Futhark, we'd write this type as ``[n][d]Real``.  Note
that in Dex, ``n`` and ``d`` are completely abstract type parameters,
while in Futhark they are term-level variables.

The real advantage of Dex's approach is that it permits a very
lightweight notation for index spaces.  For example, ``for i j.e``
produces a two-dimensional array where each element is given by the
expression ``e``, and the type checker figures out the span of ``i``
and ``j`` based on the context.  For example, in ``for k``, Dex
figures out that ``k`` must be part of the index set ``d``, because it
is used to index the innermost dimension of ``x``.  Pretty cool!

A naive translation to Futhark would be this:

.. code-block:: Futhark

   let pairwiseL1 [n][d] (x: [n][d]f64) =
     tabulate_2d n n (\i j -> f64.sum (tabulate d (\k -> x[i,k] - x[j,k])))

Note that the tabulation functions require explicit size-passing, and
that the indexes are just integers - the type checker will not help us
if we accidentally use the ``k`` along the wrong dimension.

Of course, the above is not how you'd actually write this program in
Futhark.  Instead you'd first define a function for computing the L₁
distance:

.. code-block:: Futhark

   let L1 [n] (xs: [n]f64) (ys: [n]f64) : f64 =
     map2 (-) xs ys |> map f64.abs |> f64.sum

And then you'd apply it to all the pairs:

.. code-block:: Futhark

   let pairwise_L1 [n][m] (xss: [n][m]f64) : [n][n]f64 =
     map (\a -> map (\b -> L1 a b) xss) xss

I think this program illustrates the main difference in philosophy
between Dex and Futhark.  While Dex uses dependent types to secure an
index-based notation, Futhark instead encourages index-free
programming.  I suspect the two approaches are fundamentally
equivalent, but it's an interesting contrast that I think is due to
the two language's different backgrounds.  Dex is specifically
designed to implement scientific code and formulae, which is
traditionally very index-oriented.  Futhark is more about supporting a
"traditional" combinator-based functional programming style, but just
making it run much faster.  You could view Futhark as a data-parallel
ML, while Dex is `higher-order dependently typed Einstein summation
<https://en.wikipedia.org/wiki/Einstein_notation>`_.

I also suspect this focus on indexes is because the Dex authors have a
background of being frustrated with NumPy-style programming, where the
absence of efficient indexing can be quite restrictive.  They even
even use this NumPy implementation of L₁ distances as motivation in
their paper::

  def pairwiseL1(x):
    return sum(abs(x.T - x[..., newaxis]), axis=1)

I certainly agree that this is hard to read.

The good ones
-------------

Porting a two-line Dex program to Futhark is enough to wax
philosophically for a paragraph or two, but it's still a pretty
shallow comparison.  Therefore, I also ported five of `the Dex example
programs
<https://github.com/google-research/dex-lang/tree/main/examples>`_,
plus whatever of the `Dex prelude
<https://github.com/google-research/dex-lang/blob/main/lib/prelude.dx>`_
I needed along the way.  I'm not going to claim that I ported the five
most difficult programs, but at least one of them was quite
complicated.  The Futhark programs total about 450 lines of code
(excluding comments and blanks).

My general impression is that when it comes to expressing parallelism,
Dex and Futhark are about equivalent.  Dex's index notation is more
concise, but I personally find it slightly easier to understand and
decompose Futhark expressions.  As an example, this Dex function
computes the covariance of a matrix::

  def covariance (n:Type) ?-> (d:Type) ?->
      (xs:n=>d=>Float) : (d=>d=>Float) =
     xsMean :    d=>Float = (for i. sum for j. xs.j.i) / IToF (size n)
     xsCov  : d=>d=>Float = (for i i'. sum for j.
                             (xs.j.i' - xsMean.i') *
                             (xs.j.i  - xsMean.i )   ) / IToF (size n - 1)
     xsCov

In Futhark we write it as:

.. code-block:: Futhark

   let covariance0 [n] (xs:[n]f64) (xsm:f64) (ys:[n]f64) (ysm:f64) =
     f64.sum (map2 (\x y -> (x-xsm) * (y-ysm)) xs ys) / f64.i64 (n-1)

   let covariance [n][d] (xs:[n][d]f64) =
     let xsT = transpose xs
     let means = map mean xsT
     in map2 (\a a_mean ->
                map2 (\b b_mean -> covariance0 a a_mean b b_mean)
                     xsT means)
             xsT means

It's certainly more verbose, but I had to read the Dex function
carefully to understand what the indexes implied, while I have a much
easier time understanding the structure of the computation from the
Futhark formulation.  Of course, I also have years of experience with
Futhark, compared to just days with Dex.

Most of the translations were pretty simple, for example the
`Mandelbrot set <../examples/dex-mandelbrot.html>`_, `Monte Carlo pi
<../examples/dex-pi.html>`_, and `Brownian motion
<../examples/dex-brownian-motion.html>`_ programs.  One difference that
made me feel *major* jealousy is that the ``dex script`` command is
also able to generate `pleasant reports
<https://google-research.github.io/dex-lang/mandelbrot.html>`_
containing both the code and visualisations and plots of various
values.  We definitely need a tool like this for Futhark!

The `Sierpinski triangle <../examples/dex-sierpinski.html>`_ program
has a fun little detail in Dex, which is that the ``randIdx`` function
uses the Dex type system to determine the range of the index being
produced.  While the ``randIdx`` function itself can still be wrong,
this makes it hard to *use* it incorrectly.  The Futhark translation
of ``randIdx`` asks the user to pass in a range explicitly, and also
returns just an integer.

The bad one
-----------

The largest ported example by far is `a ray tracer
<../examples/dex-raytrace.html>`_.  It uses ray marching with `signed
distance functions
<https://en.wikipedia.org/wiki/Signed_distance_function>`_ to describe
objects.  The Dex program rather casually uses the ``grad`` operator
to apply `automatic differentiation (AD)
<https://en.wikipedia.org/wiki/Automatic_differentiation>`_ to compute
surface normals from the distance function.  This is a really elegant
technique, but Futhark does not (yet!) have a ``grad`` operator.  In
Futhark, the sensible thing to do is to hard-code the gradient
functions for the three different kinds of objects, so of course I
instead used `forward-mode AD with dual numbers
<../examples/dual-numbers.html>`_ implemented via the Futhark module
system.  The resulting code finally convinced me that built-in AD is a
necessity for a modern numerical languages.  I was on the fence
before, since I worry that doing it well will be invasive in both the
language and compiler, but I never want to write this kind of
boilerplate again.

The rest of the ray tracer was fairly straightforward to implement.
Dex uses its effect system to implement the loop where the lights in
the scene apply their contributions to a given point, which I wrote in
Futhark as basically a fold.  In fact, I didn't yet find a Dex example
where the effect system was more than a small notational convenience.
I'm sure there's one, though!  Effect systems are not things you just
add on a lark.

There was one part that confused me initially, but which makes perfect
sense in retrospect.  The ray tracer normalises the intensity of all
pixels (triples of floats) based on the average intensity (unusual I
think, but fine).  In Dex this is done like this::

  image / mean (for (i,j,k). image.i.j.k)

When I first read this, I couldn't figure out whether it was
normalising *per channel*.  I always get a bit wary when overloaded
operators like that ``/`` are involved.  Of course, that
``for``-expression is over a *single* index that just happens to be a
triple, and the components of which are then used to index the
three-dimensional ``image`` array.  It's really just flattening the
array, and the type checker makes the individual ``i``, ``j`` and
``k``s take on the appropriate value.

Conclusions
-----------

With respect to expressing parallelism, Dex and Futhark seem
equivalent in expressive power, but Dex has the edge in concision.
I'd be curious about going the other way, and porting some of the
original Futhark benchmark programs *to* Dex, like `local volumetric
calibration
<https://github.com/diku-dk/futhark-benchmarks/blob/master/finpar/LocVolCalib.fut>`_.

Dex has several small conveniences over Futhark: while the effect
system didn't matter much for the examples I looked at, Dex's type
classes and broadcasting operators did help a bit with making things
more concise.

If you need AD, then Dex is miles ahead of Futhark.  While I managed
to implement the surface normals in the ray tracer, I gave up on
porting `mcmc.dx
<https://google-research.github.io/dex-lang/mcmc.html>`_ because it
contains a higher-order function that applies the ``grad`` operator to
a functional argument.  This would have to be implemented with a
higher order parametric module (`which I wrote were useless not long
ago
<https://futhark-lang.org/blog/2019-12-18-design-flaws-in-futhark.html#higher-order-modules>`_),
but I just didn't have the heart for it.  I'll keep this as a usage
case for when we implement AD properly.

I didn't look much at performance, since Dex is sparsely documented
and the benchmarking tools seem to be mostly for internal use.  I
performed a rough timing of sequential execution of the ray tracer,
where the Futhark and Dex versions are about equally fast.  Dex also
has multi-threaded and CUDA backends, but I did not try them.

Speaking of sparse documentation, Dex is still young and appears to be
changing frequently.  My understanding is based on `the paper
<https://openreview.net/pdf?id=rJxd7vsWPS>`_, reading the example
programs, and skimming some of the implementation.  I may have missed
important details, and this post may even be outdated by the time you
read it.

Inspiration for Futhark
=======================

I don't think it would take that much effort to let Futhark be more
implicit with respect to sizes.  An easy start would be to permit
return-size polymorphism, which would let us write a ``tabulate``
function with this type:

.. code-block:: Futhark

   val tabulate [n] 'a : (i64 -> a) -> [n]a

Currently the Futhark type checker forbids size parameters that are
used only in negative position.  This restriction is mostly because I
implemented the size type system on my own, and since I had little
experience with implementing dependent type systems, I was worried
about inadvertently admitting unsound constructs (and more
pragmatically, I was worried about bugs in the implementation).  I
locked it down more than might strictly be necessary.  If we want to
support this kind of result size inference, then we do need to figure
out what to do with expressions like

.. code-block:: Futhark

   zip (tabulate f) (filter p xs)

Here the size of the ``tabulate`` must be the size of the array
returned by ``filter``, which is existential.  As far as I can figure
based on the paper, Dex wouldn't allow an expression like the above,
as it handles existentials in an conventional and explicit manner::

  filter :: (a -> Bool) -> m=>a -> E n. n=>a

Presumably it's up to the user to do the unpacking of the existential
context as needed.  In Futhark I wanted to preserve "direct style"
programming, so you could write expressions like ``map f (filter p
xs)`` where the compiler implicitly unpacks the existential context
for you, but maybe the cost is too great.

I think *some degree* of Dex's type-safe indexing can almost already
be implemented in Futhark, especially if we loosen the above
restriction that size parameters must be used in parameters.  We
already have an example where `"phantom sizes" are used to implement
triangular arrays <../examples/triangular.html>`_.  Maybe Futhark
should go more in the direction of dependent types?  It's unfortunate
that we're a small team, as it also takes a lot of time to write
`papers about making the language go wrooooom
<https://futhark-lang.org/publications/sc20.pdf>`_...
