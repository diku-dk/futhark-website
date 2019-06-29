---
title: Giving programmers what they want, or what they ask for
author: Troels Henriksen
description: How exactly should the Futhark compiler behave when programmers use tiny arrays for convenience?
---

When designing a programming language, you sometimes encounter design
questions that have little impact, and whose implementation either way
is straightforward, but which force you to think about the core
philosophy of the language.  This post is about one such issue as
encountered in Futhark.

I shall start with an example.  By now, we have a decent amount of
experience with having other people, mostly students, write Futhark
code.  One common technique we have observed is to use small
constant-sized arrays as a data structure.  This may be a habit that
comes from NumPy or other array languages, where this seems to be a
popular style.  For example, several we might represent a point in
three-dimensional space as a three-element array::

  type vec3 = [3]f32

A function for computing the dot product of two such vectors can then
be defined as::

  let dot (a: vec3) (b: vec3) = reduce (+) 0 (map2 (*) a b)

This runs correctly, but the programmer may be dismayed at the
run-time performance.  The reason is that the Futhark compiler treats
arrays uniformly: they are located on the heap (possibly the GPU if
using an appropriate compiler), and collective operations like
``map2`` and ``reduce`` are executed on the GPU.  The base overhead of
launching a GPU operation seems to be at least 10μs, which is *vastly*
slower than multiplying and summing six floats on the CPU.
Essentially, the compiler treats all arrays as potentially large and
worth processing in parallel, which makes working on small arrays
carry a disproportionate overhead.

In this particular case, the compiler does statically know the size of
the array, although it might not in general.  Why, then, does the
compiler not simply compile operations on this small array in a more
efficient manner?  Mostly because it is not clear what the threshold
should be for considering an array "small".  There is certainly no
need for computing the dot product of a three-element vector in
parallel, but what about 100 elements?  Or 1000?  What if the vector
is already resident in GPU memory because it is the result of a
preceding operation?  No matter which cutoff point we pick, there will
be some hidden threshold, invisible to the programmer, that may have a
dramatic impact on application performance.  There will always be some
application for which the threshold is too large or too small.

We really have three options open to use:

1) Treat all arrays uniformly as potentially large.

2) Treat arrays below some threshold as "small" and compile them
   differently.

3) Combine options (1) and (2) but generating multiple versions of the
   code, then dynamically pick the best one based on the array sizes
   encountered.  This involves performing semi-automated calibration
   to fit the cut-over threshold to the specific hardware and code in
   question.

All of these carry disadvantages.  (1) works poorly for small arrays,
and (2) is unpredictable and does not handle arrays with a
statically-unknown size, but which happen to be small at run-time.
Having program performance depend to such a degree on whether the
compiler can statically deduce the size of an array, and compare it to
some built-in threshold, sounds like a frustrating programming
experience.  Further, it becomes hard to *explain* to Futhark
programmers how they should write their code to get the performance
they seek, since the compiler will be making its own guesses about
what to do.

Option (3) could in principle work, but it assumes a `sufficiently
smart compiler <http://wiki.c2.com/?SufficientlySmartCompiler>`_;
always a dicey proposition.  In practice, it would require lengthy
post-compilation calibration, similar to `profile-guided optimisation
<https://en.wikipedia.org/wiki/Profile-guided_optimization>`_, before
the programmer would get a valid impression of the run-time performance
of their code.  While such a complex approach is *sometimes* necessary
(and we have a paper coming up about doing just that in a different
setting!), it really seems overkill for this simple problem.

While the Futhark compiler is undoubtedly a complex beast, we wish to
minimise how much of its internal structure and assumptions
programmers are expected to learn (`Futhark is for the desert!
<2018-06-18-designing-a-programming-language-for-the-desert.html>`_).
Our choice is thus to go with option (1), primarily because it has
simple rules for programmers to follow.  Returning to the original
example, it should instead be written this way::

  type vec3 = {x: f32, y: f32, z: f32}
  let dot (a: vec3) (b: vec3) = a.x*b.x + a.y*b.y + a.z*b.z

This will behave predictably, since scalar components of records and
tuples are typically stored in registers (if they fit).

However, this approach does have the major drawback that it is
specialised to three-dimensional vectors, while the original
implementation could be straightforwardly extended to handle arbitrary
dimensions.  While it is no great problem to duplicate something as
simple as this dot product, it would be very annoying to repeat all of
our code to handle both array- and record/tuple representations.

Fortunately, Futhark's higher-order module system allows us to write
generic code that is parameterised over some data representation.  The
module system has been `discussed in a previous post
<2017-01-25-futhark-module-system.html>`_, but here is a quick example
of how a generic dot product can be written with the aid of the
modules from the `vector <https://github.com/athas/vector>`_ package::

  module mk_dotprod(V: vector) = {
    let dotprod (a: V.vector f32) (b: V.vector f32) =
      V.reduce (+) 0 (V.map (\(x, y) -> x+y) (V.zip a b))
  }

While we are using terms like ``map`` and ``reduce`` as in our
original formulation, their implementation depends on how the module
parameter ``V`` is instantiated.  For example, we can instantiate it
with a module where the ``V.vector`` type is represented as a
three-element tuple, and ``V.reduce``/``V.map`` are unrolled
sequential loops::

  module vector_2 = cat_vector vector_1 vector_1
  module vector_3 = cat_vector vector_2 vector_1
  module dotprod_3 = mk_dotprod vector_3

Now we can say ``dotprod_3.dotprod`` to access a function that is
identical (after some straightforward compiler simplifications) to the
specialised ``dotprod`` we wrote for three-element records.

Similarly, we can instantiate ``mk_dotprod`` with a module that
represents vectors as arrays, and provides parallel implementations of
``V.reduce``/``V.map``::

  module dotprod_any = mk_dotprod any_vector

Of course, the ``vector`` package already provides a parametric
module, ``vspace`` (`docs
<https://futhark-lang.org/pkgs/github.com/athas/vector/0.2.3/doc/lib/github.com/athas/vector/vspace.html>`_),
that contains common operations like dot product.

In conclusion: the Futhark compiler performs a great deal of compiler
magic to generate high-performance code.  This is not really
avoidable.  However, we need to pick our battles and decide where it
does major restructuring of the code, and where it compiles in a
predictable manner.  Our currently philosophy is to let the compiler
aggressively rearrange the structure of any parallelism present, but
compile "scalar code" in a predictable manner.  Also, if you tell it
that something is parallel (by using an actual parallel ``map``), then
the compiler will take it seriously.

The real takeaway here is that we need to be clear in our
documentation and educational resources that where possible, you
should prefer tuples and records over tiny arrays.
