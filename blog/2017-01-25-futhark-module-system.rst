---
title: The Futhark Module System
author: Troels Henriksen
description: We recently added a higher-order module system to Futhark - this posts describes how it works, and why we chose the design we did.
---

When most programmers think of module systems, they think of rather
utilitarian systems for namespace control and splitting programs
across multiple files.  And in most languages, the module system is
indeed little more than this.  But when it became time to add a module
system to Futhark, we eyed an opportunity to address several
long-standing weaknesses in one fell swoop.  We observed that an
`ML`_-style module system would allow us to add modularity, namespace
control, polymorphism, and higher-order functions, or at least
functional equivalents.  In short, we would gain the ability to write
generic code in Futhark; something we have sorely missed.  And
crucially: the ML module system is defined entirely in terms of
compile-time substitution.  This gives us a guarantee that no matter
which abstractions and parametric behaviour is encoded by the
programmer via the module system, we can strip it away immediately,
and produce a program in the monomorphic first-order core language
that is expected by our optimising compiler.  This is a core part our of
`language design philosophy`_.

.. _`language design philosophy`: https://futhark-lang.org/blog/2016-09-03-language-design.html

This blog post will give an introduction to the Futhark module system.
Prior knowledge of ML-style module systems is not required.  If you do
already have ML experience, note that I will be using Futhark's
syntax, which differs somewhat from both Standard ML and OCaml.  Most
notably, Futhark makes use of curly braces; an inescapable
prerequisite for any language that hopes for adoption.

Simple Modules
--------------

At the most basic level, a *module* (called a *struct* in Standard ML)
is merely a collection of declarations::

  module AddI32 = {
    type t = i32
    let add (x: t) (y: t): t = x + y
    let zero: t = 0
  }

Now, ``AddI32.t`` is an alias for the type ``i32``, and ``AddI32.add``
is a function that adds two values of type ``i32``.  The only peculiar
thing about this notation is the equal sign before the opening brace.
The declaration above is actually a combination of a *module
binding*::

  module ADDI32 = ...

And a *module expression*::

  {
    type t = i32
    let add (x: t) (y: t): t = x + y
    let zero: t = 0
  }

In this case, the module expression is just some declarations enclosed
in curly braces.  But, as the name suggests, a module expression is
just some expression that returns a module.  A module expression is
syntactically and conceptually distinct from a regular value
expression, but serves much the same purpose.  The module language is
designed such that evaluating a module expression can always be done
at compile time.

Apart from a sequence of declarations, a module expression can also be
merely the name of another module::

  module Foo = AddInt32

Now every name defined in ``AddInt32`` is also available in ``Foo``.
At compile-time, only a single version of the ``add`` function is
defined.

Module Types
------------

What we have seen so far is nothing more than a simple namespacing
mechanism.  The ML module system only becomes truly powerful once we
introduce module types and parametric modules (in Standard ML,
these are called *signatures* and *functors*).

A module type is the counterpart to a value type.  It describes which names
are defined, and as what.  We can define a module type that describes ``AddInt32``::

  module type Int32Adder = {
    type t = i32
    val add: t -> t -> t
    val zero: t
  }

As with modules, we have the notion of a *module type expression*.  In
this case, the module type expression is a sequence of *spec*s
enclosed in curly braces.  A spec is a requirement of how some name
must be defined: as a value (including functions) of some type, as a
type abbreviation, or as an abstract type (which we will return to
later).

We can assert that some module implements a specific module type via
module type ascription::

  module Foo = AddInt32 : Int32Adder

Syntactical sugar that allows us to move the module type to the left
of the equal sign makes a common case look smoother::

  module AddInt32: Int32Adder = {
    ...
  }

When we are ascribing a module with a module type, the module type
functions as a filter, removing anything not explicitly mentioned in
the module type::

  module Bar = AddInt32 : { type t = int
                            val zero: t }

An attempt to access ``Bar.add`` will result in a compilation error,
as the ascription has hidden it.  This is known as an *opaque*
ascription, because it obscures anything not explicitly mentioned in
the module type.  The module systems in Standard ML and OCaml support
both opaque and *transparent* ascription, but in Futhark we support
only the former.  This example also demonstrates the use of an
anonymous module type.  Module types work much like `structural
types`_ known from e.g. Go ("compile-time duck typing"), and are named
only for convenience.

We can use type ascription with abstract types to hide the definition
of a type from the users of a module::

  module Speeds: { type thing
                   val car: thing
                   val plane: thing
                   val futhark: thing
                   val speed: thing -> int } = {
    type thing = int

    let car: thing = 0
    let plane: thing = 1
    let futhark: thing = 2

    let speed (x: thing): int =
      if      x == car     then 120
      else if x == plane   then 800
      else if x == futhark then 10000
      else                      0 -- will never happen
  }

The (anonymous) module type asserts that a distinct type ``thing``
must exist, but does not mention its definition.  There is no way for
a user of the ``Speeds`` module to do anything with a value of type
``Speeds.thing`` apart from passing it to ``Speeds.speed`` (except
putting it in an array or tuple, or returning it from a function).
Its definition is entirely abstract.  Furthermore, no values of type
``Speeds.thing`` exist except those that are created by the ``Speeds``
module.

Parametric Modules
------------------

While module types serve some purpose for namespace control and
abstraction, their most interesting use is in the definition of
parametric modules.  A parametric module is conceptually
equivalent to a function.  Where a function takes a value as input and
produces a value, a parametric module takes a module and produces a
module.  For example, given a module type::

  module type Monoid = {
    type t
    val add: t -> t -> t
    val zero: t
  }

We can define a parametric module that accepts a module satisfying
the ``Monoid`` module type, and produces a module containing a
function for collapsing an array::

  module Sum(M: Monoid) = {
    let sum (a: []M.t): M.t =
      reduce M.add M.zero a
  }

There is an implied assumption here, which is not captured by the type
system: the function ``add`` must be associative and have ``zero`` as
its neutral element.  These constraints are from the parallel
semantics of ``reduce``, and the algebraic concept of a `monoid`_.
Note that in ``Monoid``, no definition is given of the type ``t`` - we
only assert that there must be some type ``t``, and that certain
operations are defined for it.

We can use the parametric module ``Sum`` thus::

  module SumI32s = Sum(AddInt32)

We can now refer to the function ``SumI32s.sum``, which has type
``[]i32 -> i32``.  The type is only abstract inside the definition of
the parametric module.  We can instantiate ``Sum`` again with
another module; this one anonymous::

  module Prod64s = Sum({
    type t = f64
    let add (x: f64) (y: f64): f64 = x * y
    let zero: f64 = 1.0
  })

The function ``Prodf64s.sum`` has type ``[]f64 -> f64``, and computes
the product of an array of numbers (we should probably have picked a
more generic name than ``sum`` for this function).

Operationally, each application of a parametric module results in
its definition being duplicated and references to the module parameter
replace by references to the concrete module argument.  This is quite
similar to how C++ templates are implemented.  Indeed, parametric
modules can be seen as a simplified variant with no specialisation,
and with module types to ensure rigid type checking.  In C++, a
template is type-checked when it is instantiated, whereas a
parametric module is type-checked when it is defined.

Parametric modules, like other modules, can contain more than one
declaration.  This is useful for giving related functionality a common
abstraction, for example to implement linear algebra operations that
are polymorphic over the type of scalars.  This example uses an
anonymous module type for the module parameter, and the ``open``
declaration, which brings the names from a module into the current
scope::

  module Linalg(M: {
    type scalar
    val zero: scalar
    val add: scalar -> scalar -> scalar
    val mul: scalar -> scalar -> scalar
  }) = {
    open M

    let dotprod (xs: [n]scalar) (ys: [n]scalar): scalar =
      reduce add zero (zipWith mul xs ys)

    let matmul (xss: [n][p]scalar) (yss: [p][m]scalar): [n][m]scalar =
      map (\xs -> map (dotprod xs) (transpose yss)) xss
  }

We are using these facilities to carve a `Futhark standard library`_,
although it is still very sparse.

Modules versus Higher-Order Functions
-------------------------------------

The above examples of parametric modules could equally well have
been implemented using polymorphic higher-order functions.  Indeed,
there has been work in the ML community on blurring the `phase
distinction`_ between modules and values.  A module can be viewed as
nothing but a record containing types and values.  However, for
Futhark, we *like* the phase distinction.  We want to be sure we can
compile away all the higher-order behaviour, in order to guarantee
simple lightweight code that does not have to keep function pointers
or closure objects around.  An ML-style module system gives us just
this, and is very simple to implement.  The implementation in Futhark
was added in just one week, at a cost of less than 500 lines of code,
although it did build on an earlier embryonic module system without
module types or parametric modules.

Of course, we are well aware that the module system is significantly
more verbose and clunky than proper higher-order functions.  We intend
to add shorthand forms for just those cases that can be encoded in the
module system, likely including some mechanism similar to type classes
in order to permit ad-hoc bounded polymorphism.  With our module
system, we now have a basis that is powerful enough to encode the
generic code we need, as well as a compilation model that is able to
remove all the overhead of abstraction.  All we have left to do is add
more convenient interfaces to the sound core functionality.

.. _ML: https://en.wikipedia.org/wiki/Standard_ML
.. _`structural types`: https://en.wikipedia.org/wiki/Structural_type_system
.. _`monoid`: https://en.wikipedia.org/wiki/Monoid
.. _`phase distinction`: https://en.wikipedia.org/wiki/Phase_distinction
.. _`Futhark standard library`: https://github.com/diku-dk/futhark/tree/master/futlib
