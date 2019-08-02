---
title: Towards Size Types in Futhark
author: Troels Henriksen
description: As is the fate of every language to come out of a
             university department, it is time to make the Futhark type system more
             complicated.
---

Futhark is not a large or particularly innovative language.  We prefer
to keep the novelties in the compiler, and only include language
features that have already proven their worth in existing functional
languages.  One exception to this principle is *size annotations*, by
which functions can express pre- and post-conditions on their
arguments and return values.  The classic example is matrix
multiplication, where we can state that the innermost sizes of the
array arguments must match, and that the shape of the result
corresponds to the shape of the arguments:

.. code-block:: Futhark

  let matmul [n][m][p] (x: [n][m]i32) (y: [m][p]i32): [n][p]i32 =
    ...

The leading ``[n][m][p]`` are not ordinary parameters, but rather
*size parameters*.  They are not passed explicitly when calling
``matmul``, but rather inferred from the matrices passed for ``x`` and
``y``.  Size parameters are in scope inside the function definition as
``i32``-typed variables, which means we can define a function for
returning the length of an array as thus:

.. code-block:: Futhark

  let length [n] 'a (xs: [n]a) = n

The ``length`` function (which is also polymorphic in the element type
of ``xs``) does not use any of the values of ``xs`` - it just returns
the size ``n``.

We can even write functions where we state that the size of an array
we return is the same as the value of one of our parameters, like in
``iota``:

.. code-block:: Futhark

  let iota (n: i32) : [n]i32 = 0..<n

For simplicity, dimensions can be only names or constants, not
compound expressions.  I'll return to the ramifications of this.

We have found size annotations to be very useful for understanding
code, and it is one of the language features that Futhark programmers
(few as they are) tend to single out as particularly useful, as size
errors are some of the most common problems in array programming.
Unfortunately, size annotations have one major flaw: they are not
truly part of the type system.  Currently, size annotations are
ignored by the type checker and verified at run-time.

In this post, I will outline an idea for moving from size annotations
to checkable *size types*.  The design is not yet fully formed, as we
shall see.  While size types have a strong connection to `dependent
types <https://en.wikipedia.org/wiki/Dependent_type>`_, the way I
intend to use them in Futhark are not quite the same as in truly
dependently typed languages like `Idris
<https://www.idris-lang.org/>`_, so I cannot just take a design from
the latest POPL paper.  In particular, I want to preserve the Futhark
current programming experience as much as possible, notably including
preserving solid type inference.  This is not so much because there is
all that much Futhark code that I wish to avoid breaking, but because
Futhark is currently a rather simple, accessible, and *fun* language,
and I would like to keep it that way.

Basic type inference
--------------------

The idea behind size types is based on an extension of classic
`Hindley-Milner type inference
<http://dev.stephendiehl.com/fun/006_hindley_milner.html>`_.  So
first, let us review what that's all about.  There are two main
concepts we must understand: *instantiation* and *let-generalisation*.

Instantiation
~~~~~~~~~~~~~

We will get to type inference shortly.  For now, consider an
explicitly typed identity function in Futhark:

.. code-block:: Futhark

  let id 'a (x: t) : a = x

This function is known to the type checker by the following *type scheme*:

.. code-block:: Futhark

  val id 'a : a -> a

The type scheme contains all the information that is needed to check
uses of the function.  Specifically, the type scheme tells us that
``id`` has a single *type parameter* (``a``).  Consider now an application:

.. code-block:: Futhark

  id 2

where ``i`` has type ``i32``.  Note that an explicit argument is not
provided for the type parameter.  Whenever a reference to a
polymorphic definition such as ``id`` is encountered by the type
checker, its type scheme is *instantiated*.  The instantiation
procedure generates a fresh *type variable* for each type parameter,
and then replaces each type parameter in the type with its
corresponding type variable.  Type variables are a bit of machinery
that stand in for currently unknown types.  A type variable can be
`unified
<https://en.wikipedia.org/wiki/Unification_(computer_science)>`_ with
another type, which will replace all instances of the type variable
with the other type.

For example, in the application ``id 2``, the ``id`` function might be
instantiated by generating a fresh type variable ``t0``, and replacing
``a`` with ``t0`` in the type scheme of ``id``, such that the type of
*this* instance of ``id`` now has type ``t0 -> t0``.  Note the absence
of type parameters: these are only present in type *schemes*, not
*types*.  (`Higher-ranked types
<https://wiki.haskell.org/Rank-N_types>`_ are different, but these do
not exist in Futhark.)  Since we are applying ``id`` to ``2``, the
type checker will unify the parameter type of ``id`` (``t0``) with the
type of ``2`` (``i32``), producing the substitution ``t0 ⟼ i32``,
which is then applied whenever ``t0`` occurs.  Since ``t0`` also
occurs in the return type of ``id``, we find that the final type of
``id 2`` is ``i32``.

Let-generalisation
~~~~~~~~~~~~~~~~~~

*Let-generalisation* can be seen as the dual to instantiation, in that
it turns un-unified type variables into type parameters in order to
infer polymorphic definitions.  The idea is simple.  Whenever we have
a ``let``-binding

.. code-block:: Futhark

   let x = e...

then if any type variables constructed while inferring the type of
``e`` remain in the type of ``e``, those type variables are turned
into type parameters when constructing the type scheme for ``x``.

For example, consider the following definition:

.. code-block:: Futhark

  let f x = x

When new names are bound (here, the parameter ``x``), we generate a
new type variable, say ``tx``.  Hence, the body of ``f`` has type
``tx``, and ``f`` as a whole has type ``tx -> tx``, where ``tx`` is a
type variable.  Let-generalisation finds all such type variables and
turn them into type parameters for the type scheme of ``f``:

.. code-block:: Futhark

  val f 'tx : tx -> tx

There need not be any relationship in naming between type parameters
and type variables.  In practice, type variables tend to have ugly
internal names (because many of them are typically generated during
type-checking nontrivial functions), while we would like inferred type
parameters to have short and clean names.  Thus, the compiler may
perform some renaming before constructing the type scheme.

In practice, and in Futhark, let-generalisation is only done for
``let``-bindings of *functional* type.  We shall later see an example
where this causes some problems.

Rigid type variables
~~~~~~~~~~~~~~~~~~~~

The type variables discussed so far have been placeholders that could
be replaced (through unification) with concrete types.  We call such
types *nonrigid*.  In contrast, *rigid* types do not unify with
anything but themselves.  The most common source of rigid types are
primitive types and explicit type parameters:

.. code-block:: Futhark

  let g 'a (x: a) : i32 = x

Here we are writing a function that tries to turn an ``a`` into an
``i32``.  Clearly this should not be well typed.  And indeed it is
not: both ``a`` and ``i32`` are *rigid*, so they cannot be unified
with each other.

As a contrast, consider this contrived function:

.. code-block:: Futhark

  let h 'a x : a = x

Here the type checker will generate a nonrigid type variable ``xa``
for ``x``, unify ``xa`` with ``a`` to produce the substitution ``xa ⟼
a``, and finally infer the following type scheme:

.. code-block:: Futhark

  let h 'a : a -> a

Some presentations do not have the notion of rigid type variables at
all - they simply consider them *types*, rather than type *variables*.
However, for size types, it is convenient to treat them as a
degenerate class of variables, as we shall see.

Extending Hindley-Milner with Size Types
----------------------------------------

The basic idea is pretty simple: have *size variables* that act much
as type variables, using the same unification machinery.  We have a
distinction between *rigid sizes*, which cannot unify with anything
but themselves, and *nonrigid size variables*, which do unify.  When
unifying two array types ``[d1]t1`` and ``[d2]t2`` we unify ``d1``
with ``d2`` and ``t1`` with ``t2``.

Consider the type of ``zip``:

.. code-block:: Futhark

  val zip [n] 'a 'b : [n]a -> [n]b -> [n](a,b)

Now suppose that we are type-checking the following application:

.. code-block:: Futhark

  zip xs ys

The type scheme of ``zip`` is instantiated and each type- and size
parameter replaced with a new non-rigid type variable.  Let us say
that post-instantiation, this occurrence of ``zip`` has the following
type:

.. code-block:: Futhark

  [d0]t1 -> [d0]t2 -> [d0](t1, t2)

When checking the application of ``zip`` to ``xs``, we have to unify
the parameter type ``[d0]t1`` with the type of ``xs``, which we will
suppose is ``[10]bool``.  The unification succeeds and produces the
following substitutions:

.. code-block:: Futhark

  d0 ⟼ 10
  t1 ⟼ bool

This means that we infer the type of ``zip xs`` as ``[10]t2 ->
[10](bool, t2)``.  When we then try to apply this to ``ys``,
unification will succeed only if the size of ``ys`` can be unified
with the constant size ``10`` (so, ``ys`` must be an 10 element
array).

Let-generalisation functions more or less the same.  Suppose we are
type-checking the following ``let``-binding of a lambda abstraction:

.. code-block:: Futhark

  let f = \xs ys -> zip xs ys

After unification, the right-hand side will be inferred to have type
``[d0]t1 -> [d0]t2 -> [d0](t1, t2)``, where ``d0`` is a nonrigid size
variable and ``t1``, and ``t2`` are nonrigid type variables.
Let-generalisation then takes place, turning the former into a size
parameter, and the latter two into type parameters, yielding the
following type scheme:

.. code-block:: Futhark

  val f [d0] 't1 't2 = [d0]t1 -> [d0]t2 -> [d0](t1, t2)

When replacing a type variable with an array type, the dimensions of
that array type are propagated as well.  This has the consequence that
if the following fully polymorphic function is called with two arrays,
those arrays must have the same size:

.. code-block:: Futhark

  let pair 'a (x: a) (y: a) = (x, y)

This restriction is actually already in place in Futhark (although by
a different mechanism), and was added to slowly prepare the way for
size types.

This simple mechanism is enough to handle a surprising majority of the
cases that occur even in nontrivial Futhark programs.  However, we
also need to think about how to handle less well behaved programs.

Unknowable sizes
~~~~~~~~~~~~~~~~

Consider the ``filter`` function, which removes those elements of an
array for which some function returns false:

.. code-block:: Futhark

  val filter [n] 'a = (a -> bool) -> [n]a -> [?]a

What should we put in place of the question mark?  The only
non-constant size available to use is ``n``, and that is clearly not
correct.  We call the size of such an array *existential*, based on
the "∃" quantifier from logic: we know that the array must have *some*
size, but we cannot know what it is until the function returns.  To
handle such cases, we just leave the dimension empty:

.. code-block:: Futhark

  val filter [n] 'a = (a -> bool) -> [n]a -> []a

This is permitted *only* in declarations like type schemes and when
annotating parameter types: when type-checking an expression, we would
like to have the invariant that all array dimensions have a size,
although that size may well be a size variable.  To accomplish this,
whenever we type-check an application of a function with empty
dimensions in its return type, we instantiate those empty dimensions
with new *rigid* size variables.  The rigidity is crucial, as it means
the shape of the array cannot match the shape of *any other array*.
For example, in the expression

.. code-block:: Futhark

   zip (filter p xs) ys

there is *no expression ``ys``* that would make this well-typed.  In
most cases, when we want to do something interesting with an array of
existential size, we will have to insert an explicit *size coercion*.
These coercions are dynamically checked, and change the type of the
array as far as the type checker is concerned:

.. code-block:: Futhark

  zip (filter p xs : [10]bool) ys

This convinces the type checker that ``filter p xs`` has type
``[10]bool``.  Only the sizes of each dimension may be changed this
way - the rank and element type of the array must be as otherwise
inferred.  Size coercions are intended to be the *sole* syntactic
construct that can fail at run-time with a shape error.

Similarly, we may have ``if`` expressions where the two branches
return arrays of two different sizes:

.. code-block:: Futhark

  if b then iota 10
       else iota 20

Those dimensions that do not match will have their size replaced with
a rigid size variable.

Another case of rigid sizes occurs when a function with a
size-dependent type is given an argument for its size parameter that
cannot syntactically be a size.  Consider again ``iota``:

.. code-block:: Futhark

  val iota : (n: i32) -> [n]i32

The type of ``iota x`` should be ``[x]i32``, but what about ``iota
(x+1)``?  Syntactically, there is no such thing as a type
``[x+1]i32``.  While it is likely we may lift this restriction some
day (it is in place to avoid having to solve complicated arithmetic
problems during unification), for now such an application will result
in a rigid size.  A simple workaround is to ``let``-bind ``x+1`` to a
name and then passing that name to ``iota``.

Unsolved Problems
~~~~~~~~~~~~~~~~~

All of this is work-in-progress.  While I have `a branch
<https://github.com/diku-dk/futhark/tree/dependent-types>`_
implementing most of this (and it is able to compile most of our
benchmarks and tests), there are still unsolved problems in both
implementation and concept.  As an example of the latter, consider the
following program:

.. code-block:: Futhark

  let f xs = zip (filter p xs) xs

By the rules discussed so far, ``xs`` will initially be given the type
``[d0]t1``, where ``d0`` is a nonrigid size variable, ``filter p xs``
will have type ``[d1]t1`` where ``d1`` is a *rigid* type variable, and
then due to the ``zip`` we will produce the substitution ``d0 ⟼ d1``,
and so the whole type of ``f`` will be ``[d1]t1 -> [d1](t1, t2)``,
where ``d1`` is a rigid size variable computed inside ``f`` itself.
Clearly this makes no sense.  I think the rule is that it should be a
type error for a rigid size variable to propagate into the type of
something that is being ``let``-bound.

Another problem brought about by rigid size variables is the potential
for time travel.  Specifically, since Futhark allows term-level values
to be extracted from types (remember ``length`` from above), I am
concerned that a type-based unification performed inside a
control-flow path (``if`` branch) that is not executed at run-time,
can influence the size of some array generated *outside* the ``if``.
Suppose first that the function ``concat`` has the following type::

  val concat [n] [m] 'a = [n]a -> [m]a -> []a

Note that if we call ``concat`` with two-dimensional arrays, the row
sizes must match exactly, because they are identified by the same type
parameter (``a``).

Now ponder the following program:

.. code-block:: Futhark

  let shape [n] [m] (xss: [n][m]i32) = (n, m)

  let f (b: bool) (xs: []i32) =
    let arr = [] -- Inferred as type [][]i32.
    in if b
       then shape (concat [filter p xs] arr)
       else shape arr

What should ``f false [1,2,3]`` return?  Due to the ``concat``, we are
forcing the row size of ``arr`` to be the size of ``filter p xs``,
which is a rigid size variable.  However, if ``b`` is false, then
``filter p xs`` will never be run, yet we still inspect the size of
``arr``!  I'm not quite sure how to handle this.  Either we need to
make ``arr`` properly polymorphic (currently only functional bindings
are let-generalised), or we need to institute rules for certain
syntactic constructs (mostly array literals and related cases) that
prevent unification with rigid size variables.  The fundamental
problem is that at run-time, we may need to construct an actual
multidimensional array *right now*, and when that happens, we better
have *actual* sizes for the dimensions available, and not just a
promise that they'll match the size of some other array that may or
may not be computed in the future.

As a final problem, consider the type of ``map``:

.. code-block:: Futhark

  val map [n] 'a 'b : (a -> b) -> [n]a -> [n]b

Should ``map iota is`` be well typed?  Probably not, since this would
only work if all values of ``is`` are the same.  But how should this
be prevented?  Maybe type checking of function application should take
a closer look at whether any dependent typing is going on in case of
higher-order functions, but a clear rule has not yet crystallised.

Perspective
~~~~~~~~~~~

I don't think ironing out the kinks in size types is an insurmountable
problem.  Fundamentally, I think they are quite simple, and I'm
perfectly willing to give up some completeness (and resorting to size
coercions) in order to maintain simplicity and type inference.
However, to fully convince myself and others, we may have to break
open the big box of Greek letters, and actually prove some properties
about the type system.  I'll get around to that once I figure out
exactly what the rules are even supposed to *be*.

Promisingly, it took fairly little work to make sure most of our
benchmarks type-check under the current prototype of size types.  I
had to fix only a few things, and in most cases I would say they even
improved the style of the program.  In a few cases, I even fixed real
(but dormant) bugs.  I think this is because size types more or less
just codify what is already good Futhark coding practice.

There is of course also the risk that the type checker is simply buggy
and is letting incorrect programs slip by.  That's the frustrating
part about working on a type checker: your programs may type check,
but that does not mean that the type checker is correct...
