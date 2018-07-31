---
title: The Futhark Record System
author: Troels Henriksen
description: The design of the record system in Futhark, with some notes on the design space and why we chose what we did.
---

Most programming languages, except perhaps for the most bare-bones,
support some mechanism for creating *records*, although rarely using
that term.  For the purposes of this post, a record is an object
containing labeled *fields*, each of which contains a value.  In C, a
struct is a record::

  struct point {
    int x;
    int y;
  };

In object-oriented languages, records are further embellished with
methods and (sometimes) access control, giving rise to *classes*.  For
this blog post, I will stick to the simpler incarnation of records as
simply labeled collections of values.  It turns out that even this
simple idea contains interesting design questions.  In the following,
I will discuss the various ways records crop up in various programming
languages, and describe the design and notation we have chosen for
records in Futhark.  I should note that there is (intentionally) little
novelty in our design - we have mostly picked features that already
seemed to work well elsewhere.  Futhark is a simple language designed
for (relatively) small programs that have to run really fast, so we
don't need language features that support very elaborate data
structures.  Constructing the compiler is plenty challenging already.

Basics of Records
-----------------

At its core, a record is a mapping from keys to values.  What sets
records apart from associative arrays or other key-value data
structures is that the keys for a record (the labels of the fields)
are determined at compile time.  Consider the ``point`` struct defined
above - there is no way to dynamically add or remove a field at
run-time.  In some languages (e.g. Python) the distinction is less
clear-cut, but we still tend to organise our values as some where the
keys are dynamic, and others where they are static.  Records are a
useful organising principle, even when they are all just hash tables
underneath and there is no type system enforcement.  However, as
Futhark is a statically typed language, we will need to make decisions
about how record types work.

One of the key differences between how languages handle records is
whether they use *nominal* or *structural* typing.  In C, for example,
we could define two structs with identical fields::

  struct point_a {
    int x;
    int y;
  }

  struct point_b {
    int x;
    int y;
  }

To the C compiler, these two types are distinct.  If a function
expects an argument of type ``point_a`` and we call it with a value of
type ``point_b``, we will get a type error.  Thus, C is nominally
typed: types with different names are completely distinct, except for
type aliases defined via ``typedef``.

In a structural record system, the types ``point_a`` and ``point_b``
would be identical, because they have the same structure.  This is
sometimes called static duck typing.  Go is an example of a
language that uses structural types pervasively, but there are also
languages that use both nominal and structural types for different
parts of the type system.  One such language is `Standard ML`_, where
records are structurally typed, and algebraic types are nominally
typed.

.. _`Standard ML`: https://en.wikipedia.org/wiki/Standard_ML

Records in Futhark
------------------

The Futhark record system is primarily inspired by the records in
Standard ML, and is therefore structurally typed.  One particularly
nice capability of a structural record system is anonymous record
types, which we in Futhark write as a sequence of comma-separated
field descriptions enclosed in curly braces.  For example, ``{x:i32,
y:bool}`` describes a record with two fields: ``x``, containing a
32-bit integer, and ``y`` containing a boolean.  The order in which
fields are listed does not matter, but duplicate labels are not
allowed.

Anonymous record types may seem strange at first, but quickly become
natural.  If we can write ``(int,bool)`` to denote an unnamed pair,
why not also ``{x:i32, y:bool}`` for an unnamed record?  A record is
nothing more than a tuple with labels, after all.  This also means
that there is no need for the language to have a facility for defining
named records.  The usual type abbreviation mechanism works the same
whether the right-hand side is a primitive type, a tuple, or a
record::

  type point = {x:f64, y:f64}

(In Futhark, the built-in type ``f64`` is a double-precision floating
point number.)

Records are constructed via *record expressions*::

  let some_point: point = {x=1.0, y=2.0}

A record expression is a comma-separated sequence of *field
assignments*, enclosed in curly braces.  For example, ``{x=1.0,
y=2.0}`` produces a record of type ``{x:f64, y:f64}``, which is
equivalent to the type ``point`` defined above.  The order in which
fields are listed makes no difference (except in the case of
duplicates), and so ``{y=2.0, x=1.0}`` has the same type and produces
the same value as ``{x=1.0, y=2.0}``.

This flexible use of anonymous record types and record expressions
would be more difficult in a nominal record system.  Consider the
following two definitions in a hypothetical nominal record system::

  type r1 = {x: bool}
  type r2 = {x: bool}

When we encounter a record expression ``{x=true}``, is the result of
type ``r1`` or ``r2``?  Most languages solve this by requiring type
annotations.  For example, in C, you have to declare the type of a
variable when you introduce it, and hence name clashes between struct
members it not an issue.  In OCaml and Haskell, record labels are
scoped: only one ``x`` field label can exist at a time.  In the case
above, the ``x`` from ``r1`` would be shadowed by the definition of
``r2``, and hence ``{x=true}`` would have type ``r2``.  A common
consequence is that programmers assign the labels of a type a unique
prefix::

  type r1 = {r1_x: bool}
  type r2 = {r2_x: bool}

The need for such prefixes is one of the most common criticisms levied
at the Haskell record system (although `work is ongoing`_ on fixing
it).  Altogether, for Futhark, we found the Standard ML approach
simpler and more elegant.

.. _`work is ongoing`: https://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/

Now that we can both describe record types and create record values,
the next question is how to access the fields of a record, which we
call *field projection*.  In languages descended from C, this is done
with dot-notation: ``p.x`` extracts field ``x`` from the record (well,
struct) ``p``.  Standard ML (but not OCaml) chooses a different
notation, which we have also adopted for Futhark: ``#x p``.  This
notation feels more functional to me.  You can pass it, in curried
form, as the argument to ``map``, to project an array of records to an
array of field values: ``map #x ps``.  This requires an explicit
lambda if done with dot-notation.  However, this quality is a matter
of subjective aesthetic preference (and `Elm can do this`_ with
dot-notation anyway).  A more important reason is ambiguity.  Since
module- and variable names reside in different namespaces, we can have
both a module ``p`` and a variable ``p`` in scope simultaneously.  Is
``p.x`` then a module member access, or a field projection?

Other languages solve this ambiguity in a wealth of different ways.  C
sidesteps the issue by not having modules at all.  C++'s namespaces
use a different symbol (``::``).  Java implements modules as static
class members, which means there is only one namespace, and either the
"record" or the "module" will be in scope.  OCaml makes module names
lexically distinct by mandating that they begin with an uppercase
letter, while variable names must begin with a lowercase letter.
While this latter solution is elegant, I do not wish to impose such
constraints on Futhark (for reasons I will not go into here).  Hence,
we are going with the SML notation: ``#x p`` retrieves field ``x``
from ``p``.

.. _`Elm can do this`: http://elm-lang.org/docs/records

Field projection is not the only way to access the fields of a record.
Just as we can use tuple patterns to take tuples apart, so do we have
*record patterns* for accessing the fields of a record::

  let {x=first, y=second} = p

This binds the variables ``first`` and ``second`` to the ``x`` and
``y`` fields of ``p``.  Instead of just names, ``first`` and
``second`` could also be patterns themselves, permitting further
deconstruction when the fields of a record are themselves records or
tuples.  For now, *all* fields of the record must be mentioned in the
pattern.  As a common-case shortcut, a field name can be listed by
itself, to bind a variable by the same name::

  let {x,y} = p
  -- same as
  let {x=x,y=y} = p

Record patterns can of course also appear as function parameters,
although type annotations are necessary due to limitations in the type
inference capabilities of the Futhark compiler::

  let add_points {x1:f64, y1:f64} {x2:f64, y2:f64} =
    {x = x1 + x2, y = y1 + y2}

Record Updates
--------------

When working with records, it is frequently useful to change just one
field of a record, while leaving the others intact.  Using the
constructs seen so far, this can be done by taking apart the record in
a record pattern (or using projection), and constructing a new one::

  let incr_x {x;f64, y:f64} =
    {x = x+1.0, y = y}

This works fine for small records, but quickly becomes unwieldy once
the number of fields increases.  OCaml supports a ``with`` construct
for this purpose: ``{p with x = p.x+1.0}`` (using OCaml's dot notation
for field access).  This works fine, and would also function in
Futhark, but we opted for a more general construct instead.

So far, record expressions have consisted of comma-separated field
assignments.  We extend this notation, so that an arbitrary expression
can occur in place of a field assignment::

  {p, x = #x p}

An expression used like this (here, ``p``) must itself evaluate to a
record.  The fields of that record are added to the record constructed
by the record expression.  For example, we can rewrite ``incr_x``::

  let incr_x (p: {x:f64,y:f64}) =
    {p, x = #x p + 1.0}

Record expressions are evaluated left-to-right, such that if duplicate
fields occur, the *rightmost* one takes precedence.  That means we
could introduce a bug by erroneously writing the above expression as::

  {x = #x p + 1, p}

Since ``p`` already has a field ``x``, the result of the field
assignment will not be included in the resultant record.  This error
is easy to make, but fortunately also easy to detect and warn about in
a compiler.

These extended record expressions are not just for record updates, but
perform general *record concatenation*.  For any two records ``r1``
and ``r2``, the record expression ``{r1,r2}`` produces a record whose
fields are the union of the fields in ``r1`` and ``r2`` (the latter
taking precedence).

We do not yet know which programming techniques are enabled by this
capability, but we are looking forward to finding out.  It seems
likely that we will eventually add facilities for partial record
patterns (only extracting a subset of fields), as well as some
facility for removing fields from records.  We may also adopt some
form of `row polymorphism`_ once the time comes to add full parametric
polymorphism to Futhark.  But that will have to wait for another blog
post.

.. _`row polymorphism`: https://brianmckenna.org/blog/row_polymorphism_isnt_subtyping
