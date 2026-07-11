---
title: Dot Notation for Records
author: Troels Henriksen
description: Futhark has switched from using an SML-style notation for accessing the fields of records, to more conventional dot notation.  This post discusses why, and the challenges we encountered.
---

When we designed the Futhark `record system`_, we went with a notation
copied from Standard ML.  Specifically, given a record

    let foo = { x = 2, y = 3 }

we would access field ``x`` by writing ``#x foo``.  This differs from
the vast majority of languages (including most others in the ML
family), which use the ubiquitous dot notation: ``foo.x``.  The main
reason for adopting Standard ML's notation was ambiguity, since dot
notation was used for the `module system`_.  Given an expression
``foo.x``, is this a reference to field ``x`` of record ``foo``, or to
definition ``x`` in the module ``foo``?

.. _`record system`: /blog/2017-03-06-futhark-record-system.html
.. _`module system`: /blog/2017-01-25-futhark-module-system.html

`OCaml`_ solves this ambiguity by requiring module names and variable
names to be lexically distinct, specifically by requiring module names
to start with a capital letter.  However, adding naming constraints is
a bit contrary to our `language design philosophy`_, which states that
familiarity and simplicity are key.  However, given the ubiquity of
dot notation in other languages, our choice to adapt Standard ML's
notation is also in conflict with our philosophy.

.. _`language design philosophy`: /blog/2016-09-03-language-design.html
.. _`OCaml`: https://ocaml.org/

The solution, which I believe is also used by `F#`_, is to unify the
value and module name spaces.  The meaning of ``foo.x`` is then always
clear, as it can be resolved by the type of the ``foo`` in scope.
This does not mean that Futhark's grammar becomes context-dependent,
since grammatically a field- and a module access can be treated
equivalently.

.. _`F#`: http://fsharp.org/

Of course, this would not be a blog post if not for an additional
wrinkle.  Futhark supports a range of primitive types, such as ``i32``
(signed 32-bit integers), ``u32`` (unsigned 32-bit integers), ``f32``
(single-precision floats), and so on.  For each of these types, the
`Futhark basis library`_ defines a module that contains various
utility functions.  For example, there is a module ``f32`` that
contains such functions as ``f32.sqrt``, ``f32.min``, and ``f32.cos``.
The problem occurs because Futhark's approach to converting/casting
between primitive values was to define a collection of special
overloaded functions, one for every type, that could convert an
argument of an arbitrary primitive type to the desired type.  For
example, there was a function ``f32`` that could be called with any
primitive value (say, an integer), and would return a corresponding
single-precision float.  With the unification of value and module name
spaces, only one of these would be in scope simultaneously.  This was
a problem, as both the mathematical modules and the conversion
functions were used quite liberally in existing Futhark code.

.. _`Futhark basis library`: https://futhark-lang.org/docs/

Since either the conversion functions or the modules would have to be
renamed, likely with a longer name, we investigated exactly *how*
frequently they were used, based on the premise that the most
frequently used constructs should be given the shortest names.  We
looked at the `Futhark benchmark suite`_, which constitutes the
largest single collection of Futhark code (currently 8000 non-comment
and non-blank lines).  The analysis showed that 205 lines contained
calls to module functions, while 158 lines performed type conversions.
But interestingly, the vast majority of the type conversions were
either from ``i32`` to ``f32`` (83 instances), or the other way (33
instances).  Most of the remaining type conversions involved bit
fiddling in a cryptography benchmark.

.. _`Futhark benchmark suite`: https://github.com/diku-dk/futhark-benchmarks

This inspired a new approach: ditch the overloaded conversion
functions entirely, and replace them with monomorphic versions in the
per-type modules, as well as succinctly named top-level functions for
the most common conversions.  Thus, to convert from, say, ``u8`` to
``i16``, you would now call the function ``u8`` in the ``i16`` module:
``i16.u8``.  The Futhark basis library prelude (which is implicitly
imported by every Futhark program) was augmented with the following
function definitions::

  -- | Create single-precision float from integer.
  let r32 (x: i32): f32 = f32.i32 x
  -- | Create integer from single-precision float.
  let t32 (x: f32): i32 = i32.f32 x

  -- | Create double-precision float from integer.
  let r64 (x: i32): f64 = f64.i32 x
  -- | Create integer from double-precision float.
  let t64 (x: f64): i32 = i32.f64 x

The mnemonics behind the names is that "t" is for "truncate", and "r"
is for "real", as in "real numbers".  The versions for
double-precision floats were mostly added for completeness.

The end result of this whole story is:

  * Futhark records now use conventional dot notation.  Great win!

  * The majority of type conversions are just as concise as before,
    although some now require a module qualification.

  * Some overloaded functions were removed.  Since overloading is
    implemented with compiler magic, and user-defined functions cannot
    be overloaded, it's always nice to have fewer of them.
