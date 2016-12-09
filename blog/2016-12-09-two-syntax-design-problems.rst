---
title: Two Syntax Design Problems in Futhark and Their Resolution
author: Troels Henriksen
description: A tale of two times we had to put some serious thought into details of the Futhark syntax.
---


Futhark will never be anyone's primary language.  Neither will Futhark
be the primary language in any large system.  As a pure functional
language, any interaction with the outside world is impossible.  As an
array language, many problems are impractical to express directly in
Futhark, unless some preprocessing is performed.  Futhark's intended
niche is implementing performance-sensitive computational kernels,
which are merely part of a larger system.  Two design principles arise
from this basic assumption:

  1. First, the compiler must generate code that is inter-operable with
     other languages.  This is primarily a technical consideration in
     the compiler implementation.

  2. Second, the Futhark *language* must be designed to feel familiar.
     Futhark does not promise wide enough applicability to justify
     learning complicated and arcane notation and semantics.

The second principle is the topic of this post.  I will discuss two
recent design issues in the Futhark syntax, and how they were
resolved.  In one case, adherence to familiar notation was judged
important enough to justify a rather odd lexical rule, while in
another case, no existing notation was judged sufficient for our
purposes.  In both cases, `Fritz Henglein`_ provided invaluable input
that forced rigour in our reasoning.

.. _`Fritz Henglein`: http://www.diku.dk/~henglein/

Futhark is primarily concerned with efficient compilation, and
worrying about syntax may appear as unproductive procrastination.
However, Futhark is not just a research study - it is a language built
for practical use.  As the main user interface to our compiler, the
ergonomics of the language deserve attention.  And more importantly,
language design is *fun*.

Array Types
-----------

The first issue I wish to discuss is the design of Futhark's syntax
for array types.  For a long while, we had simply adopted Haskell's
list type syntax: an array of ``int``s was written ``[int]``, and a
two-dimensional array of ``int``s was written ``[[int]]``.  This was
both familiar and readable.  However, some timer later we extended
Futhark with optional *shape declarations*, by which individual
dimensions of an array could be annotated with a variable or constant
indicating its size.  For example, a function for matrix
multiplication could be defined as::

  fun (x: [[int,m],n]) (y: [[int,p],m] y): [[int,p],n] = ...

This nicely describes the invariant that an *n\*m* matrix can be
multiplied with an *m\*p* matrix, yielding an *n\*p* matrix.  Here,
``m``, ``n``, and ``p`` are in binding position in the parameters.
Shape declarations not only serve as documentation; they also provide
useful hints to the compiler.  In the future, we may even use them to
signal compile-time errors when shapes are mismatched (using
lightweight `dependent types`_), but for now, they are checked at
run-time.

Apart from having an effect on the aesthetics and readability of code,
the notation for types is also important when communicating the
semantics of the language.  For example, using this notation for array
types, we can describe the type of ``map`` as::

  map : (a -> b) -> [a,n] -> [b,n]

This type concisely describes that the output array of ``map`` has the
same outer size as the input array, while nothing is said about any
inner sizes (``a`` and ``b`` can be any type, including arrays with
their own shape declarations).

.. _`dependent types`: https://en.wikipedia.org/wiki/Dependent_type

In general, any array type ``[t]`` could receive a shape declaration
as ``[t,n]``, meaning "array of ``n`` values of type ``t``".  The
motivation for placing the shape declaration to the right of the
element type was due to the intuition that such annotations were an
optional "extra".  However, this design turned out to have a major
disadvantage: array types had to be read right-to-left.  For example,
a three-dimensional integer matrix of size *k\*n\*m* would be written
``[[[int,m],n],k]``.  This proved very confusing in practice.

One simple solution would be to simply move the size annotation to the
left of the element type, and perhaps replace the commas with
asterisks.  Our matrix multiplication function would then be::

  fun (x: [n,[m,int]]) (y: [m,[p,int]] y): [n,[p,int]] = ...

Much better!  But while we are mucking about with the syntax anyway,
we might also consider more radical changes.  The Haskell notation
works well when you have only a small number of dimensions, but
becomes unwieldy fast.  How many dimensions does ``[[[[[int]]]]]``
have?  It is also annoying that the element type is hidden all the way
in the middle.  Deeply nested lists of lists are uncommon in Haskell,
but not particularly so in Futhark.

We considered a C-style syntax, such as ``int[k][n][m]``.  This looks
less noisy, but unfortunately composes badly.  When adding a new outer
dimension (or stripping the outermost), we have to make modifications
in the "middle" of the type.  This makes it harder to give type
signatures.  Let us consider ``map`` again::

  map : (a ds -> b ds') -> a[n]ds -> b[n]ds'

Here, ``ds`` and ``ds'`` are some arbitrary (possibly empty) list of
shape declarations.  This is very awkward!  Things are much simpler if
we move the dimensions to the *left* of the element type:
``[k][n][m]int``.  Now, for any type ``t``, an array of ``n`` ``t``s
is just ``[n]t``  The type of ``map`` can be written as::

  map : (a -> b) -> [n]a -> [n]b

This has much cleaner properties of composition, and is the solution
we ended up picking.  But can we simplify further?  As a notational
shortcut we could permit ``[k,n,m]int`` instead of ``[k][n][m]int``.
This saves a few characters, but we judged that it was not worth
having two ways of expressing the same type.  Especially since this
notation is also much less readable when the optional dimension
declarations are elided: compare ``[][][]int`` and ``[,,]int``.

Although the ``[k][n][m]int`` notation looks weird at first glance, it
has been quite comfortable to use in practice.  After a few months of
trials, we have not run into any unfortunate issues.  It is easy to
type, too, as you don't have to move the cursor around too much when
adding or removing dimensions.

Array Indexing
--------------

The second issue appeared much more recently, and I am still unsure
about the elegance of its solution.

In Futhark we support literal array expressions by enclosing the
elements in brackets: ``[1,2,3]``.  We also support array indexing
using the familiar notation of a name followed by indices in brackets:
``a[x]``.  So far, so good.

In its original incarnation, Futhark required parentheses around the
arguments in a function call, like most languages derived from C.
Recently, in order to ease equational reasoning and bring Futhark more
in line with functional language conventions, the application syntax
was changed to use juxtaposition.  For example, ``f(x,y)`` became ``f
x y``.  But a problem arose: how should ``f [x]`` be interpreted?  Is
it an application of the function ``f`` to the array ``[x]``, or an
indexing of the array ``f`` at position ``x``?

While applications of functions to literal arrays are rare in
application code, they are common in tests and examples.  When
demonstrating ``map``, it is convenient to be able to say::

  map (+2) [1,2,3] == [3,4,5]

instead of::

  map (+2) ([1,2,3]) == [3,4,5]

It was therefore not desirable to outright ban literal arrays as
arguments to functions.  Based on how indexing and application is used
in practice, we can construct a disambiguation rule: an index expression may
not have whitespace between the array name and the indices.  Thus,
``f[x]`` is an index expression, and ``f [x]`` is a function application.

While this solves the ambiguity, anyone with parser writing experience
probably feels a little uneasy at this solution.  The reason is that
such whitespace information is typically not available during syntax
analysis, having been removed by the lexer.  While there are good
reasons for using hand-written parsers, the Futhark compiler uses a
conventional ``lex``+``yacc`` setup, in part to ensure that the
grammar remains simple and unambiguous.

The solution we chose is indisputably a lexer hack, but it has been
surprisingly unproblematic.  We simply introduced a new lexeme that
represents a name followed immediately by a bracket.  Thus, ``f[`` is
treated as a single undivided lexeme.  To permit indexing of arbitrary
parenthesises expressions, e.g. ``(f a b)[0]``, we also added ``)[``
as a lexeme.  The resulting grammar productions look a bit weird, but
they are fully unambiguous, which gives us confidence in their
robustness.  However, it means that anywhere else these character
sequences are valid (presently nowhere), we will have to handle these
"conjoined" lexemes.  This is a risk we take in order to support
familiar-looking syntax.

This issue could have been solved in other ways.  For example, we
could have changed array literals to require a prefix, or maybe
another form of brackets.  We could also make radical changes to array
indexing syntax.  Maybe require a field access dot as in F#:
``a.[i]``.  Or perhaps treat arrays as functions and index via
application: ``a i``.  These notations all have advantages and
disadvantages, and we may revisit the issue in the future.  For now,
we have chosen to go with familiarity, at the cost of a parsing hack.
