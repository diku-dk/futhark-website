---
title: Attributes in Futhark
author: Troels Henriksen
description: A design rationale for Futhark's <em>attributes</em>, a mechanism for attaching freeform metadata to expressions.
---

Many programming languages support some form of `annotations
<https://docs.oracle.com/javase/tutorial/java/annotations/>`_,
`decorators <https://realpython.com/primer-on-python-decorators/>`_,
or `attributes
<https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/attributes/>`_,
by which the programmer can attach metadata to parts of the program.
Some, like Python's decorators, can actually modify the semantics of
the program, usually by triggering some kind of code generator.  I
have always had a strong aversion to these, as they feel like a
half-baked solution to a problem better addressed by Lisp-style
macros, a proper language extension, or the blunt hammer of fully
independent pre-compilation code generation.

Other kinds of *attributes* (the term I will use for the rest of this
post) are more benign, because they do not affect the semantics of the
program itself, but merely have effects such as locally disabling
compiler warnings, provide test data for testing tools, or provide
hints for optimisation.  The key property is that tools (and humans!)
can ignore these attributes without grossly misinterpreting the
program.

In this post I will describe the attribute facility that we included
in `version 0.15.8 of Futhark
<https://github.com/diku-dk/futhark/releases/tag/v0.15.8>`_.  For
years, I resisted adding such a facility out of the usual `desire for
simplicity and automation
<https://futhark-lang.org/blog/2018-06-18-designing-a-programming-language-for-the-desert.html>`_.
However, I have finally accepted that there are some cases where a few
programmer-provided attributes can significantly reduce the burden on
the compiler, and in particular shorten compile times.

Attributes have some tricky interactions with compiler optimisations,
and we will get to those, but let us first look at some of the
motivating use cases for attributes in Futhark.

Restricting multi-versioning
----------------------------

Futhark supports nested parallelism, meaning that higher-order
parallel operations can themselves contain more parallel operations.
When we compile an expression ``map f xs``, we are then faced with a
choice: do we exploit the parallelism in the definition of ``f`` (if
any), or do we merely parallelise the outer ``map`` on its own, and
run any parallel operations inside ``f`` sequentially?  The dilemma is
that all parallelism carries *overhead*, so if the array ``xs`` is big
enough, exploiting nested parallelism is not optimal.  However, if
``xs``, is small, then we may need the parallelism in ``f`` to
saturate the machine!  The Futhark compiler handles this by generating
code that contains *two different semantically identical but
operationally different compilations of the expression*, with a
conditional to choose at run-time which one to use:

.. code-block:: Futhark

   if length xs > threshold
   then map f_sequential xs
   else map f_parallel xs

This technique is called `incremental flattening
<https://futhark-lang.org/blog/2019-02-18-futhark-at-ppopp.html>`_ and
is one of the most novel parts of the Futhark compiler.  But it comes
at a cost: the program may grow significantly, since code is
duplicated for every level of nested parallelism.  The main
consequence of this is a noticeable increase in compile times.  In
some cases, this is unavoidable: after all, incremental flattening is
motivated exactly by the observation that different inputs need
different programs, and *somebody* needs to compile those programs!
But in other cases, we as programmers have knowledge of the input that
the compiler does not.  Perhaps we know that in practice, the ``xs``
array will *never* be particularly large, and so nested parallelism
should *always* be exploited.  There is then no reason to waste time
compiling the partially sequentialised code version.

This problem can be solved safely with attributes, since the whole
point of incremental flattening is to generate multiple semantically
equivalent code versions.  Pruning some of these versions cannot
possibly affect the result of the program, but merely affect
compile-time or run-time performance.

Another potential use case is inhibiting `inlining
<https://en.wikipedia.org/wiki/Inline_expansion>`_, which is another
semantically transparent transformation that can nevertheless have
significant impact on compiler and program performance.  As I'm
writing this, the Futhark compiler will aggressively inline *all*
functions.  This is partially because the vast majority of
optimisations do not work across function calls, but also because
functions are tricky business on a GPU if they require memory
allocation.

While there is good reason to inline most functions in a Futhark
program, large programs sometimes contain a few functions with large
definitions that are used in just a few places, near the root of the
call tree.  For example, a ray tracer may have a main rendering
function, which is called from a handful of different entry points.
Inlining such functions does not provide meaningful opportunity for
optimisation, but does bloat the program.  Ideally the compiler should
figure out on its own where inlining is worthwhile, but this is hard.
Asking the user to add an attribute to those few functions that should
*not* be inlined is very easy (and doesn't prevent us from making the
compiler smarter in the future).

A final potential use case is indicating to the compiler how you
*expect* some expression to be compiled, and have the compiler issue a
warning when that expectation is not met.  For example, Futhark is
able to bounds-check all array accesses dynamically (and I need to
write a blog post about that), as well as discharging some of those
checks statically.  While dynamic checking is not *very* expensive, it
still has a cost, and for certain innermost loops, we may want the
compiler to inform us if not all safety checks can be discharged at
compile-time.  We can then read the code very carefully to manually
confirm its correctness (because of course programmers can judge the
correctness of a program just by reading it), and then turn off
run-time checking for the expression in question.

Oh, *that's* the final example of attributes: disabling dynamic checks
locally for some expression.

Design of user-visible attributes
---------------------------------

As usual when making language design decisions, `I stole from Rust
<https://doc.rust-lang.org/reference/attributes.html>`_.  An attribute
is written as ``#[attr]``, where *attr* in Rust is a somewhat
complicated thing (a "token tree").  In Futhark, we are going with a
simplar grammar, where an attribute is essentially a `Prolog term
<http://www.dai.ed.ac.uk/groups/ssp/bookpages/quickprolog/node5.html>`_::

  attr ::=    id
            | id "(" [attr ("," attr)*] ")"

The idea is that most attributes will be just a single atom,
e.g. ``#[sequential]``, while the compound form can be used to encode
more complex cases.  For example, ``#[if_opencl(sequential)]`` might
be translated to just ``#[sequential]`` in the compiler frontend when
compiling to OpenCL, and otherwise removed.

One notable aspect of the design is that unknown attributes are
attached to the program, but otherwise *ignored* by the compiler. This
has the rather obvious disadvantage that typos are not detected.
However, it is crucial to the usability of attributes - it would not
do if we had to modify the compiler whenever some other tool, say a
documentation generator, wanted bespoke attributes for its own
purposes.

In Rust, attributes can be attached to definitions and statements (and
potentially other things - it's not entirely clear to me).  In
Futhark, attributes can be prefixed to expressions, declarations, and
module type specs.  I'll only talk about expression attributes for
this post, as they are the most interesting.  They look like this:

.. code-block:: Futhark

   #[incremental_flattening(only_inner)]
   map f xs

This indicates to the compiler that we wish to generate only the code
version where parallelism inside of ``f`` is also exploited.

One significant limitation of the attribute system is that it does not
permit references to variables.  For example, we might have wished to
be able to hint to the compiler that some function parameter will
always have a "small" value, which is useful information to the
optimiser.  It could be written like this:

.. code-block:: Futhark

   let f (x: i32) (y: i32) =
     #[small(x)]
     ...

But how will the compiler know that this ``x`` is supposed to refer to
a variable in scope, and is not just some atom like ``small``?  The
compiler frequently renames and moves variables during compilation, so
we cannot depend on that ``x`` remaining stable, which means that the
name in the attribute will quickly stop referring to any variable in
scope.  I think the eventual solution will involve expanding the
attribute notation with syntax for denoting variables in scope,
perhaps with something like ``#[small(!x)]``.  But that is for the
future.

Attributes in the intermediate representation
---------------------------------------------

So far I have only talked about attributes as they appear in the
user-visible source language.  Equally important, and much more
tricky, is how they appear and are preserved in the intermediate
representation (IR) as the program is rewritten and optimised by the
compiler.  Indeed, many of the most important attributes affect
compiler passes near the end of the compilation pipeline.  It wouldn't
do for the attributes to be removed by intermediate passes.

As a starting challenge, Futhark internally represents the program in
`Administrative Normal Form (ANF)
<https://en.wikipedia.org/wiki/A-normal_form>`_.  This essentially
means that functions are chains of ``let`` bindings, and that
subexpressions are only allowed to be constants and variables.  I'll
stray from strict ANF in some of the examples to come for readability,
but the compiler can do no such thing. So how are attributes on source
language expressions maintained when translating to the ANF IR?

This challenge is relatively straightforward to overcome - simply make
room for an attribute set in the representation of a ``let`` binding
(which apply to *only* that binding, and not later ones in the same
sequence), and take care to insert them when normalising the program.
This does raise some questions, however.  Should the source language
expression ``#[foo] ((x + y) + z)`` normalise to

.. code-block:: Futhark

   let tmp = x + y
   #[foo]
   let z = tmp + z
   in z

or to

.. code-block:: Futhark

   #[foo]
   let tmp = x + y
   #[foo]
   let z = tmp + z
   in z

That is, should attributes be propagated to *every* binding that
results from normalisation, or only the final one?  We currently do
the former, since the IR bindings all seem like equal components of
the original source expression.

Another question is what to do when applying copy propagation,
constant folding, and similar optimisations on the IR.  Consider the
simplification of the following program fragment, where the first
``let``-binding has an attribute:

.. code-block:: Futhark

   #[foo]
   let y = x
   let z = y

Copy propagation should simplify this to just ``let z = x``, but
should the resulting ``let``-binding also inherit the attribute
``#[foo]``?  For now, the answer is *no*, because the binding of ``y``
is getting removed entirely, rather than being "combined" with the one
for ``z``.  While it is conceptually easy to propagate attributes
virally, I think there is a risk of propagating them further than the
programmer originally intended.  I would rather err on the side of
caution, since the programmer can always insert *more* attributes in
the original source program, while it is not possible to constrain the
optimiser's rules on how they are propagated.

Consider now this case:

.. code-block:: Futhark

   #[foo]
   let (xs, ys) = map (\a -> (a+2, y)) as

(Note that in the compiler IR, ``map`` implicitly "unzips" results
that are arrays of tuples.)  The compiler notes that the ``ys`` result
is actually invariant to the ``as`` array, and that it can thus be
factored out of the ``map`` and into a separate (and simpler)
``replicate`` operation:

.. code-block:: Futhark

   #[foo]
   let xs = map (\a -> a+2) as
   let ys = replicate (length as) y

We preserve the attribute on the ``map`` binding, but not on the
``replicate``.  The motivation is that the new ``map`` is a modified
version of the original ``map`` (which had the attribute), while the
``replicate`` is an entirely new thing.  I must admit to having a bad
feeling about this one, though.

When performing `loop fusion
<https://en.wikipedia.org/wiki/Loop_fission_and_fusion>`_, we take the union of the attributes of the fused operations.  That is,

.. code-block:: Futhark

   #[foo]
   let ys = map f xs
   #[bar]
   let zs = map g ys

becomes

.. code-block:: Futhark

   #[foo]
   #[bar]
   let zs = map (g ∘ f) xs

Most simplifications and optimisations have not proven too tricky to
augment with an understanding of attributes.  The really nasty case
has been *inlining*.  When we wish to inline an application

.. code-block:: Futhark

   #[foo]
   let y = f x

what do we do about the attribute?  We cannot simply ignore it, as
many interesting attributes (such as
``incremental_flattening(only_inner)``) are *always* applied to
functions such as ``map``, with the intent to *actually* affect the
compiler intrinsic used to define the user-visible ``map`` function.

Perhaps we can go the other way and propagate the attribute on the
function call to *every* expression in the inlined function
definition?  But this can also cause problems.  Consider the attribute
``sequential_outer``, which asks for the *outermost* level of
parallelism in the expression to be sequentualised, but *preserve all
inner parallelism*.  If we have a function definition

.. code-block:: Futhark

   let f arg =
     map (\r -> map f r) arg

and an application

.. code-block:: Futhark

   #[sequentialise_outer]
   let y = f x

then inlining with full attribute propagation will produce the following:

.. code-block:: Futhark

   #[sequentialise_outer]
   let arg = x
   #[sequentialise_outer]
   let y = map (\r -> #[sequentialise_outer] map f r) arg

This clutters bindings with attributes that will have no effect, which
is annoying when you read the IR produced by the compiler.  Worse, we
have propagated the attribute in such a way that it will now cause
*all* parallelism to be sequentialised.  Hardly what we wanted!  For
this attribute, we want to propagate it to all top-level parallel
statements in the inlined function body, but *not* inside lambda
definitions.

Unfortunately, it seems that there is no general principle for how to
propagate attributes when inlining.  In the Futhark compiler, I have
had to teach the inliner about the different attributes and how they
should be propagated.  The number of attributes that affect
optimisation and code generation fortunately remains fairly low, so an
ad-hoc approach is not too onerous.  However, I expect this is
something we will have to revisit in the future.

Revisiting unknown attributes
-----------------------------

As discussed earlier, the compiler accepts all unknown attributes
without error or warning.  The downside of this is rather obvious,
since attributes by design have no semantic effect, and so it is a bit
hard to test whether you made a typo somewhere.

I am not yet sure how to improve the situation.  Maybe a non-default
compiler mode where it warns about unknown attributes?  I think I will
wait and see how often I and others screw this up in practice before
looking for a solution.
