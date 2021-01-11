---
title: Design decisions I do not regret
author: Troels Henriksen
description: Not everything in Futhark is bad or dubious, and here's some examples.
---

I have written several posts on `things that we got wrong in Futhark
<2019-12-18-design-flaws-in-futhark.html>`_, `why the compiler goes
wrong <2018-12-08-why-futhark-sometimes-goes-wrong.html>`_, as well as
posts about `complicated features
<https://futhark-lang.org/blog/2020-03-15-futhark-0.15.1-released.html#size-types>`_
where it is still unclear whether the feature works well enough.
However, there are also some parts of Futhark's design that I feel
pretty good about!  This post will discuss some of them.  I will focus
on very concrete details, rather than grand observations such as
"bulk-parallel functional programming is a good paradigm for numerical
programming".  Such doctrinal statements are more of an entire
*thesis*, and require much richer and more qualified answers than just
"this turned out pretty well".  Everything to follow is what I don't
*regret*, so it is necessarily subjective, but maybe other language
designers will find it useful or amusing.

Short names
-----------

Naming things is one of the two most difficult problems in computer
science, and in contrast to the other two (cache invalidation and
off-by-one errors), there is no tooling or theory to tell you when you
pick a bad name.  Matters are even graver when it comes to names that
are part of public interfaces.  You may well receive acclaim for
fixing a subtle cache invalidation bug in old and widely used code,
but do not expect similar praise if you also change the naming
convention of the public API from underscores to CamelCasing along the
way.

Futhark exposes many names, both in its command line interface and its
`built-in libraries <https://futhark-lang.org/docs/prelude/>`_.  Many
of the names have been changed over the years.  When we initially
fleshed out the primitive type system, I agonised over how to name the
new types.  Should 32-bit signed integers be ``Int32`` as in Haskell,
or ``i32`` as in Rust?  I worried the latter was *too short*.  Yet in
the end, that was the scheme we went with:
``i8``/``i16``/``i32``/``i64`` for the signed types,
``u8``/``u16``/``u32``/``u64`` for the unsigned, and ``f32``/``f64``
for floats.

Incidentally, here is a piece of advice: if you are ever agonising
over some design detail that is not core to what makes your language
special, and all options seem equally reasonable, just go with
whatever Rust does.  Odds are that whatever Rust decided was preceded
by significant debate, and that whatever they picked has few gotchas.
Lexical syntax is one area where this is particularly good (and easy)
advice to follow.

But back to naming things.  The terse type names turned out to work
fine.  In fact, I realised that *I have never regretted making a name
too short*.  The names I still regret tend to be those that are too
imprecise or *too long* - the ``negate`` function in the ``f32`` and
``f64`` modules should have been ``neg``, for example.  I am not quite
sure why this is.  When I code in Haskell, I have a preference for
relatively verbose names for top-level definitions.  In Futhark, short
names just seem to fit better with the language aesthetics.  Maybe
that is just the nature of numerical code?  I don't know.  But I
definitely rely on this observation to guide future naming of things.

File orientation
----------------

While Futhark is not designed for large programs, we still want to
support multi-file programs.  Code in a file ``A`` must be allowed to
reference definitions in some other file ``B``.  Languages differ
significantly in how they go about this.  Futhark went for a
particularly simple model: the declaration ``import "foo"`` allows
access to the definitions in the file ``foo.fut``, looked up relative
to the importing file.  That is all there is to it.  If you know how a
hierarchical file system works, then you know how Futhark looks up
files.  This is not textual inclusion as in C - each file still has to
be syntactically valid on its own.  We are not barbarians.

`The philosophical background is spelled out here
<2018-06-18-designing-a-programming-language-for-the-desert.html>`_,
but the basic rationale is that *nobody enjoys learning about language
import mechanics*, and in Futhark we can get away with doing something
stupidly simple.  Other languages, like Haskell and Java, put more
effort into decoupling the notion of a "module" or "compilation unit"
from that of a physical file.  Even languages that reason in terms of
files usually also provide support for a "search path", where you
provide the compiler with additional directories that are searched
when including files.

The downside of this approach is that for larger programs with
multiple directories of source files, a file has no unique way to be
referenced (except for its absolute pathname, which is not practical),
so you may need to use different relative paths in different files
(e.g. ``"../lib/foo"`` and ``"../../lib/foo"``).  Another downside is
that you cannot have a central repository of "system files" that are
always checked (like ``/usr/include`` in Unix).

A *major* advantage of the design is that each file in a program can
be processed as the "root", without having to look for some kind of
project definition file that contains necessary compiler directives
such as include paths.  This has allowed very lightweight zero-config
tooling, such as a "go to definition" command in `futhark-mode
<https://github.com/diku-dk/futhark-mode>`_ and showing the types of
variables - something that most Emacs modes definitely do not have
without additional configuration.

Clearly this solution wouldn't work for all languages, but it has
worked really well for Futhark.  I was initially worried because this
design is much more similar to *bad* languages (like shell script)
than to *good* languages (like Haskell), but it hasn't caused problems
yet, and I don't recall any Futhark programmers ever being confused
about how ``import`` works.  This is the right design for Futhark, and
I don't expect to regret it any time soon.

No block comments
-----------------

`These remain a bad idea
<2017-10-10-block-comments-are-a-bad-idea.html>`_.  Emacs has ``M-x
comment-region`` and inferior editors likely have something similar.
No regrets.  In fact, my experience writing more `semi-syntactic
tooling <2019-08-31-beginning-a-collection-of-futhark-examples.html>`_
has only strengthened my conviction.  Line comments are easy to parse
in an ad-hoc way, and it is particularly nice that there is only one
way of writing comments.

Explicit binding
----------------

Suppose we have a record type:

.. code-block:: futhark

  type t = {foo: i32, bar: bool}

In a function definition we can bind variables ``a``, ``b`` to the
field contents as follows:

.. code-block:: futhark

   let f ({foo=a, bar=b}: t) = ...

And if we wish to bind variables with the same names as the fields,
there is a shorthand syntax inspired by OCaml and Haskell:

.. code-block:: futhark

   let f ({foo, bar}: t) = ...

These languages also support an even more concise form:

.. code-block:: futhark

   let f ({..}: t) = ...

This implicitly binds variables with the same names as the record
fields.  Futhark does *not* support this.  The reason is a philosophy
of never bringing names into scope unless those names are actually
syntactically present at the binding site in some form.  There are
many things I enjoy about programming, but guessing where some
variable comes from is not among them.  I find C++'s policy of
implicitly bringing class fields into scope inside method bodies to be
particularly confusing.

My distaste for complex binding structure goes further: Futhark
requires that all definitions occur *before* their first use.
Originally this was just for implementation simplicity, but I have
come to enjoy the restriction.  It's nice not to worry so much about
the best definition order, because the compiler already ruled out most
options.  OCaml and SML have similar restrictions, so this is not
*just* Futhark aping some 70s Pascal dialect.

The one exception to this explicitness principle is the ``open``
statement, which brings names from another module into scope.  While a
plain ``open`` is rare, it is used implicitly by ``import``.  There
are also *local open* expressions, which allow us to write
e.g. ``M.(x*y+z)`` with the names in module ``M`` in scope within the
parenthesised expression.  This is *really* convenient when writing
advanced modularised numerical code, as the alternative is the much
more clumsy ``x M.* y M.+ z``.  I am not sure how to reconcile this
with our general principle, but maybe you just can't be a
fundamentalist all the time.

No dependencies
---------------

Futhark is written in Haskell, and all of its code dependencies are
either pure Haskell libraries or embed small amounts of C code.  This
means that it is quite easy to build the Futhark compiler - all you
need is a standard Haskell build tool like ``stack`` or ``cabal``,
which is supported and documented on most operating systems.  I do not
recall anyone having trouble installing the compiler itself.
Difficulties tend to be more in the direction of setting up GPU
drivers on Linux (yet another of the great unsolved problems in
computer science).

This policy is not all upside.  For many years, we lugged around a
home-brewed implementation of algebraic simplification because we were
averse to native code dependencies.  We also generate C code (compiled
with the system C compiler) rather than interacting directly with LLVM
the way `Accelerate <http://www.acceleratehs.org/>`_ does it.  This
means that there are some things we cannot efficiently express in our
generated code.  On the other hand, Accelerate's dependency on LLVM
can make it tricky to install, unless your system happens to have the
right version of LLVM installed (and HPC systems and servers running
*mature* versions of Red Hat Enterprise Linux tend not to).  `ISPC
<https://ispc.github.io/>`_ provides compiler binaries that embed a
statically linked version of LLVM - this *may* be a direction we could
go in, but I suspect statically linking LLVM with a Haskell program is
more tricky than linking it with C++.  And frankly, I think I already
spend too much time hacking on build systems and CI setups.

We have gone quite far with this focus on installation simplicity.
For example, the `Futhark package manager
<https://futhark.readthedocs.io/en/latest/man/futhark-pkg.html>`_ does
not even use native code to download packages.  Instead, it shells out
to ``curl``.  This has allowed us to statically link the Futhark
binary that we provide for releases, a well as not having to worry
about certificate store configuration.  If you can download with
``curl`` in the command line, then ``futhark pkg`` will work.  And if
your system requires you to configure a special proxy or whatnot, then
whichever configuration files or environment variables that ``curl``
respects will also work for ``futhark pkg``.

I was initially hesitant about this approach, as I was always taught
that shelling out is ugly, but it has worked smoothly for us so far.
