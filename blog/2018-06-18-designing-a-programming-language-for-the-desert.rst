---
title: Designing a Programming Language for the Desert
author: Troels Henriksen
description: The design of Futhark and its assorted tools is driven by constraints based on its narrow niche.  This blog post explains some of them.
---

Futhark will never be a popular language.  Futhark is intended for a
niche - developing the small performance-critical core of larger
programs - that few programmers ever need to touch.  And even for
those programmers who do, it will likely not be the majority of their
programming.  Thus, even were Futhark to become the indisputably best
language in its area - which is certainly the goal! - the user base
would still be small, and so would the development resources.  This
post explains some of the design decisions we have made in this
context.  The overall idea is a bit like trying to thrive in the
desert - you can do it, even indefinitely, but you have to respect the
realities of the environment, in particular with respect to not
overextending or exposing yourself.  Fair warning, however: everything
I know about desert survival comes from reading `Dune
<https://en.wikipedia.org/wiki/Dune_(novel)>`_.  Fairly sure you don't
have to watch out for sandworms in a real desert.

Limitations on the Compiler
---------------------------

We can probably assume that the Futhark development team will always
remain fairly small (although we certainly hope to grow it, and we
currently invite both open source contributions as well as hire PhD
students - but that's a story for another day).  As a result, we have
a strong focus on keeping the compiler maintainable, in particular by
not adding too many maintenance-heavy features that increase our
exposure to external factors.  For example, it is easy to add new
compiler backends to Futhark.  We have exploited this to add a very
nice `python backend </blog/2016-04-15-futhark-and-pyopencl.html>`_ in
addition to the one for C, and we are currently working on a similar
one for C#.  It would be a relatively small matter to add similar
backends for R, Ruby, Java, Swift, Rust, and so forth.  Yet for each
of these backends we would have to set up testing and benchmarking
infrastructure to ensure they remain operational, even in the face of
changes in the library landscape of the target landscape (for example,
defects or modifications made to way we access OpenCL).  Over-extension
here would result in either spending all our resources on maintenance
of perhaps rarely used features, or else simply producing a buggy
compiler.

Another example of our design philosophy is the small number of
configuration options exposed by the Futhark compiler.  Compare the
``futhark-opencl`` `manpage
<http://futhark.readthedocs.io/en/latest/man/futhark-opencl.html>`_
with the `one for GCC <https://linux.die.net/man/1/gcc>`_.  Also, none
of the ``futhark-opencl`` options affect the compilation process
itself.  This is partially a matter of aesthetics (I like programs
that just *do the right thing*), but also a pragmatic engineering
decision.  Each configuration option multiplies the number of distinct
paths through the compiler, and hence the number of places bugs may
manifest themselves.  A less configurable optimiser is simpler, and
therefore easier to maintain.

Limitations on Users
--------------------

The limitations brought about by limited compiler development
resources are fairly obvious.  The design choices made to accommodate
limited *user* resources are more subtle.  It is a subject for which I
have not been able to find much prior coverage.  Most new languages
are general-purpose, and while they start out obscure, they are often
built with the hope (or fantasy) of growing large and successful, and
being used for full systems.  In a way, they are `temporarily
embarrassed millionaires
<http://www.temporarilyembarrassedmillionaires.org/>`_, and will
prosper if the right investments and risks are made.  Their context is
a famine, which will pass.  But Futhark must prosper in the desert,
and the desert is eternal.

As a starting point, we probably cannot assume that Futhark will be
the primary language of most of its users, and so we must limit the
amount of pitfalls and non-obvious behaviour (the `principle of least
astonishment
<https://en.wikipedia.org/wiki/Principle_of_least_astonishment>`_).
In the desert, resources must not be wasted.  However, this does *not*
mean that we are primarily concerned with novice programmers.  In
fact, we can assume that that only programmers of above average skill
and inclination will want to use Futhark.  Further, effective use of
Futhark will still require users to think about parallel programming,
which is by itself a rare skill (albeit easily learned).  We also do
not compromise on language design decisions necessary to obtain good
performance, since that's the entire point of a language like Futhark
in the first place.

Fortunately, we are well placed in this regard.  One of the original
design principles behind Futhark was that parallel functional
programming should not depend on exotic novel language constructs, but
rather arise naturally from the conventional functional vocabulary of
higher-order functions (``map``, ``reduce``, etc).  The complexity
should be in the compiler, not in the language.  This means that prior
experience in a common functional language is easily translated to
knowledge of Futhark, although some of the restrictions and quirks
`must of course be learnt
<http://futhark.readthedocs.io/en/latest/versus-other-languages.html>`_.

As Futhark is not suitable for full application programming, a Futhark
program will usually be a guest in a larger code base.  Polite guests
do not rearrange the furniture of their host, and similarly, Futhark
should not make arduous demands of whatever build system is already in
place.  Therefore, Futhark has been designed such that compilation is
always done simply by launching the compiler on the Futhark file that
contains the entry points.  There is no need for fiddling with include
paths (none are supported), or interacting with some Futhark-specific
build system or project configuration file.  There are no hidden
caches either: the only file(s) created are in the directory
containing the output file.  This means there is no hidden state or
database that can be corrupted, or must be periodically cleaned.

This notion of zero-configurability is not some "obviously desirable"
property whose inclusion should be a no-brainer in any language.  It
requires a real trade-off in flexibility and ergonomics - a trade-off
with costs that we believe are worth it for Futhark, but are likely too
steep for other languages.

As an example, let us consider how Futhark splits programs over
multiple files.  A file ``foo.fut`` can refer to definitions
``bar.fut`` by saying ``import "bar"``.  The imported file is found
*relative to the path of the importing file*.  For example, if the
file ``dir1/dir2/foo.fut`` contains ``import "bar"``, the compiler
will look for ``dir1/dir2/bar.fut``, and ``import "../bar"`` would be
``dir1/bar.fut``.

This is an unusual design for modern languages (except for scripting
languages), where imports are usually resolved relative to some
*import path* that is the same for all files.  For example, Haskell
uses absolute imports and assumes that the module ``Foo.Bar.Baz`` is
found in a file ``Foo/Bar/Baz.hs``, relative to an import path that
generally does not contain the directory containing the importing file
(but usually contains either the working directory of the compiler, or
the directory of some root file).  The advantage of absolute names is
that a file will always be referenced by the *same* name, no matter
where it is imported.  With Futhark's relative imports, the programmer
has to construct a path to the desired file.  This can make it harder
to understand very large programs, especially if it contains multiple
files with the same base name.

One advantage of relative importing is conceptual simplicity.
Understanding imports requires nothing more than understanding a
hierarchical file system in the first place.  This means there is less
to learn.  However, the main advantage is probably *composability*.
If a Futhark program type-checks, then any file making up that program
can also be type checked *by itself*, simply by passing it directly to
the compiler.  The practical impact is that it becomes low-effort to
write and use tools.  For example, the `flycheck
<http://www.flycheck.org>`_ definition in `futhark-mode
<https://github.com/diku-dk/futhark>`_ is less than twenty lines of
code, and will work *automatically*, with no need to read
configuration files or guess some "compilation root" or whatnot.  C
resembles this design a bit, since ``#include "foo.h"`` is relative,
but in practice it is common to use the ``-I`` option to the compiler
to expand the include path to some "root", and treat imports as
absolute.

A major downside of relative imports combined with no include path
configuration, occurs when using third party libraries.  The library
files must be *physically present* on the file system relatively close
to the user's program.  However, due to how relative imports work, a
library ``mylib`` can be incorporated simply by copying its source
files into a ``mylib`` directory next to the user program, and then
using ``import "mylib/foo"``.  We hope that in the future some
automated tooling can be built on top of this idea, to automatically
fetch and update the code from some central package database, but it
remains unclear exactly how it will work.  Also, it is not yet clear
how to write Futhark libraries that depend on *other* libraries, short
of doing vendoring in the `Go sense of the word
<https://codeengineered.com/blog/2015/go-should-i-vendor/>`_
(basically, bundle whatever libraries you need), which comes with
various serious downsides (particularly code bloat), as can be
observed in the Javascript ecosystem.

Summing up
----------

Futhark is not a deer browsing in a lush forest with nary a care in
the world.  Rather, Futhark is a grumpy `desert hedgehog
<https://en.wikipedia.org/wiki/Desert_hedgehog>`_, bristling with
spikes, thriving even under the limitations of its harsh environment,
and spending its nights `hunting scorpions
<https://www.youtube.com/watch?v=tEznitJXiM0>`_.
