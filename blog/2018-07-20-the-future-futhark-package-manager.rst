---
title: The Future Futhark Package Manager
author: Troels Henriksen
description: It's time to write a package manager for Futhark.
---

There was a time when it was considered crucial for a programming
language to come bundled with a large standard library.  Python is
perhaps the most famous example of this idea of "batteries included".
Unfortunately, this approach ties development of the libraries to the
release schedule of the language implementation (and in particular
makes it impossible to upgrade one without the other), which often
leads to these "standard" libraries falling behind compared to
independently developed libraries.  Another issue is that the standard
libraries typically must be general enough to cover *everyones* needs,
which easily makes them convoluted.  For more, see the post `Where
modules go to die
<http://www.leancrew.com/all-this/2012/04/where-modules-go-to-die/>`_
and its `corresponding discussion on Hacker News
<https://news.ycombinator.com/item?id=3913182>`_.

Instead of having "batteries included", many recent languages instead
contain minimal standard libraries, and instead provide a standard
*package manager* that makes it easy to use third party packages.
`Rust <https://www.rust-lang.org>`_ is perhaps the most prominent
example of this approach.  Since reusable Futhark code is beginning to
crop up, and I'd rather not stuff all of it into `futlib
<https://futhark-lang.org/docs/>`_ itself, it is time to design and
implement a simple package manager for Futhark.

There are lots of things we would like the package manager to do.
Unfortunately, because we live in an imperfect world, there are also
things we cannot expect it to do.

1. **It must be simple to implement.** There are lots of improvements
   to make to Futhark, and we cannot afford to spend too long on
   introducing and fixing bugs in a package manager.  The best way to
   avoid complexity is to avoid features, so we will keep the package
   manager as bare-bones as possible.

2. **It must be simple to use.** `Futhark lives in the desert
   <2018-06-18-designing-a-programming-language-for-the-desert.html>`_,
   and our users are not interested in learning a new complicated
   package manager.  It is better to restrict the flexibility of the
   package manager than to introduce complexity in the user experience.

   This also extends to package authors - ideally, issuing a release
   should be as simple as tagging a revision in a source control
   system.

3. **No user configuration required.** Whatever format a package lists
   its dependencies in must contain enough information to also allow
   the package manager to fetch them.  If you download a Futhark
   program, all you should do is run ``futhark-pkg get`` (or whatever
   the command ends up being), and then you can compile it.  This is
   in contrast to a system like `Smackage
   <https://github.com/standardml/smackage>`_, which requires package
   URLs to be manually added to a user-global configuration file.

4. **The compiler should not be modified.** In particular, the
   compiler does not support *include paths*, so the package manager
   must make the fetched packages available as files in the file
   system, such that they can be directly imported by the program.
   Concretely, I think the package manager will simply put everything
   it fetches in a ``lib/`` sub-directory, such that a program will
   have to say ``import "lib/foo/bar"`` to import file ``bar.fut``
   from the ``foo`` package.  The package manager will *not be a build
   system*.  You will still need to manually invoke the compilers
   after fetching the dependencies.  This creates operational
   simplicity.

5. **No need for a central server.** We do not want more
   infrastructure maintenance burdens.  However, the design should
   permit us to later add such a server as a package registry, as this
   seems likely to be useful for documentation and discoverability.

6. **The package manager must fetch only pre-packaged tarballs.**
   Interfacing directly with version control systems is too complex.
   Fortunately, code hosting sites like GitHub make it trivial to
   generate tarballs corresponding to revision tags.

7. **It need not be possible for a program to simultaneously depend on
   multiple versions of the same package.** Such support might be
   difficult to implement, and is anyway only needed for large
   programs.  Futhark programs are supposed to be small, so this is
   complexity that is not worth it.

8. **"Vendoring" (copying the dependencies directly into your own
   source repository) must still be possible for those who prefer
   that.** Ideally, just by committing the ``lib/`` directory alluded
   to in point 4.

9. **Initially focus on supporting a GitHub-centred workflow**.  This
    is only relevant if it becomes necessary to make a choice about
    what code hosting service to support initially (if a generic
    solution is not possible).  Ultimately it is not appropriate to
    depend exclusively on a centralised proprietary service, but we
    need to start somewhere.

I have never implemented a package manager before, although I have
used a few.  To figure out how to turn the above wish list into a
program, I have been doing some reading.  In particular, I have found
`this series of blog posts by Russ Cox
<https://research.swtch.com/vgo>`_ very interesting.  Russ is also
working on adding a new package manager, called ``vgo``, to the `Go
language <https://golang.org/>`_.  While I am not a big fan of Go as
such, I have a lot of respect for the thoughtfulness that tends to go
into the language and its tools.  For example, `Minimum Version
Selection <https://research.swtch.com/vgo-mvs>`_ seems like an elegant
solution that both manages to avoid complex version resolution, and
provides reproducible builds without the need for a lock file - and
all by preferring to use the *oldest* compatible packages rather than
the *newest*.

Let's look at how ``vgo`` addresses the requirements for Futhark.
Simplicity of use is certainly the case, as ``vgo`` automatically
reads ``import`` statements in the source code and downloads necessary
dependencies before building.  It also does not require any user
configuration, but does use the ``$GOPATH`` directory as sort of a
cache.  ``vgo`` does not *require* a central server, but inherits a
nice scheme from ``go get`` whereby any HTML page can act as a sort of
package proxy by serving `appropriate META tags
<https://golang.org/cmd/go/#hdr-Remote_import_paths>`_.  Previous
package managers for Go interacted directly with the zoo of different
version systems, but Russ Cox `explains how this is far too much
complexity <https://research.swtch.com/vgo-module>`_, so ``vgo``
fetches only tarballs.  In fact, his explanation is why I added this
requirement for the Futhark package manager.  Multiple versions of the
same package are explicitly supported by ``vgo`` via `semantic import
versioning <https://research.swtch.com/vgo-import>`_.  This makes
sense for Go, which must scale to gigantic programs, but less so for
Futhark.  Finally, vendoring is unaffected, and ``vgo`` seems to also
initially support GitHub import paths, and uses the GitHub web API to
fetch tags and tarballs instead of invoking ``git`` directly.  Looks
like a pretty close match for our needs!

The only missing piece is the question about simplicity of
implementation, so I guess the only option is to start writing some
code and see what happens.
