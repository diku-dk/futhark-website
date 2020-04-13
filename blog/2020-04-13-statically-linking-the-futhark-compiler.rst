---
title: Statically linking the Futhark compiler
author: Troels Henriksen
description: My experiences with placing statically linked binaries in the Futhark binary releases.
---

For the convenience of users who would rather not install a Haskell
compiler and its associated tooling, we've made precompiled Futhark
binaries `available for years now
<https://futhark-lang.org/releases>`_.  The only platform we provide
releases for is x86-64 Linux, mostly because we lack the CI
infrastructure to build on anything else (and macOS users `should just
install from Homebrew <https://formulae.brew.sh/formula/futhark>`_).
Until recently, the binaries were ordinary dynamic executables.  This
means that the user must have the necessary dependencies already
installed, and we must document what they are.  So what are they?
Let's look at one recent release::

  $ ldd futhark-0.15.3-linux-x86_64/bin/futhark
          linux-vdso.so.1 =>  (0x00007ffc14bfb000)
          libm.so.6 => /lib64/libm.so.6 (0x00007f3928cf2000)
          libz.so.1 => /lib64/libz.so.1 (0x00007f3928adc000)
          libpthread.so.0 => /lib64/libpthread.so.0 (0x00007f39288c0000)
          libtinfo.so.5 => /lib64/libtinfo.so.5 (0x00007f3928696000)
          librt.so.1 => /lib64/librt.so.1 (0x00007f392848e000)
          libutil.so.1 => /lib64/libutil.so.1 (0x00007f392828b000)
          libdl.so.2 => /lib64/libdl.so.2 (0x00007f3928087000)
          libgmp.so.10 => /lib64/libgmp.so.10 (0x00007f3927e0f000)
          libc.so.6 => /lib64/libc.so.6 (0x00007f3927a41000)
          /lib64/ld-linux-x86-64.so.2 (0x00007f3928ff4000)

Most of these dependencies are very standard - the C library, its math
component, the Linux syscall interface, and so on.  The only remotely
unusual parts are ``libtinfo.so.5`` (for terminal information),
``libz.so.`` (unpacking zip files), and ``libgmp.so.10`` (`bignums
<https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic>`_).
All of these are pretty common libraries, but they are not truly
universal, and there have been a few cases where the ``futhark``
binary failed to run because of a missing dependency.  In particular,
different distributions use have installed different versions of
``libgmp.so`` by default.  Similarly, using ``libtinfo.so.5`` instead
of ``libncursesw.so.6`` reflects that this binary was built on a
relatively old system (Ubuntu 14.04, running on Travis).  On newer
systems, this may require users to install a package such as
`ncurses5-compat-libs
<https://aur.archlinux.org/packages/ncurses5-compat-libs/>`_.  the CI
systems is not a solution, because then the binaries might not run on
older systems, and many scientific computing installations tend to run
systems like `RHEL
<https://www.redhat.com/en/technologies/linux-platforms/enterprise-linux>`_,
which can stay unchanged for many years.  What to do?  I got into
compilers so I didn't have to worry about all this systems
administration stuff!

Static linking
--------------

The traditional solution is to use `static linking
<https://en.wikipedia.org/wiki/Static_library>`_, where all library
code is bundled into the executable itself.  This was the traditional
approach before dynamic linking became common on Unix in the late 80s
and early 90s.  Static linking has had a bit of a renaissance
recently, with many newer languages statically linking most
dependencies by default, and only dynamically linking with C
dependencies.  `Go <https://golang.org/>`_ in particular is (in)famous
for static linking.  The motivation is easier deployment, as
executables become self-contained.

Like Rust, Go, and so many others, the `Glasgow Haskell
Compiler <https://www.haskell.org/ghc/>`_ (GHC) links Haskell
dependencies statically and C dependencies dynamically.  GHC can also
link the C dependencies statically, although it can `be a bit involved
<https://ro-che.info/articles/2015-10-26-static-linking-ghc>`_, and
you usually need special packages on the system where you build (but
not where you run) to get the static libraries.  I'd rather not make
that the default for people who compile Futhark on their own.

My solution was to write a `Nix derivation
<https://github.com/diku-dk/futhark/blob/master/default.nix>`_ for
building Futhark release tarballs.  It concisely encapsulates the
environment and dependencies necessary for statically linking the
``futhark`` binary (as well as the man pages, for that matter).  The
result is beautiful::

  $ ldd futhark-nightly-linux-x86_64/bin/futhark
          not a dynamic executable

What about size?  What is the performance impact of statically linking
the C dependencies?  Well::

  $ du -sh futhark-0.15.3-linux-x86_64/bin/futhark
  73M   futhark-0.15.3-linux-x86_64/bin/futhark
  $ du -sh futhark-nightly-linux-x86_64/bin/futhark
  32M   futhark-nightly-linux-x86_64/bin/futhark

The statically linked binary turns out to be half the size.  This is
not a fair comparison, because the build environments are also very
different, but it shows that static linking is not going to make the
``futhark`` binary explode in size (or alternatively that it already
has, since all the Haskell-level dependencies have always been
statically linked).

Problems with static linking
----------------------------

Those who are familiar with static linking may have noticed a problem.
The problem is that the GNU C library, glibc, `does not work real well
with static linking
<https://sourceware.org/glibc/wiki/FAQ#Even_statically_linked_programs_need_some_shared_libraries_which_is_not_acceptable_for_me.__What_can_I_do.3F>`_.
Specifically, the *Name Service Switch*, which among other things
performs DNS lookups, is intrinsically tied to dynamic linking, and
will often not work for statically linked binaries.  While the Futhark
compiler doesn't do a lot of network requests, its package manager
does do a few.  The best solution is likely to use a different C
library that is more friendly to static linking, like `musl
<https://www.musl-libc.org/>`_.  People are `working on building
static Haskell executables with Nix
<https://github.com/nh2/static-haskell-nix>`_, but for now I could not
get it to work.

The solution was to realise that our network needs are extremely
simple.  All we need is to perform a few HTTP GETs on specific URLs
and retrieve the contents as byte sequences.  We could just shell out
to `curl <https://curl.haxx.se/>`_ instead.  `And now we do
<https://github.com/diku-dk/futhark/commit/0bf0fea02abd0a4f3a04a32b8cc9e640c7e35f08>`_.
This also allowed us to remove all Haskell dependencies related to
HTTP requests, which caused a significant decrease in binary size.
The downside is that using the Futhark package manager will require
``curl`` to be installed, but ``curl`` is so common that it will not
be a problem.  It also saves us from having to worry about managing a
certificate store in order to validate HTTPS signatures or whatnot.
Again, not why I got into compilers.  If ``curl`` works, then
``futhark pkg`` will work.  It will even use the default ``curl``
configuration file, so the users of weird networks can put whatever
they want there to make it work.
