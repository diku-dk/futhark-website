---
title: The Present Futhark Package Manager
author: Troels Henriksen
description: We wrote a package manager for Futhark.
---

Recently I `wrote a post
<2018-07-20-the-future-futhark-package-manager.html>`_ about the need
for a package manager for Futhark, and the requirements we have for
it.  I concluded that of all existing package managers, `vgo
<https://research.swtch.com/vgo>`_ comes closest to meeting our needs,
so I set about writing a ``vgo``-inspired package manager.  This work
is now done, and `futhark-pkg
<http://futhark.readthedocs.io/en/latest/man/futhark-pkg.html>`_ is
the result - which validates the goal that the design should be simple
to implement.  In this post, I will show some examples of working with
``futhark-pkg``.  It has been an important objective that the package
manager is operationally transparent, so I will also show exactly what
modifications it makes to the file system.  Of course, this post does
not contain the `full package manager documentation
<http://futhark.readthedocs.io/en/latest/package-management.html>`_,
but it should be enough to get an idea of how it works in the common
case.

Basic Concepts
--------------

A package is uniquely identified with a *package path*, which is
similar to a URL, except without a protocol.  At the moment, package
paths are always links to Git repositories hosted on GitHub.  In the
future, this will become more flexible.  As an example, a package path
may be ``github.com/athas/fibs``.

Packages are versioned with `semantic version numbers
<https://semver.org/>`_ of the form ``X.Y.Z``.  Whenever versions are
indicated, all three digits must always be given (that is, ``1.0`` is
not a valid shorthand for ``1.0.0``).

Most ``futhark-pkg`` operations involve reading and writing a *package
manifest*, which is always stored in a file called ``futhark.pkg``.
The ``futhark.pkg`` file is human-editable, but is in day-to-day use
mainly modified by ``futhark-pkg`` automatically.

Using Packages
--------------

Dependencies can be added by using ``futhark-pkg add``, for example::

  $ futhark-pkg add github.com/athas/fibs

Note that there is no need to create any kind of package metadata file
in advance to tell ``futhark-pkg`` about *your* code.  Everything is
created automatically as needed.  The above creates a new file
``futhark.pkg`` with the following contents:

.. code-block:: text

   require {
     github.com/athas/fibs 1.0.0 #7d0c88e0ee0f48e620eba33d8a84ce98c44f033a
   }

This lists one required package, with its package path, minimum
required version (which is the newest version unless otherwise
specified), and the expected commit hash.  The latter is used for
verification, to ensure that the contents of a package version cannot
be changed silently.

``futhark-pkg`` will perform network requests to determine whether a
package of the given name and with the given version exists and fail
otherwise (but it will not check whether the package is otherwise
well-formed).

Adding a package with ``futhark-pkg add`` modifies ``futhark.pkg``,
but does not download the package files.  All modifications to the
local file system (except updating ``futhark.pkg``) is done via
``futhark-pkg sync``, which modifies ``lib/`` based on the contents of
``futhark.pkg``.  Continuing our example::

  $ futhark-pkg sync
  $ tree lib
  lib
  └── github.com
      └── athas
          └── fibs
              ├── fibs.fut
              └── fibs_tests.fut

  3 directories, 2 files

The contents of an installed are just ordinary files in the file
system, and can be imported with the usual language mechanisms::

  > import "lib/github.com/athas/fibs/fibs"
  > fibs 10
  [1i32, 1i32, 2i32, 3i32, 5i32, 8i32, 13i32, 21i32, 34i32, 55i32]

This leads to somewhat verbose import paths, but on the other hand,
there is no magic, and no configuration necessary.

We can remove the package with::

  $ futhark-pkg remove github.com/athas/fibs

But we will need to run ``futhark-sync`` to actually remove the files
in ``lib/``::

  $ futhark-pkg sync
  $ tree lib
  lib

  0 directories, 0 files

The intended usage is that ``futhark.pkg`` is added to version
control, but ``lib/`` is not, as the contents of ``lib/`` can always
be reproduced from ``futhark.pkg``.  However, adding ``lib/`` as well
works just fine for people who prefer `vendoring
<https://stackoverflow.com/questions/26217488/what-is-vendoring>`_.

Upgrading Dependencies
~~~~~~~~~~~~~~~~~~~~~~

The ``futhark-pkg upgrade`` command will update every version
requirement in ``futhark.pkg`` to be the most recent available
version.  You still need to run ``futhark-pkg sync`` to actually
retrieve the new versions.  Be careful - while upgrades are safe if
semantic versioning is followed correctly, this is not yet properly
machine-checked, so human mistakes may occur.

As an example:

.. code-block:: text

   $ futhark-pkg add github.com/athas/fut-foo 0.1.0
   $ cat futhark.pkg
   require {
     github.com/athas/fut-foo 0.1.0 #d285563c25c5152b1ae80fc64de64ff2775fa733
   }

Now let's upgrade to the most recent version:

.. code-block:: text

   $ futhark-pkg upgrade
   Upgraded github.com/athas/fut-foo 0.1.0 => 0.2.1.
   $ cat futhark.pkg
   require {
     github.com/athas/fut-foo 0.2.1 #3ddc9fc93c1d8ce560a3961e55547e5c78bd0f3e
   }
   $ futhark-pkg sync
   $ tree lib
   lib
   └── github.com
       └── athas
           ├── fut-bar
           │   └── bar.fut
           └── fut-foo
               └── foo.fut

   4 directories, 2 files

Note that ``fut-foo 0.2.1`` depends on ``github.com/athas/fut-bar``,
so it was fetched automatically by ``futhark-pkg sync``.

``futhark-pkg upgrade`` will *never* upgrade across a major version
number.  Due to the principle of `Semantic Import Versioning
<https://research.swtch.com/vgo-import>`_, a new major version is a
completely different package from the point of view of the package
manager.  Thus, to upgrade to a new major version, you will need to
use ``futhark-pkg add`` to add the new version and ``futhark-pkg
remove`` to remove the old version.  Or you can keep it around - it is
perfectly acceptable to depend on multiple major versions of the same
package, because they are really different packages.

Creating Packages
-----------------

A package is a directory tree (which at the moment must correspond to
a Git repository).  It *must* contain two things:

  * A file ``futhark.pkg`` at the root defining the package path and
    any required packages.

  * A *package directory* ``lib/pkg-path``, where ``pkg-path`` is the
    full package path.

The contents of the package directory is what will be made available
to users of the package.  The repository may contain other things
(tests, data files, examples, docs, other programs, etc), but these
are ignored by ``futhark-pkg``.  This structure can be created
automatically by running for example::

  $ futhark-pkg create github.com/sturluson/edda

This only works if you do not already have a ``futhark.pkg`` file in
the current directory.  Note also, no ``https://``.  The result is
this ``futhark.pkg``::

  package github.com/sturluson/edda

  require {
  }

And this file hierarchy:

.. code-block:: text

   $ tree lib
   lib
   └── github.com
       └── sturluson
           └── edda

   3 directories, 0 files

Note that ``futhark-pkg create`` is not necessary simply to *use*
packages, only when *creating* packages.

When creating a package, the ``.fut`` files we are writing will be
located inside the ``lib/`` directory.  If the package has its own
dependencies, whose files we would like to access, we can use
*relative imports*.  For example, assume we are creating a package
``github.com/sturluson/edda`` and we are writing a Futhark file
located at ``lib/github.com/sturluson/edda/saga.fut``.  Further, we
have a dependency on the package ``github.com/athas/foo-fut``, which
is stored in the directory ``lib/github.com/athas/foo-fut``.  We can
import a file ``lib/github.com/athas/foo-fut/foo.fut`` from
``lib/github.com/sturluson/edda/saga.fut`` with::

  import "../foo-fut/foo"

Releasing a Package
~~~~~~~~~~~~~~~~~~~

Currently, a package corresponds exactly to a GitHub repository
mirroring the package path.  A release is done by tagging an
appropriate commit with ``git tag vX.Y.Z`` and then pushing the tag to
GitHub with ``git push --tags``.  In the future, this will be
generalised to other code hosting sites and version control systems
(and possibly self-hosted tarballs).  Remember to take semantic
versioning into account - unless you bump the major version number (or
the major version is 0), the new version must be *fully compatible*
with the old.

When releasing a new package, consider getting it added to the
`central package list <https://futhark-lang.org/pkgs>`_.  See `this
page
<https://github.com/diku-dk/futhark-docbot/blob/master/README.md>`_
for details.

Version Selection
-----------------

The package manifest ``futhark.pkg`` declares which packages the
program depends on.  Dependencies are specified as the *oldest
acceptable version* within the given major version.  Upper version
bounds are not supported, as strict adherence to semantic versioning
is assumed, so any later version with the same major version number
should work.  When ``futhark-pkg sync`` calculates which version of a
given package to download, it will pick the oldest version that still
satisfies the minimum version requirements of that package in all
transitive dependencies.  This means that a version may be used that
is newer than the one indicated in ``futhark.pkg``, but only if a
dependency requires a more recent version.

Safety
------

In contrast to some other package managers, ``futhark-pkg`` does not
run any package-supplied code on installation, upgrade, or removal.
This means that all ``futhark-pkg`` operations are in principle
completely safe (barring exploitable bugs in ``futhark-pkg`` itself,
which is unlikely but not impossible).  Further, Futhark code itself
is also completely pure, so executing it cannot have any unfortunate
effects, such as stealing your data.  The worst it can do is loop
infinitely, consume arbitrarily large amounts of memory, or produce
wrong results.

The exception is packages that uses ``unsafe``.  With some cleverness,
``unsafe`` can be combined with in-place updates to perform arbitrary
memory reads and writes, which can trivially lead to exploitable
behaviour.  You should not use untrusted code that employs ``unsafe``
(but the ``--safe`` compiler option may help).  However, this is not
any worse than calling external code in a conventional impure
language, which generally can perform any conceivable harmful action.

Wrapping Up
-----------

``futhark-pkg`` is a very simple package manager, but it makes serious
sacrifices to obtain that simplicity.  First of all, it is unclear
whether minimal version selection will work in practice in the long
term.  Fortunately, the dependency solver is a fairly isolated
component, so it can be replaced with something more elaborate if
necessary.  Second, ``futhark-pkg`` does not have a fully
thought-through mechanism for handling packages that get renamed.  And
finally, it would be nice if packages were not restricted to GitHub.
That should not be hard to fix, however.

The only way to determine whether ``futhark-pkg`` is useful is to use
it.  To this end, we have have written a handful of packages (mostly
extracted from existing benchmark programs), and built `futhark-docbot
<https://github.com/diku-dk/futhark-docbot>`_, which uses a
combination of ``futhark-pkg`` and `futhark-doc
<http://futhark.readthedocs.io/en/latest/man/futhark-doc.html>`_ to
automatically populate an `index of known Futhark packages along with
hyperlinked documentation <https://futhark-lang.org/pkgs>`_.  It works
quite well, and was easy to implement.  Hopefully more tooling will be
just as easy to add.  In particular, I would like a tool that can test
whether a new version of a package breaks compatibility with an old
version, and if so suggests a major version bump instead of a minor.
