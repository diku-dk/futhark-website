---
title: Docs
---

The Futhark documentation is divided into several parts.  The
in-progress book `Parallel Programming in Futhark`_ can be read freely
online, and is the starting point for learning Futhark. The `Futhark
User's Guide`_ contains detailed instructions on how to use the
compilers, as well as the `language reference`_ and instructions on
`how to install the Futhark compiler`_.

There is also automatically generated `documentation for the Futhark
prelude`_.  The prelude library is very small, so in most cases
you will want to use `external packages <https://futhark-lang.org/pkgs/>`_.

If there is something you believe should be documented, but is not,
you are very welcome to report the omission as a bug on our bug
tracker.  See the page `Get Involved`_ for more information.

.. _`Parallel Programming in Futhark`: https://futhark-book.readthedocs.io
.. _`Futhark User's Guide`: https://futhark.readthedocs.io/en/stable
.. _`language reference`: https://futhark.readthedocs.io/en/stable/language-reference.html
.. _`how to install the Futhark compiler`: https://futhark.readthedocs.io/en/stable/installation.html
.. _`documentation for the Futhark prelude`: https://futhark-lang.org/docs/prelude/
.. _`Get Involved`: /getinvolved.html

Bridges
*******

The Futhark compiler can generate C or Python code.  A bridge allows
programs written in other languages to conveniently call this code.

* **Haskell**: `Futhask <https://gitlab.com/Gusten_Isfeldt/futhask>`_

* **Python**: `futhark-pycffi
  <https://github.com/pepijndevos/futhark-pycffi/>`_ (or the
  compiler's `builtin Python backend
  <https://futhark.readthedocs.io/en/stable/man/futhark-pyopencl.html>`_,
  which is slower but more convenient)

* **Rust**: `genfut <https://github.com/Erk-/genfut>`_

Tools
*****

* `Futhark support for Sublime Text 3 <https://github.com/titouanc/sublime-futhark>`_

* `futhark-mode <https://github.com/diku-dk/futhark-mode>`_ for Emacs

* `Syntax highlighting for Vim <https://github.com/BeneCollyridam/futhark-vim>`_

* `Futhark language definition for Gedit
  <https://github.com/diku-dk/futhark/blob/master/tools/futhark.lang>`_
  (place the linked file in
  ``~/.local/share/gtksourceview-3.0/language-specs/``)

* `Property-based testing for Futhark <https://github.com/Unigurd/fucheck>`_
