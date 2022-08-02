---
title: Docs
---

The Futhark documentation is divided into several parts. The in-progress
book [Parallel Programming in
Futhark](https://futhark-book.readthedocs.io) can be read freely online,
and is the starting point for learning Futhark. The [Futhark User\'s
Guide](https://futhark.readthedocs.io/en/stable) contains detailed
instructions on how to use the compilers, as well as the [language
reference](https://futhark.readthedocs.io/en/stable/language-reference.html)
and instructions on [how to install the Futhark
compiler](https://futhark.readthedocs.io/en/stable/installation.html).

There is also automatically generated [documentation for the Futhark
prelude](https://futhark-lang.org/docs/prelude/). The prelude library is
very small, so in most cases you will want to use [external
packages](https://futhark-lang.org/pkgs/).

If there is something you believe should be documented, but is not, you
are very welcome to report the omission as a bug on our bug tracker. See
the page [Get Involved](/getinvolved.html) for more information.

# Bridges

The Futhark compiler can generate C or Python code. A bridge allows
programs written in other languages to conveniently call this code.

-   **Haskell**: [Futhask](https://gitlab.com/Gusten_Isfeldt/futhask)
-   **Python**:
    [futhark-pycffi](https://github.com/pepijndevos/futhark-pycffi/) (or
    the compiler\'s [builtin Python
    backend](https://futhark.readthedocs.io/en/stable/man/futhark-pyopencl.html),
    which is slower but more convenient)
-   **Rust**: [genfut](https://github.com/Erk-/genfut), [futhark-bindgen](https://github.com/zshipko/futhark-bindgen)
-   **Standard ML**: [futhark-server-sml](https://github.com/diku-dk/futhark-server-sml).
-   **OCaml**: [futhark-bindgen](https://github.com/zshipko/futhark-bindgen)

# Tools

-   [Futhark support for Sublime Text
    3](https://github.com/titouanc/sublime-futhark)
-   [futhark-mode](https://github.com/diku-dk/futhark-mode) for Emacs
-   [Syntax highlighting for
    Vim](https://github.com/BeneCollyridam/futhark-vim)
-   [Language Extension for VS Code](https://github.com/diku-dk/futhark-vscode)
-   [Futhark language definition for
    Gedit](https://github.com/diku-dk/futhark/blob/master/tools/futhark.lang)
    (place the linked file in
    `~/.local/share/gtksourceview-3.0/language-specs/`)
-   [Property-based testing for
    Futhark](https://github.com/Unigurd/fucheck)

# Syntax highlighters that support Futhark

- [Linguist](https://github.com/github/linguist/) (through [language-etc](https://github.com/Alhadis/language-etc)).

- [Pygments](https://pygments.org/)

- [Kate Highlighting XML](skylighting/futhark.xml), compatible with [Skylighting](https://hackage.haskell.org/package/skylighting) which is used by [Pandoc](https://pandoc.org/).

- [LaTeX `listings`](https://github.com/diku-dk/futhark/blob/master/tools/futhark-listings.tex)
