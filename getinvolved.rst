---
title: Getting involved with Futhark
---

Futhark is both a research- and an open source project.  Everything we
implement is released under a free software license, and we do all our
development in the open, `on Github`_.  There are several ways of
getting involved:

- **Submit a bug!** Just open an issue on our `bug tracker`_.  This
  does not have to be an actual compiler bug, but might just be a
  feature suggestion, language design proposal, request for
  documentation, or anything else you believe we should do.

- **Write a patch!** Improving the compiler can be hard, but it is
  very rewarding!  The Futhark compiler is implemented in Haskell, and
  while an attempt has been made to steer away from the more obscure
  Haskell features and keep the design simple and maintainable, it is
  still an optimising compiler.  Such beasts are always a little
  complicated.

  A compiler patch does not have to be some sophisticated new
  optimisation - everything from fixing a typo in a comment to making
  the generated code look slightly prettier is very welcome.

- **Write a new benchmark program!** We want to make the fastest
  functional language there is, but for that to work, we will need
  benchmark programs.  We already have a healthy set of `benchmarks`_,
  but we could always use more.  We are most interested in programs
  that already have implementations in other languages, as this makes
  comparisons possible.

- **Improve this website!** The Futhark website is `also on Github`_
  and probably has room for improvement.

.. _`on Github`: https://github.com/diku-dk/futhark
.. _`bug tracker`: https://github.com/diku-dk/futhark/issues
.. _`benchmarks`: https://github.com/diku-dk/futhark-benchmarks
.. _`also on Github`: https://github.com/diku-dk/futhark-website
