---
title: Docs
---

Futhark still suffers from rather spartan documentation.  What we have
is assembled `at Read the Docs`_.  Among other things, you can find a
`language overview`_, `list of papers`_, and a set of manual pages.

If there is something you believe should be documented, but is not,
you are very welcome to report the omission as a bug on our bug
tracker.  See the page `Get Involved`_ for more information.

We also have some Haddock-generated `documentation of the compiler internals`_.

.. _`at Read the Docs`: https://futhark.readthedocs.org/
.. _`language overview`: https://futhark.readthedocs.org/en/latest/language-overview.html
.. _`list of papers`: https://futhark.readthedocs.org/en/latest/publications.html
.. _`Get Involved`: /getinvolved.html
.. _`documentation of the compiler internals`: /haddock/

Publications
============

We have published a number of papers on Futhark, and hopefully more in
the future.  They are presented below in reverse chronological order.

Size Slicing - A Hybrid Approach to Size Inference in Futhark (`PDF <publications/fhpc14.pdf>`_)
------------------------------------------------------------------------------------------------

Futhark supports automatic size inference of arrays, and this paper
describes our approach, which is based on slicing.  The descriptions
are still up-to-date, although the Futhark source language has since
grown support for user-defined size annotations, which can sometimes
enable the compiler to make better assumptions about the shapes of
arrays.

Bounds Checking: An Instance of Hybrid Analysis (`PDF <publications/array14.pdf>`_)
-----------------------------------------------------------------------------------

We implemented a novel form of bounds checking by extracting
*predicate functions* from programs with array indexing.  These
predicates functioned as *sufficient conditions* for all bounds checks
in the original program: if the extracted predicates evaluated to
true, then every array index was guaranteed to be in bounds.  The idea
is that this produces an efficient alternative to precise bounds
checking even for very complicated accesses (such as indirect
indexing).  The idea works, but was hard to implement and maintain and
thus distracted us from our core work, so it is no longer used in the
Futhark compiler.  Instead, we provide an ``unsafe`` keyword that one
can use to remove bounds checks that would otherwise hinder
parallelisation.  In the future, we might return to this work.

A T2 Graph-Reduction Approach To Fusion (`PDF <publications/fhpc13.pdf>`_)
--------------------------------------------------------------------------

A presentation of the core of the producer-consumer fusion algorithm
in the Futhark compiler (although the language was called L0 at the
time).  The description of the fundamental algorithm is still correct,
although it does not cover some of the newer language additions, nor
does it describe horisontal fusion.
