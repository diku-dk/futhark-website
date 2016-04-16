---
title: Docs
---

Futhark still suffers from rather spartan documentation.  What we have
is assembled `at Read the Docs`_.  Among other things, you can find a
`language overview`_, `language reference`_, and a set of manual
pages.

If there is something you believe should be documented, but is not,
you are very welcome to report the omission as a bug on our bug
tracker.  See the page `Get Involved`_ for more information.

We also have some Haddock-generated `documentation of the compiler internals`_.

.. _`at Read the Docs`: https://futhark.readthedocs.org/
.. _`language overview`: https://futhark.readthedocs.org/en/latest/language-overview.html
.. _`language reference`: https://futhark.readthedocs.org/en/latest/language-reference.html
.. _`Get Involved`: /getinvolved.html
.. _`documentation of the compiler internals`: /haddock/

Publications
************

We have published a number of papers on Futhark, and hopefully more
will follow in the future.  They are presented below in reverse
chronological order.

Design and GPGPU Performance of Futhark’s Redomap Construct (`PDF <publications/array16.pdf>`_)
-----------------------------------------------------------------------------------------------

.. class:: papermetadata
Submitted to `ARRAY '16`_ (response pending)

A detailed presentation of one of Futharks internal language
constructs - ``redomap`` - which is used to represent various forms of
``map``-``reduce``-fusion.  We present some microbenchmarks
implemented in both Thrust and Futhark and discuss their relative
performance.

Gotta Go Fast: An Optimising GPGPU Compiler for a Data-Parallel Purely Functional Language (`PDF <publications/icfp16.pdf>`_)
-----------------------------------------------------------------------------------------------------------------------------

.. class:: papermetadata
Submitted to `ICFP '16`_ (response pending)

This is the first paper in which we present performance figures for
our parallel code generator.  We compare the performance of Futhark to
Accelerate and hand-written C with OpenCL and obtain very decent
results.  We also describe the uniquenss type system, kernel
extraction, fusion, and efficient sequentialisation.  A dense paper,
but it contains a lot of information on our compilation process.

Size Slicing - A Hybrid Approach to Size Inference in Futhark (`PDF <publications/fhpc14.pdf>`_)
------------------------------------------------------------------------------------------------

.. class:: papermetadata
Presented at `FHPC '14`_

Futhark supports automatic size inference of arrays, and this paper
describes our approach, which is based on slicing.  The descriptions
are still up-to-date, although the Futhark source language has since
grown support for user-defined size annotations, which can sometimes
enable the compiler to make better assumptions about the shapes of
arrays.

Bounds Checking: An Instance of Hybrid Analysis (`PDF <publications/array14.pdf>`_)
-----------------------------------------------------------------------------------

.. class:: papermetadata
Presented at `ARRAY '14`_

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

.. class:: papermetadata
Presented at `FHPC '13`_

A presentation of the core of the producer-consumer fusion algorithm
in the Futhark compiler (although the language was called L0 at the
time).  The description of the fundamental algorithm is still correct,
although it does not cover some of the newer language additions, nor
does it describe horisontal fusion.

.. _`FHPC '13`: http://hiperfit.dk/fhpc13.html
.. _`FHPC '14`: https://sites.google.com/site/fhpcworkshops/fhpc-2014
.. _`ARRAY '14`: http://www.sable.mcgill.ca/array/2014/
.. _`ICFP '16`: http://conf.researchr.org/home/icfp-2016
.. _`ARRAY '16`: http://conf.researchr.org/track/pldi-2016/ARRAY-2016
