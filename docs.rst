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
Basis Library`_.

If there is something you believe should be documented, but is not,
you are very welcome to report the omission as a bug on our bug
tracker.  See the page `Get Involved`_ for more information.

We also have some Haddock-generated `documentation of the compiler
internals`_ which is automatically updated every night.

.. _`Parallel Programming in Futhark`: https://futhark-book.readthedocs.io
.. _`Futhark User's Guide`: https://futhark.readthedocs.io/
.. _`language reference`: https://futhark.readthedocs.io/en/latest/language-reference.html
.. _`how to install the Futhark compiler`: https://futhark.readthedocs.io/en/latest/installation.html
.. _`documentation for the Futhark Basis Library`: /docs/
.. _`Get Involved`: /getinvolved.html
.. _`documentation of the compiler internals`: /haddock/

Publications
************

We have published a number of papers on Futhark, and hopefully more
will follow in the future.  They are presented below in reverse
chronological order.

Strategies for Regular Segmented Reductions on GPU (`PDF <publications/fhpc17.pdf>`_)
-------------------------------------------------------------------------------------

.. class:: papermetadata
To be presented at `FHPC '17`_

A description of an implementation technique for regular segmented
reductions on GPU.  The technique is based on having three different
strategies for dealing with different problem classes.  This is the
technique currently used by the Futhark compiler, but it is presented
in a general setting, and could be used by other libraries and
languages that make use of regular segmented reductions.

Futhark: Purely Functional GPU-Programming with Nested Parallelism and In-Place Array Updates (`PDF <publications/pldi17.pdf>`_)
--------------------------------------------------------------------------------------------------------------------------------

.. class:: papermetadata
Presented at `PLDI '17`_

A general and self-contained description of the main points of the
design and implementation of Futhark, including pieces of fusion, a
formalisation of the uniqueness typing rules, and our mechanism for
kernel extraction.  The latter is the main novelty, as it allows the
Futhark compiler to exploit regular nested parallelism in a more
efficient (albeit also more restricted) manner than full flattening,
while still being more powerful than approaches that support only flat
parallelism.  The `accompanying benchmark suite
<https://github.com/diku-dk/futhark-pldi17>`_ is freely accessible.

APL on GPUs - A TAIL from the Past, Scribbled in Futhark (`PDF <publications/fhpc16.pdf>`_)
------------------------------------------------------------------------------------------------------

.. class:: papermetadata
Presented at `FHPC '16`_

A paper describing an APL compiler (`apltail`_) that operates by
translating APL into a *typed array intermediate language* (*TAIL*),
and from there into Futhark.  While the Futhark details are light, the
paper demonstrates a simple use of Futhark as a target language for a
compiler.  We succeed in achieving decent speedup on several (small)
APL programs.  The `accompanying benchmark suite
<https://github.com/diku-dk/futhark-fhpc16>`_ may be worth a look.

Design and GPGPU Performance of Futhark’s Redomap Construct (`PDF <publications/array16.pdf>`_)
----------------------------------------------------------------------------------------------------------

.. class:: papermetadata
Presented at `ARRAY '16`_

A detailed presentation of one of Futhark's internal language
constructs - ``redomap`` - which is used to represent various forms of
``map``-``reduce``-fusion.  We present some microbenchmarks
implemented in both Thrust and Futhark and discuss their relative
performance.

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
.. _`FHPC '16`: https://sites.google.com/site/fhpcworkshops/fhpc-2016
.. _`ARRAY '14`: http://www.sable.mcgill.ca/array/2014/
.. _`ICFP '16`: http://conf.researchr.org/home/icfp-2016
.. _`ARRAY '16`: http://conf.researchr.org/track/pldi-2016/ARRAY-2016
.. _`apltail`: https://github.com/melsman/apltail/
.. _`PLDI '17`: http://pldi17.sigplan.org/home
.. _`FHPC '17`: http://conf.researchr.org/track/FHPC-2017/FHPC-2017-papers
