---
title: Publications
---

We have published a number of research papers on Futhark, and hopefully
more will follow in the future. They are presented below in reverse
chronological order.

# Papers on language design and implementation

## Compiling Generalized Histograms for GPU

::: {.papermetadata}
Presented at [SC \'20](https://sc20.supercomputing.org/)
([pdf](publications/sc20.pdf), [bib](publications/sc20.bib))
:::

An explanation of the implementation strategy behind Futhark\'s
`reduce_by_index` construct. The paper mostly focuses on the low-level
performance analysis needed to make this construct run fast, but is also
some discussion of the language integration, and some of the compiler
optimisations. We have [made the research artifact
available](https://github.com/diku-dk/futhark-sc20), which also contains
a CUDA library implementation of the ideas.

## Bounds Checking on GPU

::: {.papermetadata}
Presented at [HLPP \'20](https://hlpp2020.dcc.fc.up.pt/)
([pdf](publications/hlpp20.pdf), [bib](publications/hlpp20.bib))
:::

Few GPU-targeting languages perform bounds checking, because it is
surprisingly tricky to do well on a GPU. For years, Futhark would also
refuse to compile code that required bounds checks (or similar safety
checks) in parallel code, with the programmer having to resort do
disabling bounds checks. However, eventually we found a simple and
efficient way to implement bounds checking, which we describe in this
paper.

## Compositional Deep Learning in Futhark

::: {.papermetadata}
Presented at [FHPNC \'19](https://icfp19.sigplan.org/home/FHPNC-2019)
([pdf](publications/fhpnc19.pdf), [bib](publications/fhpnc19.bib))
:::

This paper presents a library for implementing neural networks in
Futhark. The library functions are generically typed and the composition
structure allows for networks to be trained (using back-propagation) and
for trained networks to be used for predicting new results (using
forward-propagation). Individual layers in a network can take different
forms ranging over dense sigmoid layers to convolutional layers. We
demonstrate that Futhark\'s elimination of higher-order functions and
modules leads to efficient generated code, with comparison to
TensorFlow.

The [code is available](https://github.com/HnimNart/deeplearning) and
the paper itself is a distillation of Duc Minh Tran\'s [bachelor\'s
thesis](student-projects/duc-bsc-thesis.pdf).

## Data-Parallel Flattening by Expansion

::: {.papermetadata}
Presented at [ARRAY \'19](https://pldi19.sigplan.org/home/ARRAY-2019)
([pdf](publications/array19.pdf), [bib](publications/array19.bib))
:::

One of Futhark\'s main difficulties is its restriction to *regular*
parallelism. This paper presents a programming technique for expressing
certain kinds of *irregular* data-parallel problems in a regular manner.

## Incremental Flattening for Nested Data Parallelism

::: {.papermetadata}
Presented at [PPOPP \'19](https://ppopp19.sigplan.org/)
([pdf](publications/ppopp19.pdf), [bib](publications/ppopp19.bib))
:::

This paper expands on the compilation scheme presented in our [PLDI 2017
paper](#futhark-purely-functional-gpu-programming-with-nested-parallelism-and-in-place-array-updates),
to employ a *multi-versioned* approach, in which the parallelism in the
program is mapped to multiple independent (but semantically equivalent)
code versions, and the best one picked at run-time based on the concrete
input data observed. The title is an homage to the paper [Data-Only
Flattening for Nested Data
Parallelism](https://dl.acm.org/citation.cfm?id=2442525), which also
seeks to improve on the inefficiency of full flattening. See also the
[blog post on the paper](/blog/2019-02-18-futhark-at-ppopp.html).

[Research artifact available
here.](https://github.com/diku-dk/futhark-ppopp19)

## High-Performance Defunctionalisation in Futhark

::: {.papermetadata}
Presented at [TFP \'18](http://www.cse.chalmers.se/~myreen/tfp2018/)
([pdf](publications/tfp18.pdf), [bib](publications/tfp18.bib))
:::

Futhark initially did not support higher-order functions, because the
usual compilation strategy creates a great degree of indirection, which
can inhibit optimisation and efficient compilation. In this paper, we
present a de functionalisation transformation that relies on type-based
restrictions on the use of expressions of functional type, such that we
can completely eliminate higher-order functions in all cases, without
introducing any branching. We prove the correctness of the
transformation and discuss its implementation in Futhark, a
data-parallel functional language that generates GPU code. The use of
these restricted higher-order functions has no impact on run-time
performance, and we argue that we gain many of the benefits of general
higher-order functions, without in most practical cases being hindered
by the restrictions. An extended treatment can be found in Anders Kiel
Hovgaard\'s master\'s thesis, [available
here](student-projects/hovgaard-msc-thesis.pdf).

## Static Interpretation of Higher-Order Modules in Futhark

::: {.papermetadata}
Presented at [ICFP \'18](https://conf.researchr.org/home/icfp-2018)
([pdf](publications/icfp18.pdf), [bib](publications/icfp18.bib))
:::

This paper discusses the higher-order ML-style module system available
in Futhark. Most of the discussion is a theoretical treatment, including
a formally-verified implementation in Coq. The implementation in the
Futhark compiler does not use this verified implementation for a variety
of reasons, but it does almost exactly follow the semantic object
definitions given in the paper.

## Modular Acceleration: Tricky Cases of Functional High-Performance Computing

::: {.papermetadata}
Presented at [FHPC
\'18](https://icfp18.sigplan.org/track/FHPC-2018-papers)
([pdf](publications/fhpc18.pdf), [bib](publications/fhpc18.bib))
:::

This case study examines the data-parallel functional implementation of
three algorithms: generation of quasi-random Sobol numbers,
breadth-first search, and calibration of Heston market parameters via a
least-squares procedure. We show that while all these problems permit
elegant functional implementations, good performance depends on subtle
issues that must be confronted in both the implementations of the
algorithms themselves, as well as the compiler that is responsible for
ultimately generating high-performance code. In particular, we
demonstrate a modular technique for generating quasi-random Sobol
numbers in an efficient manner, study the efficient implementation of an
irregular graph algorithm without sacrificing parallelism, and argue for
the utility of nested regular data parallelism in the context of
nonlinear parameter calibration.

## Design and Implementation of the Futhark Programming Language

::: {.papermetadata}
Troels Henriksens PhD thesis (revised), defended in November of 2017
([pdf](publications/troels-henriksen-phd-thesis.pdf),
[bib](publications/troels-henriksen-phd-thesis.bib))
:::

This PhD thesis describes the overall background and motivation behind
the development of Futhark, as well as a collection of some of the core
implementation techniques (size-dependent typing, fusion, moderate
flattening, tiling). The treatment is high level, and the technicalities
of the concrete compiler implementation is not discussed in great
detail. The first part of the thesis describes the overall philosophy
behind the design and implementation of Futhark, and is fairly readable.
The latter part of the thesis, which discusses concrete program
transformations, is a more difficult read, and probably only of interest
to academics. The empirical evaluation chapter is a good description of
what Futhark does well, and what it does not so well (at least as of the
time the thesis was written).

## Strategies for Regular Segmented Reductions on GPU

::: {.papermetadata}
Presented at [FHPC
\'17](http://conf.researchr.org/track/FHPC-2017/FHPC-2017-papers)
([pdf](publications/fhpc17.pdf), [bib](publications/fhpc17.bib))
:::

A description of an implementation technique for regular segmented
reductions on GPU. The technique is based on having three different
strategies for dealing with different problem classes. This is the
technique currently used by the Futhark compiler, but it is presented in
a general setting, and could be used by other libraries and languages
that make use of regular segmented reductions.

## Futhark: Purely Functional GPU-Programming with Nested Parallelism and In-Place Array Updates

::: {.papermetadata}
Presented at [PLDI \'17](http://pldi17.sigplan.org/home)
([pdf](publications/pldi17.pdf), [bib](publications/pldi17.bib))
:::

A general and self-contained description of the main points of the
design and implementation of Futhark, including pieces of fusion, a
formalisation of the uniqueness typing rules, and our mechanism for
kernel extraction. The latter is the main novelty, as it allows the
Futhark compiler to exploit regular nested parallelism in a more
efficient (albeit also more restricted) manner than full flattening,
while still being more powerful than approaches that support only flat
parallelism. The [accompanying benchmark
suite](https://github.com/diku-dk/futhark-pldi17) is freely accessible.

## APL on GPUs - A TAIL from the Past, Scribbled in Futhark

::: {.papermetadata}
Presented at [FHPC
\'16](https://sites.google.com/site/fhpcworkshops/fhpc-2016)
([pdf](publications/fhpc16.pdf), [bib](publications/fhpc16.bib))
:::

A paper describing an APL compiler
([apltail](https://github.com/melsman/apltail/)) that operates by
translating APL into a *typed array intermediate language* (*TAIL*), and
from there into Futhark. While the Futhark details are light, the paper
demonstrates a simple use of Futhark as a target language for a
compiler. We succeed in achieving decent speedup on several (small) APL
programs. The [accompanying benchmark
suite](https://github.com/diku-dk/futhark-fhpc16) may be worth a look.

## Design and GPGPU Performance of Futhark's Redomap Construct

::: {.papermetadata}
Presented at [ARRAY
\'16](http://conf.researchr.org/track/pldi-2016/ARRAY-2016)
([pdf](publications/array16.pdf), [bib](publications/array16.bib))
:::

A detailed presentation of one of Futhark\'s internal language
constructs - `redomap` - which is used to represent various forms of
`map`-`reduce`-fusion. We present some microbenchmarks implemented in
both Thrust and Futhark and discuss their relative performance.

## Size Slicing - A Hybrid Approach to Size Inference in Futhark

::: {.papermetadata}
Presented at [FHPC
\'14](https://sites.google.com/site/fhpcworkshops/fhpc-2014)
([pdf](publications/fhpc14.pdf), [bib](publications/fhpc14.bib))
:::

Futhark supports automatic size inference of arrays, and this paper
describes our approach, which is based on slicing. The descriptions are
still up-to-date, although the Futhark source language has since grown
support for user-defined size annotations, which can sometimes enable
the compiler to make better assumptions about the shapes of arrays.

## Bounds Checking: An Instance of Hybrid Analysis

::: {.papermetadata}
Presented at [ARRAY \'14](http://www.sable.mcgill.ca/array/2014/)
([pdf](publications/array14.pdf), [bib](publications/array14.bib))
:::

We implemented a novel form of bounds checking by extracting *predicate
functions* from programs with array indexing. These predicates
functioned as *sufficient conditions* for all bounds checks in the
original program: if the extracted predicates evaluated to true, then
every array index was guaranteed to be in bounds. The idea is that this
produces an efficient alternative to precise bounds checking even for
very complicated accesses (such as indirect indexing). The idea works,
but was hard to implement and maintain and thus distracted us from our
core work, so it is no longer used in the Futhark compiler. Instead, we
provide an `unsafe` keyword that one can use to remove bounds checks
that would otherwise hinder parallelisation. In the future, we might
return to this work.

## A T2 Graph-Reduction Approach To Fusion

::: {.papermetadata}
Presented at [FHPC \'13](http://hiperfit.dk/fhpc13.html)
([pdf](publications/fhpc13.pdf), [bib](publications/fhpc13.bib))
:::

A presentation of the core of the producer-consumer fusion algorithm in
the Futhark compiler (although the language was called L0 at the time).
The description of the fundamental algorithm is still correct, although
it does not cover some of the newer language additions, nor does it
describe horisontal fusion.

# Application Papers

These papers are not about Futhark as a language or compiler, but use it
to build or show something.

-   Wojciech Michal Pawlak: **Accelerated Financial Algorithms
    Derivative Pricing and Risk Management Applications**.  PhD
    thesis.  May 2021 ([pdf](publications/wmp-phd-thesis.pdf),
    [bib](publications/wmp-phd-thesis.bib)).
-   Cosmin Oancea, Ties Robroek, Fabian Gieseke: **Approximate
    Nearest-Neighbour Fields via Massively-Parallel Propagation-Assisted
    K-D Trees**. MLDB\'20. ([pdf](publications/mlbd20.pdf)).
-   Fabian Geseke, Sabina Rosca, Troels Henriksen, Jan Verbesselt,
    Cosmin Oancea: **Massively-Parallel Change Detection for Satellite
    Time Series Data with Missing Value**. ICDE\'20.
    ([pdf](publications/icde20.pdf))
-   Wojciech Michal Pawlak, Martin Elsman, Cosmin Eugen Oancea: **A
    Functional Approach to Accelerating Monte Carlo based American
    Option Pricing**. IFL\'19. ([pdf](publications/ifl19.pdf))

# Selected Student Projects

-   Kristian Bøjer Andreasen: **Data-Parallel Coherency Sensitive
    Hashing for Approximate Nearest Neighbour Fields**.  MSc thesis.
    Department of Computer Science, University of
    Copenhagen. June 2021. ([pdf](student-projects/kristian-bøjer-andreasen-msc-thesis.pdf))
-   Christian Charlie Virt, Jonathan Wraa-Hansen: **Optimisation and
    GPU code generation of Stencils for Futhark**.  June 2021.
    ([pdf](student-projects/christian-jonathan-msc-thesis.pdf)).
-   Morten Tychsen Clausen: **Regular Segmented Single-pass Scan in
    Futhark**.  June 2021. ([pdf](student-projects/morten-msc-thesis.pdf))
-   Kasper Unn Weihe, Kristian Quirin Hansen, Peter Kanstrup Larsen:
    **Linear Algebra in Futhark**.  Department of Computer Science,
    University of Copenhagen. February 2021.
    ([pdf](student-projects/kristian-kasper-peter-project.pdf))
-   Till Severin Grenzdörffer: **Accelerating Ocean Modelling: Adressing
    performance bottlenecks of the ocean modelling framework Veros**.
    MSc project, Department of Computer Science, University of
    Copenhagen. January 2021.
    ([pdf](student-projects/till-msc-project.pdf))
-   Kristian Høi: **Parallel implementations of machine learning
    algorithms: Gradient boosted decision trees**. MSc Thesis,
    Department of Computer Science, University of Copenhagen.
    January 2021. ([pdf](student-projects/kristian-msc-thesis.pdf))
-   Andreas Nicolaisen, Marco Aslak Persson: **Implementing Single-Pass
    Scan in the Futhark Compiler**. MSc project. Department of Computer
    Science, University of Copenhagen. November 2020.
    ([pdf](student-projects/marco-andreas-scan.pdf))
-   Duc Minh Tran: **Multicore backend for Futhark**. MSc Thesis.
    Department of Computer Science, University of Copenhagen.
    September 2020. ([pdf](student-projects/duc-msc-thesis.pdf))
-   Emil Un Weihe: **Support Vector Machines in Futhark**. MSc thesis.
    Computer Science, University of Copenhagen. September 2020.
    ([pdf](student-projects/emil-msc-thesis.pdf))
-   Michaël El Kharroubi: **Highly parallel algorithms on GPU with
    Futhark: Practical case with block ciphers**. BSc thesis. HEPIA.
    August 2020. ([pdf](student-projects/michael-bsc-thesis.pdf))
-   Johan Johansson, Ari von Nordenskjöld: **Ray Tracing for Sensor
    Simulation using Parallel Functional Programming**. MSc thesis.
    Chalmers University of Technology. June 2020.
    ([pdf](student-projects/johan-ari-msc-thesis.pdf))
-   Ulrik Elmelund Petersen: **Optimizing the kNN algorithm for GPGPUs
    in Futhark**. BSc thesis. Computer Science, University of
    Copenhagen. June 2020.
    ([pdf](student-projects/ulrik-bsc-thesis.pdf))
-   Mathias Friis Rasmussen, Jonas Kristensen, Jens Nissen-Juul
    Sørensen, Christian Dybdahl Troelsen: **FutSpace - A Parallelizable
    Implementation of the Voxel Space Rendering Algorithm**. BSc thesis.
    Computer Science, University of Copenhagen. June 2020.
    ([pdf](student-projects/futspace-bsc-thesis.pdf))
-   Ulrik Stuhr Larsen, Lotte Maria Bruun: **A Language for Parallel
    Generation of L-Systems**. BSc thesis. Computer Science, University
    of Copenhagen. June 2020.
    ([pdf](student-projects/lotte-ulrik-bsc-thesis.pdf))
-   Robert Schenck: **Sum types in Futhark**. MSc thesis. Computer
    Science, University of Copenhagen. December 2019.
    ([pdf](student-projects/robert-msc-thesis.pdf))
-   Henrik Urms, Anna Sofie Kiehn: **Refinement types in Futhark**. MSc
    thesis. Computer Science, University of Copenhagen. September 2019.
    ([pdf](student-projects/kiehn-urms-msc-thesis.pdf))
-   Steffen Holst Larsen: **Multi-GPU Futhark Using Parallel Streams**.
    MSc thesis. Department of Computer Science, University of
    Copenhagen. September 2019.
    ([pdf](student-projects/steffen-msc-thesis.pdf))
-   Svend Lund Breddam: **Futhark Autotuners for Incremental
    Flattening**. MSc thesis. Department of Computer Science, University
    of Copenhagen. September 2019.
    ([pdf](student-projects/svend-msc-thesis.pdf))
-   Steffen Holst Larsen: **Futhark Vulkan Backend**. MSc project.
    Department of Computer Science, University of Copenhagen.
    January 2019. ([pdf](student-projects/steffen-msc-project.pdf))
-   Jakob Stokholm Bertelsen: **Implementing a CUDA Backend for
    Futhark**. BSc thesis. Department of Computer Science, University of
    Copenhagen. January 2019.
    ([pdf](student-projects/jakob-bsc-thesis.pdf))
-   Sune Hellfritzsch: **Efficient Histogram Computation on GPGPUs**.
    MSc thesis. Department of Computer Science, University of
    Copenhagen. October 2018.
    ([pdf](student-projects/hellfritzsch-msc-thesis.pdf))
-   Duc Minh Tran: **Implementation of a deep learning library in
    Futhark**. BSc Thesis. Department of Computer Science, University of
    Copenhagen. August 2018.
    ([pdf](student-projects/duc-bsc-thesis.pdf))
-   Mikkel Storgaard Knudsen: **FShark: Futhark programming in FSharp**.
    MSc thesis. Department of Computer Science, University of
    Copenhagen. August 2018.
    ([pdf](student-projects/mikkel-msc-thesis.pdf))
-   Marek Hlava and Martin Metaksov: **Accelerated Interest Rate Option
    Pricing using Trinomial Trees**. MSc thesis. Department of Computer
    Science, University of Copenhagen. August 2018.
    ([pdf](student-projects/marek-martin-msc-thesis.pdf))
-   Kasper Abildtrup Hansen: **FFT Generator in Futhark: A prototype
    Futhark library using FFTW technniques**. MSc project. Department of
    Computer Science, University of Copenhagen. June 2018.
    ([pdf](student-projects/kasper-hansen-genfft.pdf))
-   Frederik Thorøe: **Auto-tuning of threshold-parameters in Futhark**.
    BSc thesis. Department of Computer Science, University of
    Copenhagen. June 2018.
    ([pdf](student-projects/frederik-thoroe-bsc-thesis.pdf))
-   Mette Marie Kowalski: **Designing and Accelerating a Generic FFT
    Library in Futhark**. BSc thesis. Department of Computer Science,
    University of Copenhagen. June 2018.
    ([pdf](student-projects/mette-kowalski-bsc-thesis.pdf))
-   Anders Kiel Hovgaard: **Higher-order functions for a
    high-performance programming language for GPUs**. MSc project.
    Department of Computer Science, University of Copenhagen. May 2018.
    ([pdf](student-projects/hovgaard-msc-thesis.pdf))
-   Niels G. W. Serup: **Memory Block Merging in Futhark**. MSc thesis.
    Department of Computer Science, University of Copenhagen.
    November 2017. ([pdf](student-projects/niels-msc-thesis.pdf))
-   Rasmus Wriedt Larsen: **Generating Efficient Code for Futhark's
    Segmented Redomap**. MSc thesis. Department of Computer Science,
    University of Copenhagen. March 2017.
    ([pdf](student-projects/rasmus-msc-thesis.pdf))
-   Niels G. W. Serup: **Extending Futhark with a write construct**. MSc
    project. Department of Computer Science, University of Copenhagen.
    June 2016. ([pdf](student-projects/niels-write-construct.pdf)).
