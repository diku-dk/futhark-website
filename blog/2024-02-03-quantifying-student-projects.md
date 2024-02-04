---
title: Quantifying Student Projects
author: Troels Henriksen
description: The university produces a source of free labour, and this is what we get out of it.
---

Futhark's main developers work as researchers and teachers at the
[University of Copenhagen](https://diku.dk). One of the perks of
working at a university is a steady supply of ~~free labour~~ students
looking for projects. Each undergraduate student must at least write a
bachelor's thesis, and each master's student must at least write a
master's thesis. Beyond these, students can also do elective projects
if they wish. When I was a student, I always found it very interesting
to participate in real research projects, and I would like current
generations of students to have that experience as well. Allowing
students to participate in Futhark development benefits all: the
students get to spend their time building stuff that (potentially)
actually matters, and Futhark improves in ways we wouldn't have time
for otherwise. I wrote a little bit about how we make use of student
projects when I [wrote my PhD
reflections](2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html),
and [again
later](https://futhark-lang.org/blog/2022-07-07-futhark-0.21.13-released.html#the-fruits-of-forced-labour),
but I thought maybe people would be interested in some numbers about
how much student work has gone into the Futhark compiler.

Anyone who has worked in academia has experience with software built
though *student-based development*. Such software is maintained by
generations of students, each of which contribute a few pieces that
are integrated in whatever way is possible, without any major thought
towards long-term maintainability or coherent design. The quality is
usually poor, documentation often absent, and problems fixed only by
throwing more students at it. This is a risk that I was acutely aware
of when we began inviting students to work on the compiler, and we
have been quite picky regarding what we ultimately merge.

There are of course students who simply do not manage to produce a
contribution of the required quality. That is perfectly fine and
expected - they are still likely to have passed their project, which
is evaluated on their completion on learning goals, not how much we
were able to exploit their labour.

Second, some exploratory projects may produce a contribution that does
work, but which is simply too complex compared to what it offers. The
best example is the work by Steffen Holst Larsen on [Multi-GPU
Execution](https://futhark-lang.org/student-projects/steffen-msc-thesis.pdf)
and a [Vulkan
backend](https://futhark-lang.org/student-projects/steffen-msc-project.pdf).
Absolutely top notch work, but neither the compiler infrastructure nor
the surrounding software environment (in the case of Vulkan) was ready
at the time. Merging these contributions would have imposed a
nontrivial maintenance burden on us, without truly benefiting users.
These were both successful projects, in that we learned things we have
made use of since, and Steffen went on to work on compilers at
[Codeplay](https://codeplay.com/).

Let's talk numbers. First, some caveats. I only have numbers for the
projects I have supervised or co-supervised myself. [Cosmin
Oancea](http://hjemmesider.diku.dk/~zgh600/), who founded the Futhark
project, has supervised several compiler-related projects, which are
not included here. [Martin Elsman](https://elsman.com/) has also
supervised many projects, but mostly about data parallel programming,
and less about the compiler itself. I am also leaving out PhD
students, as their contributions are on a very different scale.

But as for me, I have (co-)supervised a total of 52 projects: 15 MSc
theses, 31 BSc theses, and 6 auxiliary projects. Of these, 15 were not
directly related to the compiler or its tooling, but involved such
things as implementing parallel algorithms or porting
[benchmarks](https://github.com/diku-dk/futhark-benchmarks). I will
not be considering these further, but several of them resulted in work
that we still use, or [intend to
use](https://github.com/diku-dk/alpacc).

That leaves 37 projects that directly worked on the compiler or its
tooling. Of these, 22 resulted in contributions that were integrated
in the main compiler code base. I can list a few of the more
noteworthy ones:

* [The CUDA backend](https://github.com/diku-dk/futhark/pull/704) by
  Jakob Stokholm Bertelsen.

* [A rewrite of the fusion
  engine](https://github.com/diku-dk/futhark/pull/1670) by Walter
  Restelli-Nielsen and Amar Topalovic.

* The Python/PyOpenCL backends by Daniel Gavin and Hjalte Abelskov.

* [A C# backend](https://github.com/diku-dk/futhark/pull/612) by
  Mikkel Storgaard (although [later
  removed](https://github.com/diku-dk/futhark/pull/993) because we
  couldn't maintain it).

* Improvements to the autotuner, by various students including
  Frederik Thorøe, Simon Rotendahl, and Carl Mathias Graae Larsen.

* [Futhark Language
  Server](https://github.com/diku-dk/futhark/pull/1618) by Haoran Sun.

* [Sum types](https://github.com/diku-dk/futhark/pull/779) by Robert
  Schenck.

* [Register tiling](https://github.com/diku-dk/futhark/pull/1233) by
  Anders Holst and Æmilie Cholewa-Madsen.

* [WebAssembly backend](https://github.com/diku-dk/futhark/pull/1395)
  by Philip Rajani Lassen.

* [The multicore
  backend](https://github.com/diku-dk/futhark/pull/1146) by Duc Minh
  Tran.

* [The ISPC backend](https://github.com/diku-dk/futhark/pull/1677) by
  Louis Marott Normann, Kristoffer August Kortbæk, William Pema Norbu
  Holmes Malling, and Oliver Bak Kjersgaard Petersen.

* [Scalar migration](https://github.com/diku-dk/futhark/pull/1658) by
  Philip Jon Børgesen.

* [Reactive
  benchmarking](https://github.com/diku-dk/futhark/pull/1629) by
  Aleksander Junge.

* [Locality
  optimisations](https://github.com/diku-dk/futhark/pull/2027) by
  Oscar Nelin and Bjarke Lohmann Pedersen - actually not quite merged
  yet, but will be when I find the time to do all the fit and finish.

These range from significant new parts of the compiler (such as new
backends), to rewrites of pre-existing older passes (the fusion
engine, locality optimisations). The latter are actually the hardest
to integrate, as we want to avoid performance regressions, and the
current behaviour is often a mix of hacks and ad-hoc implementation
quirks.

Futhark has definitely benefited from student work in the past, and it
is certain that we will continue to do so in the future.
