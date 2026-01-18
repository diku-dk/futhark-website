---
title: NOVA might have been Futhark
author: Troels Henriksen
description: Futhark arose in a surprisingly crowded little field, and it's a fair question whether we shouldn't have made a new language as well.
---

I have written about Futhark's history before - both [my from my own perspective
as a PhD
student](2017-12-27-reflections-on-a-phd-accidentally-spent-on-language-design.html),
and [more generally](2021-12-19-past-and-present.html). But as is natural during
time time of the year, one's thoughts dwell on the past, and I was reminded of a
programming language project (NOVA, not the new one) that arose around the same
time as Futhark, with largely the same goals and approach, and I thought it
might be interesting to write down my memories of how we looked at it at the
time, and why we ultimately decided to make a new language, instead of building
on other work.

While it is perhaps a stretch to say that Futhark arose in a *crowded* field,
the early 2010s had a good number of projects trying to make functional
programming languages for GPUs. One of the most prominent (and which is still
being worked on) is [Accelerate](https://www.acceleratehs.org/), which is
embedded in Haskell, and arose out of a popular trend -
[Nikola](https://dl.acm.org/doi/10.1145/1863523.1863533) is an even earlier
example of a GPU language embedded in Haskell. Really, embedded languages were
quite fashionable in general!
[Copperhead](https://github.com/bryancatanzaro/copperhead) was the same idea
applied to Python, and of course today there are *tons* of GPU languages
embedded in Python. While embedded languages can be quite convenient, we found
the constraints imposed by embedding to be somewhat of a distraction from the
real research we wanted to do - compiler transformations - and so we were not
interested in going in this direction.

Not all the GPU languages at the time were embedded -
[Harlan](https://github.com/eholk/harlan) is a very interesting language that
focused on maximising expressive power on GPUs, and used rather fancy
region-based memory management to support rich data structures on GPUs. Harlan's
focus was not performance however, and its lack of restrictions probably makes
it difficult to achieve performance parity with hand-written code, so it was
also not a good fit for our ambitions. And of course,
[SAC](https://sac-home.org/) was and remains around.

The NOVA programming language, which is described in the paper [NOVA: A
Functional Language for Data
Parallelism](https://www.classes.cs.uchicago.edu/archive/2016/winter/32001-1/papers/nova-array14.pdf)
from 2014, is surprisingly close in its design to what Futhark eventually
became. NOVA is a standalone functional data-parallel language, with a
vocabulary very similar to Futhark (`map`, `scan`, `reduce`, etc), polymorphism
using Hindley-Milner-based, an optimising compiler, and even features that
Futhark *still* does not have, such as recursive data types. NOVA's Lisp-like
syntax is unlike Futhark's ML-based, but as Common Lisp was my favourite
language when I first got into programming, this is actually a *benefit* from my
perspective. NOVA is benchmarked against hand-written CUDA programs and the
[Thrust](https://developer.nvidia.com/thrust) library, *exactly* as we did in
the first Futhark papers. There is basically no difference between what NOVA
tried to do (and largely achieved) and what Futhark did, but the NOVA paper was
published a year before Futhark had its first embryonic GPU code generator. So
why didn't we just ditch what we were doing and build on Nova instead? At the
time, we really didn't care about the language as such, only compiler
optimisations - NOVA was published before Futhark was even named. The answers
are a bit murky, ten years later, but I will try to give an answer based on my
memories and a reconstruction of what our thoughts might plausibly have been.

The *main* reason we did not build on NOVA, and this is perhaps the most
important lesson, is because the source code is not publicly available. I have
no doubt that if we had written to the authors, we would have been given
access - but this is a significant psychological hurdle, especially, if it turns
out that the code is not suitable for our needs. Perhaps it would turn out to be
a an unusable proof-of-concept, or have other unacceptable flaws (such as being
written in C++). If the code is publicly available without involving the
authors, it is much easier to investigate suitability for one's own needs,
without risk of social awkwardness. This a major reason why I take great care to
publish and document all code I write to substantiate scientific publications. I
think a large part of Futhark's relative success in its tiny niche (in that a
nonzero number of people have used it and even contributed to it) is because it
is straightforwardly available.

Beyond that, let us read the NOVA paper more critically, as we did when it came
out. First, it was published as part of the ARRAY '14 workshop. I am personally
a big fan of workshops, and tend to enjoy them more than large and prestigious
conferences, but they are particularly welcoming of unfinished work-in-progress.
This opens the reasonable possibility that although NOVA certainly had the
features claimed, they perhaps did not work universally - which is also not
something the paper claims. In particular, features such as recursive data types
are *really tricky* to implement properly on a GPU - consider that, the year
after, the Harlan language was able to get a good publication out of memory
management *by itself*. It is possible that NOVA managed to solve the problem,
but considering the great amount of work this would have required, the technique
would certainly have merited a mention in the paper. NOVA also mentions support
for nested (irregular) arrays, and talks about flattening them, but does not
talk about flattening the parallelism in any nontrivial way - which again is
something that I *know* is hard to do, because [we wrote a PLDI paper largely
about that](https://futhark-lang.org/blog/2017-06-25-futhark-at-pldi.html)
several years after. Finally, consider the benchmarks shown in figures 3-5 in
the paper: they are things like Black-Scholes, [word
counting](https://futhark-lang.org/blog/2019-10-25-beating-c-with-futhark-on-gpu.html),
*n*-body, and so on. Fine problems, and the performance reported is undoubtedly
impressive, but none of them require any of the language features that are
difficult to implement on a GPU.

My hunch, re-reading the paper over ten years later, is that NOVA was a solid
design and likely implementation, based on the same ideas that inspired Futhark
(so of course I'd like it). I think all of its features worked when compiling to
CPU code, but that only a subset could be compiled for GPU execution. That is
not a particularly damning indictment, as [even Futhark cannot actually handle
quite everything
yet](https://futhark-lang.org/blog/2018-12-08-why-futhark-sometimes-goes-wrong.html).
One somewhat frustrating bit of my lack of memory is that *I attended the ARRAY
workshop in 2014*, but I have no memory of speaking with the NOVA researchers
present. I simply cannot believe that I would not have asked for clarification
about its full capabilities, and, if the answer had been promising, perhaps used
their developments more directly. However, what exactly happened is lost to the
mists of time, perhaps because of the other events of that summer - immediately
after attending ARRAY, I went to the [Oregon Programming Languages Summer
School](https://www.cs.uoregon.edu/research/summerschool/summer25/), and then
off to an internship at [LLNL](https://www.llnl.gov/).

Ultimately, NOVA did not directly influence Futhark, and the basic idea of the
Futhark research project was in place before NOVA was published. It did however
confirm to us that a standalone functional data-parallel language targeting GPU
execution was feasible and likely to work well. It would be interesting to one
day talk to the developers and figure out what exactly its capabilities were,
and why they did not continue their work, as NOVA did not go further than that
single paper.
