---
title: Reflections on a PhD accidentally spent on language design
author: Troels Henriksen
description: I spent over three years on researching compiler optimisations, and accidentally also designed a programming language along the way.  Here are some thoughts on how that came about.
---

The majority of the development work on Futhark has been carried out
during my PhD work at `DIKU`_.  As I handed in my thesis in December
and successfully defended it in November (`read it here!`_), this feels
like a good time to reflect on the experiences I made.  I do not have
some golden advice to pass on to future generations of PhD students
(and in fact, I will comment only little on general PhD matters), but
perhaps some of my observations can prove useful to someone.  And if
nothing else, writing this may provide some catharsis.

.. _`DIKU`: http://diku.dk
.. _`read it here!`: /publications/troels-henriksen-phd-thesis.pdf

The reason behind the title of this post is that I did not originally
anticipate doing any work on programming language design - it just
happened by accident along the way.  Finally, I would say that I
consider myself incredibly blessed to have been given the opportunity
to work full-time for more than four years on what is essentially my
hobby.  While PhD programmes certainly have their downsides (long
hours, low pay, high stress), I can't imagine anywhere else I could
have had so much fun.

Early Days
----------

My involvement with Futhark started long before I became a PhD
student.  In the fall of 2012 I had been hired as a teaching assistant
in the compilers course at DIKU.  The co-teacher on the course,
`Cosmin Oancea`_, had designed a small data-parallel language and
implemented a compiler skeleton in Standard ML.  I was not very happy
with the design of the compiler - in particular, I found the mechanism
by which the type checker inserted type information in the AST to be
inelegant (basically, the AST had a bunch of ``option`` fields that
started out as ``NONE``, and became ``SOME`` after the type checker -
but without any static guarantees).  At the conclusion of the course,
Cosmin started looking for people to continue developing the language,
then called L0 (but I'll just call it Futhark for the remainder of
this post).  I had thought of a better way to represent the type
information, and I jumped at the chance to try it out in practice.  I
was not particularly interested in parallel programming at the time,
but I subsequently spent a few weeks putting together a compiler
because I found it fun (using Haskell, because I like it better than
Standard ML).  In fact, I found it so much fun that I eventually found
me working on it all the time.

.. _`Cosmin Oancea`: http://www.diku.dk/~zgh600/

At the time, the compiler did no optimisation, and generated only
sequential and leaky C code.  Furthermore, the language was crude.
Syntactically, it was a mixture of C and Standard ML, and for example
used curly braces to denote arrays (the horror!).  At the time, this
was fine - we had no desire to design a full programming language.
Rather, we saw ourselves as being in the business of designing
compiler optimisations, and Futhark was merely an intermediate
representation for a compiler, or perhaps a target for code
generation.  Either way, though Futhark did possess a textual syntax
so we could write benchmarks and test programs, we spent no time on
making it particularly pleasant or elegant.

After a few weeks of work in my free time, I was hired as a student
programmer to continue the work.  Eventually I also wrote my master's
thesis on Futhark, and I was hired as a PhD student in April of 2014,
a little more than a year after I first started work on the Futhark
compiler.

Starting my PhD
---------------

At the time, the compiler had only the basics: Fusion plus simple copy
propagation and folding.  The main architectural weakness was the lack
of a distinction between source and core language.  In retrospect,
this was an early design mistake, brought about by our refusal to
accept that we were in fact building a programming language.  The
initial months of my PhD were therefore spent on splitting the AST
definition in the compiler into a *source* and *core* version.  We
then augmented the latter with a notion of simple size-dependent
types.  This design turned out to be very robust, and it is still used
almost unchanged in the current compiler.  In general, I would say
that nothing is more important in a compiler than a good choice of
representation.  While it is not a practical strategy to simply pick
the best representation immediately, I would recommend designing the
compiler such that the internal AST representation can always be
tweaked to better fit the transformations being performed.  Decoupling
the IR from the source language is a good first step, but you may also
wish to structure the compiler such that it can use several different
representations internally (maybe even using a `Nanopass design`_, if
your implementation language makes that convenient).

.. _`Nanopass design`: http://nanopass.org/

Invigorated by the success of the size-dependent type system, we
decided to exercise it by constructing an elaborate optimisation for
reducing the cost of doing bounds checking.  While this resulted in `a
paper`_, the implementation was very complicated and time-consuming to
maintain.  Further, this work did not move us closer to our final
goal - efficient execution of Futhark on GPU - compared to the
low-overhead technique of simply disabling problematic bounds checks.
This was another lesson: take hacky shortcuts for anything not
directly related to your final goal.  You can always go back and
refine the luxury features later on.

.. _`a paper`: /docs.html#bounds-checking-an-instance-of-hybrid-analysis-pdf

The latter half of 2014 marked the beginning of undoubtedly the most
frustrating and difficult part of my PhD work, as we slowly started
putting together the building blocks we would need to generate GPU
code and handle nested parallelism.  The work was particularly
difficult because it had no immediate tangible effect.  We still
generated only slow, leaky, and sequential code, and the language
itself was neither elegant, nor practically useful.  Under these
circumstances, I found it hard to stay motivated, and it was hard to
convince myself as well as others that our work would eventually pay
off.  Looking back, I am surprised I did not quit, and I think it
would have been a good idea to structure the work to have more
short-term rewards.  This could have taken the form of creating a
simple and hacky compilation pipeline that could at least run
*something* on the GPU.  Alternatively, work on the compiler
infrastructure (tough and unrewarding) could have been interspersed
with work on the source language (easier and with immediate payoff).
Of course, the latter was hampered by the fact that we still refused
to admit that we were not just compiler researchers, but also language
designers.

But then the Compiler Worked
----------------------------

Eventually, the work paid off, and on the 29th of April 2015, the
first Futhark program to be compiled into OpenCL and executed
correctly on a GPU was run::

  fun [int] main ([int] a) = map(+2, a)

The compilation pipeline was still somewhat hacky and brittle, but
this proof-of-concept provided a huge surge of motivation, and
progress was rapid after that.  On May 3rd, the compiler could handle
matrix multiplication, which is essentially the equivalent of
Turing-completeness for an array language.  I spent the summer
designing and implementing our moderate flattening algorithm for
handling nested parallelism, and by mid-fall, we were compiling fairly
complicated programs into quite decent GPU code.

A particularly memorable boost of motivation came when we graduated
from only compiling our own test programs, and moved on to translating
benchmarks from Rodinia into Futhark.  In many cases, the GPU code
generated by the Futhark compiler was even able to outperform the
hand-written code.  While this clearly only happened because the
original code was flawed, it was still a quite encouraging experience.

This was the time when we really started *programming* in Futhark.
While we previously had variously small test programs and a few
guiding benchmarks, we were now porting thousands of lines of code.
But while we made a few language improvements - such as adding more
integer types in early 2016 - the language was still unpolished and
uncomfortable.  However, it was not quite so uncomfortable that I did
not want to take it for a spin on "real" applications.  At this time,
the only way to run Futhark code was to compile it to an executable,
which would then read input in a textual format on standard input, and
produce textual output on standard out.  Using this simple interface, I
was able to write some simple image processing demos, using a Python
program to read image files and convert them to the textual format,
but it was quite awkward.  Worse, it was *slow*.  Every time the
program ran, it would re-initialise the GPU, which can take several
seconds, and all data had to be copied back and forth for every run.
It was time to think a little about how to make Futhark *useful*.

Fortunately, we had an idea on how to accomplish this - and more
importantly, two motivated undergrads looking for a fun project for
their bachelors thesis!  Thus, during the spring of 2016, we developed
a compiler backend that generated not standalone binaries, but rather
reusable Python modules.  These modules then exported functions that
appeared like ordinary Python functions, but internally would offload
work to the GPU through the `PyOpenCL`_ library.  This allowed us to
easily invoke Futhark code from Python programs, with runtime speed
close to what we obtained with our C/OpenCL-based code generator.

.. _`PyOpenCL`: https://mathema.tician.de/software/pyopencl/

This, in turn, let us leverage Python's library ecosystem.  In
particular, we began writing several interactive visualisations, where
a Python program would call a Futhark function to produce a screen
image, and then use a Python library to `blit`_ the image to the
screen.  The impact of such demos is hard to overstate: While the
audience members at a presentation may nod their heads at descriptions
of how the compiler manages to transform some complicated nested
parallel program, what they *remember* is is a real-time visualisation
of the Mandelbrot set, spinning particles in an n-body simulation, or
a dynamically changing webcam filter.  And so, while Futhark was never
meant for graphics programming, or particularly optimised for
low-latency programs, we made a strategic effort to port/copy/steal a
variety of programs that could be used to grab people's attention.
Fortunately, our friends in the `Accelerate project`_ had already done
the hard part of the work, and their `collection of examples`_ is a
nice source of easy-to-read parallel programs, most of which come with
nice visualisations.  Porting these to Python+Futhark proved quite
simple.

.. _`blit`: https://en.wikipedia.org/wiki/Bit_blit
.. _`Accelerate project`: http://www.acceleratehs.org/
.. _`collection of examples`: https://github.com/AccelerateHS/accelerate-examples

Our efforts at visualising our own benchmarks proved less fruitful.
But please - if anyone knows of a flashy way to visualise `k-means
clustering in a 34-dimensional space`_, or `market parameter
calibration in the Heston model`_, please let us now!

.. _`k-means clustering in a 34-dimensional space`: https://github.com/diku-dk/futhark-benchmarks/tree/master/rodinia/kmeans
.. _`market parameter calibration in the Heston model`: https://github.com/diku-dk/futhark-benchmarks/tree/master/misc/heston

I learnt two lessons here: First, come up with something flashy for
capturing people's attention!  It does not have to be something that
fully demonstrates the potential of your work, just something that
people will remember for later.  Second, if you are in academia, take
advantage of the supply of students!  They are a wonderful source of
labour, if you can supply them with projects that they find
motivating.

Going Public
------------

The Python backend and the pretty visualisations it permitted
motivated us to present Futhark to the world.  I built a website (the
one you are reading), and submitted it to `/r/programming`_ in April
of 2016, where it made it to the top of the list.  The response was
far more positive than I had expected, and it was quite fun to read
people's take on our work.  Since Futhark is undeniably an applied
research project, getting feedback from practitioners outside of the
academic bubble is invaluable.  Since then, I have also given a talk
at the `Copenhagen Meeting Group for Functional Programmers`_ and at
`FOSDEM`_, and I hope to do more in the future.  Unfortunately,
academic success proved a little more elusive, and we had our first
major paper rejected by `ICFP`_ in 2016.  It was a pretty rushed
paper, so we were not very surprised, and it took two rewrites before
it was `accepted at PLDI` in 2017.

.. _`/r/programming`: https://programming.reddit.com
.. _`Copenhagen Meeting Group for Functional Programmers`: https://www.meetup.com/MoedegruppeFunktionelleKoebenhavnere/
.. _`FOSDEM`: https://fosdem.org/
.. _`ICFP`: http://icfpconference.org/
.. _`accepted at PLDI`: /blog/2017-06-25-futhark-at-pldi.html

By this time, Futhark was a programming language.  That was what the
website called it, and how I explained it to the outside world.  This
had been a gradual change, brought about by the fact that it's easier
to explain that "Futhark is a programming language that runs fast on
GPUs", than "Futhark is a compiler that can compile generate GPU code,
but the language is unimportant".  Unfortunately, Futhark still was
not a very good language.  The syntax was clumsy, there was no real
mechanism for abstraction, and many small conveniences - like local
functions - were not supported.  It had to change, and it did.

Some changes were superficial.  For example, we changed the function
application syntax to be based on juxtaposition rather than
parentheses.  While this caused some challenges, it was mostly
straightforward (although writing a program to transform the entire
test suite cost me a weekend).  We were far more challenged when we
started adding substantial new language features, in particular `the
module system`_ and `the record system`_.

.. _`the module system`: /blog/2017-01-25-futhark-module-system.html
.. _`the record system`: /blog/2017-03-06-futhark-record-system.html

Our language design efforts were aided by the fact that we already had
a well-working compiler.  Thus, whenever we added a new language
feature, we could immediately check whether it would inhibit
optimisations, or otherwise cause problems for code generation.  In
practice, we did this by restricting language extensions to the source
language, and requiring that all new features should be
straightforwardly compiled away into the core language.  I think there
is another lesson here: If designing a language where the success
criteria are primarily operational (performance, safety,
verifiability, etc), start by designing a bare-bones languages, with
only the most essential features.  Then, once you know how to write
the intended compiler, you can extend and improve the language.  The
experience gained while writing the compiler will help inform the
language design process, and ensure that features are not added that
will become impossible to implement.  While this *co-design* strategy
is not suitable in all cases, it is effective for those languages that
are really just glorified user interfaces to a powerful compiler.

The Future
----------

While I still see myself as principally an academic compiler
developer, language design has proven so much *fun* that we will no
doubt continue to improve on Futhark as a programming language.  We
will probably continue to be conservative, and only add features that
have already been tried by other languages.  This is partially because
of our limited manpower, partially because of our generally minimalist
sense of aesthetics, and partially because adding language features to
Futhark carries an extra tax, in that they may not restrict the
optimising power of the compiler.  For example, we are currently
working on adding support for higher-order functions, where we use
(simple) type rules to provide a guarantee that we can specialise away
all higher-order constructs early in the compilation pipeline.  In
contrast, a normal functional language could just represent a
first-class function as a function pointer paired with a
heap-allocated closure.  It is an interesting language design project
in its own right: how do you design a programming language that
*feels* high-level, but has just the right set of restrictions to
permit efficient compilation to restricted targets?  This is certainly
not a problem we expected to encounter when we started the Futhark
project, but it has become a welcome one.

Designing a language in a vacuum is difficult.  We can certainly use
our own sense of aesthetics to determine whether the language is
pleasant to use, but our own benchmarks and demo programs provide
little feedback on how Futhark works in a real-world setting.
Fortunately, we are cooperating with various academic and industrial
groups on experimenting with Futhark applied to real problems in real
code bases.  No doubt, this will also influence the design of the
language - I can already think of a few tweaks I'd like to make to how
a Futhark program is split across multiple files (but the details will
have to wait for another blog post).
