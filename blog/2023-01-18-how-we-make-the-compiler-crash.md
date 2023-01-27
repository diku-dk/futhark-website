---
title: How we make the Futhark compiler crash
description: One of the best choices we made in the development of the compiler.
---

A compiler that generates a program that runs, but produces a wrong
result, is a terrible thing to debug.  Unless the generated program is
very small, reading it is probably not practical, and even once you
manage to spot whatever is out of place, it is often very tedious to
determine the part of the compiler responsible.  Various techniques
exist for automating some of this, such as [automatic program
shrinking](https://embed.cs.utah.edu/creduce/) or using [optimisation
fuel](http://blog.ezyang.com/2011/06/debugging-compilers-with-optimization-fuel/)
to isolate problematic transformations, but this normally remains an
unenjoyable experience.

It is much easier to debug compiler *crashes*.  In these cases you can
look directly at the crashing compiler code and figure out which
invariants are being violated (at least if your compiler is written in
a memory-safe language *and why would it not be?*).  Of course there
is still the risk that the actual bug is far away from the code that
notices it - perhaps even in an entirely different compiler pass.
Matters are most enjoyable if the crash and the bug are found close
together.

This suggests a way of writing a compiler: It must be a fragile and
sensitive thing, barely alive, such that the slightest disturbance
leads to a fatal and noisy death.  The ideal compiler is only *just*
able to keep running.

So how do we instill such Freudian Thanatos in the Futhark compiler?
As most compilers, the Futhark compiler is structured as a composition
of *passes*, and the GPU compilation pipeline easily contains more
than 40 passes.  A pass can be seen as a pure function from program to
program. The language used internally by the compiler to represent
programs is *similar to* but not *identical to* the Futhark language
programmed by humans.  We call this language *Core Futhark* (in fact
there is an entire family of variants of Core Futhark, but let's keep
things simple by pretending there is only just the one).  Crucially,
Core Futhark *is a language*.  You can in principle write it by hand.
It has a type system, and we have a type checker that checks the
validity of Core Futhark programs.  This brings us to the main source
of Futhark compiler crashes: after *every* pass, we run the type
checker to verify that the output of the pass is sensible.  If it is
not, then the compiler spits out the faulty program and terminates.
Debugging can then begin by inspecting the program and figuring out
how the compiler came to produce such a mishappen thing.  We *know*
that the problem must lie in the final pass preceding termination,
which narrows the search considerably.  (There is of course also the
possibility that the type checker has a bug.)

This aggressive checking is the case even for production releases of
the compiler, and it cannot be disabled.  Having some kind of "linter"
or "verifier" is not unusual for compilers, but running it by default
is to my knowledge not standard practice.

Running a type checker after every pass is of course costly, and we'll
return to that.  But what do we gain?  To try to quantify this, I
looked at the compiler bugs reported on our [issue
tracker](https://github.com/diku-dk/futhark/issues) during the past
year.  This resulted in a total of 36 bugs that either crashed the
compiler or produced incorrect code.  Of these, only 7 were of the
latter kind, meaning we had 29 compiler crash bugs.  12 of these were
various ad-hoc crashes - the most common symptom being a panic due to
some entry missing from a symbol table.  The remaining 17 were caught
by the post-pass type checking - almost half of all compiler bugs
reported.  Of course, it is impossible to say whether these bugs would
have resulted in some other kind of crash if the type checker had not
been present, but I still think this shows the value of the compiler
having constant self-scrutiny, and immediately falling on its sword in
shame if it happens to produce an ill-typed program.

One non-quantifiable consequence of this persistent use of type
checking is that whenever we added new constructs to Core Futhark, we
had to think carefully about what its type rules would be.  This
forced us to be systematic and clear in our thinking.  While the Core
Futhark type system is not completely sound - in order to express
certain optimisations it allows some instances of [in-place
updates](2022-06-13-uniqueness-types.html) that we do not verify
completely - it has been very useful.

So what is the cost, really?  Years ago I measured the performance
overhead of type checking to be 20%.  As part of writing this post, I
figured I should check again and found that it varies from 2x to 5x
overhead, depending on the program.  That is *a lot more* than I
expected.  I'm actually not completely sure what has happened for the
impact to be that significant.  We have not added costly new analyses
to the type checker, so I suspect that the main difference is that we
have added a lot more small passes - and while they are perhaps
individually small and fast, they do force a full round of type
checking.  I'll have to look at whether we can optimise this a
bit. *Update from a few days later: this turned out to be a recent
regression due to misplaced strictness annotations.  After fixing
this, overhead is about 10%.*

No matter how much we optimise internal type checking, there will of
course still be a cost, and switching it off will certainly speed up
the compiler.  Our plan is to disable it once we are certain that the
Futhark compiler has no more bugs.
