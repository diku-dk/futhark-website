---
title: How we make the Futhark compiler crash as much as feasible
description: One of the best choices we made in the development of the compiler.
---

A buggy compiler that generates program that runs, but produces a
wrong result, is a terrible thing to debug.  Unless the generated
program is very small, reading it is probably not practical, and if
you manage to spot whatever is out of place, narrowing down the part
of the compiler responsible for generating it is often very tedious.
Various techniques exist for automating some of this, such as
[automatic program shrinking](https://embed.cs.utah.edu/creduce/) or
using [optimisation
fuel](http://blog.ezyang.com/2011/06/debugging-compilers-with-optimization-fuel/)
to narrow down the problematic transformations, but this normally
remains an unenjoyable experience.

It is much easier to debug compiler *crashes*.  In these cases you can
look directly at the crashing compiler code and figure out which
invariants are being violated (at least if your compiler is written in
a memory-safe language *and why would it not be?*).  Of course there
is still the risk that the actual bug is far away from the code that
notices it - perhaps even in an entirely different compiler pass.
Matters are most enjoyable if the crash and the bug are found close
together.

This suggests a way of writing a compiler: It must be a fragile and
sensitive thing, barely alive, such that the slighest disturbance
leads to a fatal and noisy death.  The ideal compiler is only *barely*
able to stay running.

So how do we instill thanathos in the Freudian sense in the Futhark
compiler?  As most compilers, it is structured as a composition of
*passes*, and the GPU pipelines easily contain more than 40 passes.  A
pass can be seen as a pure function from program to program. The
language used internally by the compiler to represent programs is
*similar to* but not *identical to* the Futhark language programmed by
humans.  We call this language *Core Futhark* (in fact there is an
entire family of variants of Core Futhark, but let's keep things
simple by pretending there is only just the one).  Crucially, Core
Futhark *is a language*.  You can in principle write it by hand.  It
has a type system, and we have a type checker that checks the validity
of a Core Futhark program.  This brings us to the main source of
Futhark compiler crashes: after *every* pass, we run the type checker
to verify that the output of the pass is sensible.  If it is not, then
the compiler spits out the faulty program, and terminates.  Debugging
can then begin by inspecting the faulty program and trying to figure
out how the compiler came to produce such a mishappen thing.  We
*know* that the problem must lie in the final pass preceding
termination, which narrows the search considerably.  (There is of
course also the possibility that the type checker has a bug.)

This aggressive checking is the case even for production releases of
the compiler, and it cannot be disabled.  Having some kind of "linter"
or "verifier" is not unusual for compilers, but running it by default
is to my knowledge not standard practice.

Running a type checker after every pass is of course costly.  What do we gain?
