---
title: What it takes to add a new backend to Futhark
description: A reader asked and the Futhark devblog answers the call.
---

Recently Scott Pakin [suggested writing a blog post on how to add a
new backend to the Futhark
compiler](https://github.com/diku-dk/futhark/discussions/2224), and
since there's active fiddling with the backends at this very moment,
this is not a bad idea. Let us manage expectations up front: this will
not be a *tutorial* on adding a backend. I will not go into the deep
details on the specific internal APIs that should be used. Instead, I
will focused on the core representations, and give an idea about the
kind of work (often complicated) and magnitude (sometimes relatively
little) it takes to add a new backend. It's also somewhat open
precisely what a "backend" even means. There's a significant
difference in complexity between adding a command `futhark foo
bar.fut` that produces *something* based on `bar.fut` (very easy), to
implementing another C-like GPU backend (not hard, but you need to
touch a lot of pieces), to creating a fundamentally new backend for an
alien piece of hardware (depending on your needs, can be extremely
challenging).

I will still link pertinent pieces of the source code as applicable -
sometimes it is instructive just how simple (or simplistic) it is to
finally glue together the complex bits. The Futhark compiler currently
supports a fairly diverse set of targets (sequential CPU, multicore,
different GPU APIs, C, Python). To achieve this without undue
duplication of code and effort, the compiler uses fairly heavily
parameterised representations of the program being compiled. I'll try
to get the gist across, but the full details are very, well, detailed
(and I always feel like they should be simplified - it's not the
aspect of the compiler we're most proud of).

For a drier exposition, there is also [the internal compiler
documentation](https://hackage.haskell.org/package/futhark-0.25.27/docs/Futhark.html).

## Architectural overview

The Futhark compiler is written in Haskell.
