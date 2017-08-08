---
title: Syntactic Support for Roman Numerals
author: Troels Henriksen
description: The motivation behind Futhark's newest syntactical addition.
---

Futhark lives in a strange no-mans land.  Compared to highly tuned
low-level code written by experts, Futhark programs are slower, but
much more convenient.  Compared to libraries like Numpy, Futhark is
much faster and more powerful, but less convenient.  Futhark's niche
is thus problems that do not already have some finely tuned existing
solution, and which are too complicated to solve just by composing a
few simple linear algebra operations.  If we want Futhark to gain
adoption, we have to be pro-active in searching out domains with this
class of problems, and add features that cater to their special
circumstances.  This post describes one such effort.

"Computational History" is a field I did not know actually existed,
but there is `kind of a Wikipedia page
<https://en.wikipedia.org/wiki/Computational_history>`_ about it.  Its
practitioners (if they exist) would probably be predisposed to like
Futhark by the name alone.  Apart from easy parallel performance, what
would a computational historian need in a programming language?  As
all programmers know, it's important to be able to program close to
the domain.  Imagine you're a computational historian studying ancient
Rome, and you're typing in data about `Trajan's Column
<https://en.wikipedia.org/wiki/Trajan's_Column>`_.  Wouldn't you
prefer to keep in the ancient frame of mind and describe the year of
construction as CXIII instead of 113?  Just as a bit-fiddler wants to
be able to use hexadecimal or binary literals in their code, so would
a computational classicist clearly wish to use Roman numbers.  As a
result, we have `added Roman numeric literals to Futhark
<https://github.com/diku-dk/futhark/commit/e22745b2cd8b051d145601702501dfc4fa6121c5>`_.

Like all numeric syntax, they are purely a lexical feature.  In
Futhark, hexadecimal literals are prefixed with ``0x``, binary with
``0b``, and now Roman with ``0r``.  Thus, ``0rXV == 0xF``.  The usual
numeric post-fixes are supported, so the performance-savvy historian
studying the early classical period can write e.g. ``0rXIVu8`` to
compactly represent the death year of Caesar Augustus as an unsigned
8-bit integer.  Most GPU algorithms, and presumably many future
computational history algorithms, are bandwidth-bound, so this can be
a useful optimisation.
