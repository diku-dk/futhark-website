---
title: Large array literals
description: How do you deal with programs that are not only poorly written by normal standards, but are in fact not written by humans at all?
---

A PhD student here at DIKU, [William
Due](https://github.com/WilliamDue), is conducting research into data
parallel parsing (and eventually, algorithms data parallel compilation
more generally). So far, the most significant research artifact is
[Alpacc](https://github.com/diku-dk/alpacc), a tool that can given a
Context Free Grammar can generate an appropriate data parallel parser.
The generated parsers are expressed in Futhark (although Alpacc may
also grow a CUDA backend at one point). It's interesting work,
although not useful quite yet.

The generated parsers are table-driven, and due to the special
information needed to support parallel parsing (*way* outside the
scope of this post, but see [William's BSc
thesis](https://futhark-lang.org/student-projects/william-bsc-thesis.pdf)),
the tables are pretty large. For example, [this JSON
grammar](https://github.com/diku-dk/alpacc/blob/1aeca8bb530c63cc4a8a0a12a61fa5e6df8ff3da/grammars/json.alp)
results a program `json.fut` (8.8MiB) that contains a table with
917764 elements. At runtime, that's not so bad. Each element is an
`i16`, so the run-time size is less than 2MiB. Not ideal, but
tractable. The real problem is that this table is embedded in the
generated Futhark program as a single large array literal:

```Futhark
def compositions : [917764]i16 = [51222u16, 51222u16, 12332u16, 12312u16, ...
```

The resulting program, despite being quite small besides this array
literal, took 73s to compile. Much time was spent by the various
compiler passes inspecting each individual element of this array
literal and pondering what optimisations might be possible. Another
significant cost centre was the final C code generation, which uses
the library
[language-c-quote](https://hackage.haskell.org/package/language-c-quote)
to represent the syntax tree, which has a lot of overhead for each
syntactical element. In most Futhark programs, array literals are tiny
(because humans have to type them by hand!), and they can contain
arbitrary expressions (not just constants), which means the compiler
has good reason to look at their contents and try to optimise them.

This is not the first time large array literals have cropped up in
Futhark code. Years ago, [Martin Elsman](https://elsman.com) wrote [a
library for Sobol numbers](https://github.com/diku-dk/sobol), which
also contains a few enormous constants. Apparently at least *two*
people really want to write (or more accurately *generate*) programs
that look this way, so while it is tempting to simply state that
Futhark is a *programming language* and not a *data storage format*,
perhaps it is more productive to try to handle this kind of code
properly.

So why do people want to embed these array literals, rather than
storing them in [some more convenient
format](https://futhark.readthedocs.io/en/latest/binary-data-format.html))
next to the program, and loading them at runtime? The main reason is
convenience: it's lovely for a program to be completely
self-contained, while it's annoying to load data at runtime and ferry
around some "state" or "context" that contains the data - especially
for completely static data like lookup tables.

Making large array literals work well requires changes throughout the
compiler, as we must avoid, as much as possible, ever having to look
at the elements. In the parser, of course, that is impossible. While
Futhark's parser is not particularly fast, neither is it exceptionally
slow, and parsing `json.fut` takes 2.6s. This is by itself tolerable.
The problem is in the rest of the compiler.

To solve the problem, I added the notion of an "array value" to the
various program representations (both the source AST and the IR). An
array value is like an array literal, but while a literal can contain
arbitrary expressions, an array value can contain only constant
primitive values. I then modified the parser such that whenever it
encounters an array literal that consists *exclusively* of explicitly
typed numeric literals, it produces such an array value instead of an
array literal. The compiler was then extended to pass these array
values unmodified through all passes, without looking at the
individual elements (how much can you optimise a constant anyway?). In
the code generator, I added some [ad-hoc logic to generate the desired
C array literal
directly](https://github.com/diku-dk/futhark/blob/d7580aae5d4ecf0e9033f024be8cd10a2dd2e578/src/Futhark/CodeGen/Backends/GenericC/Code.hs#L369C5-L381),
rather than passing through the somewhat inefficient representation
and pretty-printer in language-c-quote.

The C code generation was the largest cost centre, and fixing that
took the compilation time from 73s to 14s. Adding the notion of array
values brought it further down to 6.1s, of which 2.6s is parsing. This
is pretty usable, and definitely makes Alpacc-generated code less
annoying to work with. I was a bit worried that we'd have add an
entire language concept to represent static data (perhaps like
[`#embed` in C](https://thephd.dev/finally-embed-in-c23)), and I'm
pleased we managed to avoid that for now.

The remark about only doing this for arrays that contain *explicitly
typed* number literals merits an elaboration. Like Rust, Futhark
allows numeric constants to be suffixed with their type, such as
`123i32`. If no suffix is present, type inference will figure out
which type is intended. When an array literal `[1,2,3]` is seen, we
cannot know which element type is intended until type checking, and so
the parser cannot construct an appropriately typed array value. In
contrast, `[1i32,2i32,3i32]` is unambiguous. It would certainly be
possible to wait until after type checking and then construct the
array value based on the inferred, but by then you've already paid the
significant cost of doing unification and type checking on the (in the
case of `json.fut`) nearly one million subexpressions constituting the
elements. My hunch is that almost all code that contains enormous
array literals is generated, and for a code generator it is no problem
to always add the type suffixes, and it simplifies the compiler.

At this point, the main thing I'm uncertain about is where to document
this special quirk of the compiler. It's not really a question of
semantics, so the [language
reference](https://futhark.readthedocs.io/en/latest/language-reference.html)
seems inappropriate. The [performance
guide](https://futhark.readthedocs.io/en/latest/performance.html) is
exclusively about run-time performance, but perhaps it is the best
place to put strange implementation details that are important to know
when you try to do strange things.
