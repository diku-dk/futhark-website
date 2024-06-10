---
title: How we test(ed) the Futhark compiler
author: Troels Henriksen
description: A description of how the testing tools for the Futhark compiler have improved over time.
---

In this post I will go through the evolution of the tools we use for
testing the Futhark compiler. Along the way, these also became
`futhark test`, [the user-facing tool for testing Futhark
programs](https://futhark.readthedocs.io/en/latest/man/futhark-test.html),
and although rather convenient, it is still pretty crude compared to
the testing tools you will find in other languages. This post is
perhaps most interesting to other language implementation hobbyists.

## Every program starts with a shell script

In the earliest days, when you ran a Futhark program it would read
things from standard input and write things on standard output.
(Actually, in the *extremely* early days, Futhark actually supported
impure functions for doing this, although this was quickly changed to
implicitly reading the arguments to the `main` function, driven by
their types.) At some point we realised that we weren't sure that the
compiler worked, and so we decided to add a way of running our test
programs.

The vision was simple: each program could be associated with an `.in`
file that would be fed to the program on stdin, and an `.out` file
that would be compared against whatever the program on stdout. If no
`.in` file was present, it was assumed that the program contained a
type error and should be rejected. This early support for negative
tests was remarkable considering how crude the system as a whole was -
[here is the shell script that implements
it](https://github.com/diku-dk/futhark/blob/667370911e3563ee2e16f445d4e595cd2270c9da/data/runtests.sh).

You will notice, for example, that the test programs are explicitly
listed (sort of) in the test runner itself. Also, the script does not
actually *run* the programs - it merely compiles them. I vaguely
remember that this testing script is derived from one I wrote when I
was a TA on [DIKU](https://sigkilll.dk)'s compiler course (which is
[incidentally also the root origin of
Futhark](2021-12-19-past-and-present.html)), but I am pretty sure that
one actually ran the test programs.

It took about a week to actually reach the point [where the rest
runner tested the
programs](https://github.com/diku-dk/futhark/blob/df66f8355ac8e072ee8d93aa6589f45b756e9766/data/runtests.sh) -
looking at the commit history, I spent most of that week making it use
more compiler passes, and presumably only implemented *running*
programs once the compiler stopped crashing. Note that the expected
and actual program output is compared *exactly*, with `cmp(1)`. We
will return to this.

A few weeks later [I added support for only type checking the test
programs](https://github.com/diku-dk/futhark/commit/f71c4cf980900cdf58df1649b2f86d7702cc8292),
which is very handy when hacking on the type checker. I suppose this
represents the point where we had added enough tests (or made the
compiler slow enough) that actually *running* the entire test suite
was too annoying. This option is still supported in `futhark test`.

## Moving to Haskell

This code didn't last very long however, as soon after I [rewrote the
test runner in
Haskell](https://github.com/diku-dk/futhark/commit/dd40268e7c84172aac2449bc0ba1debdf9935150).
I actually thought the shell script lasted a lot longer than a few
weeks, but the Git log doesn't lie. This test runner was still not a
part of Futhark proper however, but a completely separate Haskell
program you ran with `runhaskell`. The main advantage was that it
would perform multiple tests concurrently, as compared to the totally
sequential shell script. Looking at its architecture, it is actually
very similar to the modern `futhark test` tool. For unclear reasons,
it took almost a month before we actually [switched to using the
Haskell-runner by
default](https://github.com/diku-dk/futhark/commit/132e195613474d1befe7a1d436ecc03666b06201).
Good bye, shell script.

The next major change [was adding a terrible CI
setup](https://github.com/diku-dk/futhark/commit/e6fa896d9e8c8ac44cd2fce4c1f7c36e0da9ece0).
Specifically, I piggybacked on an IRC bot I was running to manage a
lunch club with fellow students. This IRC bot was already set up to
handle IRC notifications from GitHub whenever someone updated its
code. It was a small amount of work to also make this lunch management
bot [receive notifications from the Futhark (well, L0 in those days)
repository and run the CI
suite](https://github.com/athas/eggsml/commit/03676c2a337d4b75c4ba7a98c446d0958b2cbc33).
CI failures were reported by sending me an email. It was *super*
flaky, and I don't remember why I didn't do something more sensible.
This was in December of 2013, so hardly the stone age when it comes to
CI. It wasn't until over a year later that we [finally started using
Travis-CI](https://github.com/diku-dk/futhark/commit/9ec064114873eee244f7984494bfb7694fb0044f)
([and eventually in 2020, GitHub
Actions](https://github.com/diku-dk/futhark/commit/d99e8ecf6cc8c3cc2bcfc95343a0f83ebc6996a3)).

But back to the test runner. Its main problem at this point was that
it did an *exact* comparison of expected and actual output, which
meant it was very sensitive to pretty-printing details (this all
predates the [binary data
format](https://futhark.readthedocs.io/en/latest/binary-data-format.html)).
It was also very annoying when we started having multiple backends
that did not guarantee bit-level consistency for things like floating
point arithmetic. The first (weak) attempt at solving this was
[ignoring whitespace when comparing
outputs](https://github.com/diku-dk/futhark/commit/b3fecfd8ddbba4a17207d27a2dd5699e165b16e9),
but it wasn't until [February
2015](https://github.com/diku-dk/futhark/commit/1d97f5533ef592e9690b98549b9b99f0c1d588a0)
that the test runner actually started considering the output to be
Futhark *values*, rather than just arbitrary strings, in order to
compare floating-point values within an allowable tolerance. It's
quite remarkable how long it took to get rid of some of the misdesigns
of that initial shell script.

By this point, the test runner run both the interpreter and compiler.
This was a very good design, as having two implementations of the
language forced us to think about what programs were actually
*supposed* to do, rather than what they happened to do by accident.
However, eventually we started writing test programs that were too
slow to run in the interpreter, so it became possible to [turn them
off](https://github.com/diku-dk/futhark/commit/c5d1648e6b1c2a96f0416521bcae046b500c2638).
This is the start of the test runner growing a confusing array of
different *modes* in which it can be run. A few weeks later, we added
the obvious counterpart option that [only runs the
interpreter](https://github.com/diku-dk/futhark/commit/964193c01e96600f800733032a043ece43c25dc7).
These options are still supported in `futhark test`.

The first large change to the *design* of how test programs were
specified [came in April
2015](https://github.com/diku-dk/futhark/commit/6eda2025dedc45f6a88ec8d0695805c2a34575d2),
and removed the part where a program `foo.fut` was associated with
`foo.in` and `foo.err`. The problem (apart from being annoying to
juggle multiple files) was that it only allowed a single input/output
pair for each program. Instead, the test input and expected output
would now be embedded in the program itself, in the form of a comment,
as shown here:

```
// --
// input {
//   10
// }
// output {
//    [ 0 , 1 , 1 , 2 , 3 , 5 , 8 , 13 , 21 , 34  ]
// }
```

This is still essentially the design we use today, although `//`
became `--` and `--` became `==`. It did prove awkward for [test
programs with very large expected
outputs](https://github.com/diku-dk/futhark/blob/6eda2025dedc45f6a88ec8d0695805c2a34575d2/data/benchmarks/BlackScholes.fut), but was a marked improvement for the rest.

In the same month, the ad-hoc Haskell script tucked away in a
sub-directory [finally became
`futhark-test`](https://github.com/diku-dk/futhark/commit/668be565957dffe7687f530d49ffc99d7c9d2de3),
and was built using the same build system as the main compiler. This
was the start of providing testing tools that could be used by Futhark
programmers to test their own programs (as opposed to compiler hackers
testing the compiler), although it was still quite crude, and almost
completely undocumented. (But anyway, you wouldn't want to use
2015-era Futhark.)

The opening comment in `futhark-test.hs` was pretty clear about the
audience:

```Haskell
-- | This program is a convenience utility for running the Futhark
-- test suite, and its test programs.
```

## Beyond correctness testing

Until this point, the program then named `futhark-test` had been
focused on functional correctness - when run, did the programs produce
the right output? However, at this point we had spent a lot of time
adding optimisation passes, and (unwittingly) also on breaking
optimisation passes. Our testing tool could easily detect when an
optimisation produced incorrect code ([helped by the internal
consistency checks in the
compiler](2023-01-18-how-we-make-the-compiler-crash)), but it had no
way of detecting when we broke an optimisation pass to such an extent
that it simply *did not change the program*.

Inspired by some tests I found in the LLVM test suite, I became
interested in being able to express that the program resulting from
optimisation should some loosely specified shape. I didn't want to
hardcode the entire expected representation, because the fine details
about variable naming and operation ordering are not (necessarily)
intended to be stable, but I wanted to say things like "this program
should consist of two loops at top level, the latter with a reduction
inside".

Instead of doing the work myself, I proposed the construction of such
a tool as a Bachelor's Project at the department ([the start of a
beautiful tradition](2024-02-03-quantifying-student-projects.md)), and
the skilled [Maya Saietz](https://tayacan.dk/) developed a tool where
you provided a high level AST "pattern", and the tool check checked
whether that pattern matched the program produced by the compiler.
Maya's work was quite good, but a bit overkill - and it was difficult
to express that a given construct was *not* allowed to appear in some
position (e.g., demand the absence of bounds checks). Instead, [I
added a "quick
hack"](https://github.com/diku-dk/futhark/commit/a6c0d0864bddad8f5eef2ce627c79e28ba9ef65a)
(still around in essentially unchanged form, naturally) where you can
simply specify how many of a specific kind of AST node are allowed in
the program. For example, this might specify the absence of bounds
checks:

```
structure { Assert 0 }
```

And this might specify that the program contains only a single
SOAC (which we might use to test fusion, another thing that broke often):

```
structure { SOAC 1 }
```

Nesting was handled by naming the nodes appropriately. For example, a
`SOAC` inside another `SOAC` would be tallied as a `SOAC/SOAC` node
(as well as two plain `SOAC`s - the outermost and the innermost one).
This "quick hack" has proven extremely useful, and perhaps merits a
more detailed post one day.

We also [added a notion of
"tags"](https://github.com/diku-dk/futhark/commit/709f432622a417661b1dd407a9c98ff56aeb7741),
where programs (and workloads) could be associated with arbitrary
*tags*, which would be excluded for a specific run with an `--exclude`
option. This was (and is) used for programs that do not work with a
specific backend, or for datasets that are too slow. (The sequential
Python backend, despite ostensibly being "compiled", is particularly
slow for nontrivial workloads.)

## Benchmarking

In early 2016, Futhark was becoming good enough that we started
wondering how fast it was - so [we added
`futhark-bench`](https://github.com/diku-dk/futhark/commit/ac9a62cb5ff719787c2c8e398bf6c31fca2be4d3).
It accepted the same kinds of files as `futhark-test`, but instead of
merely checking for correctness, it ran each program a bunch of times
and reported the resulting performance. In later years, when I have
had to benchmark other systems, I have often appreciated that we
(relatively) early on decided on a single fully automated way in which
Futhark programs should be benchmarked.

Around this time, since we wanted to benchmark programs with large
(ish) amounts of data, the MSc student Rasmus Wriedt Larsen [designed
and implemented a binary data
format](https://github.com/diku-dk/futhark/pull/295) that we still use
(almost) unchanged. Again, simply specifying a simple format (which is
incidentally also easy to read and write from other languages) has
proven enormously convenient. For example, the [`futhark
dataset`](https://futhark.readthedocs.io/en/latest/man/futhark-dataset.html)
and [`futhark
datacmp`](https://futhark.readthedocs.io/en/latest/man/futhark.html#futhark-datacmp-file-a-file-b)
tools can be used for generation, conversion, and comparison of data
files. At this point, test programs were also able to indicate that
the input (or output) data should be fetched from another file, rather
than being embedded in the program itself.

We have a bunch of auxiliary tools for benchmarking purposes, such as
[`futhark
benchcmp`](https://futhark.readthedocs.io/en/latest/man/futhark.html#futhark-benchcmp-file-a-file-b)
that can be used for comparing results, and
[`plot-bench.py`](https://github.com/diku-dk/futhark/blob/master/tools/plot-bench.py)
for visualising results, but I will leave a discussion of those for
another post.

## Server mode

At this point, `futhark-test` and `futhark-bench` were essentially
complete. Certainly, small things were added, such as testing multiple
entry points in the same program, or [the ability to test the presence
of specific compiler
warnings](https://github.com/diku-dk/futhark/commit/1dc03211d1322d3eae9f69a023211a9fd90c3ad1),
and they eventually became subcommands under the names `futhark test`
and `futhark bench`, but a modern Futhark programmer could probably
use them without noticing much difference.

Since the beginning, testing had been based on a simple operational
principle: a Futhark program read input on stdin and produced output
on stdout. The format had changed from text to binary, and options had
been added through which the program would report its runtime, but
running a Futhark function still implied starting the program from
scratch. It also meant testing was limited to the values that could be
represented in the binary data format, which was exclusively arrays of
primitive values - no records, tuples, or sum types.

Another problem with this approach was that it could be slow.
Initialising the GPU context could easily take several seconds
(potentially much more for large programs), which was detrimental for
programs that consisted on a large number of functions with
(relatively) small individual runtimes, [like this reduction
microbenchmark](https://github.com/diku-dk/futhark-benchmarks/blob/986b9e109f2a25a2e79ba949309706470e7d51f0/micro/reduce.fut).
It would be better if a Futhark program could be kept running (and GPU
context intact), and have it run multiple entry points during its
lifetime. This was added in early 2021 in the form of [Futhark server
mode](2021-01-18-futharkscript.html). The most visible feature
provided by server mode was `futhark literate` ([now used for our
collection of examples](http://127.0.0.1:8000/examples.html)), but it
also had significant impact on testing and benchmarking.

In particular, it was now faster - a *lot* faster for benchmarks with
many datasets, like the microbenchmark linked above. But a more subtle
change was that input needed no longer be expressed in as arrays of
primitives. Instead, it could be computed with *another* Futhark
function, and passed directly to the entry point being tested, without
having to go through a deserialisation/serialisation step. (And even
when necessary, the server protocol also came with support for
serialising *arbitrary* Futhark values to byte streams.) This meant we
could now write test programs like this:

```Futhark
-- ==
-- entry: doeswork
-- script input { mkdata 100i64 } output { 5050.0f32 }
-- script input { mkdata 10000i64 }
-- script input { mkdata 1000000i64 }

entry mkdata n = (n,map f32.i64 (iota n))

entry doeswork n arr = f32.sum arr + f32.i64 n
```

Here the `doeswork` function is the actual function being tested, and
`mkdata` is some arbitrary function that generates data. The test
stanza will run the expression `mkdata 100i64` to generate the input
(the runtime is not counted when benchmarking), which is then passed
to `doeswork`. While the example above is trivial, this is very useful
for providing arbitrarily complicated input data (although the result
must still be of a simple type). It is also quite useful that we can
test or benchmark with large amounts of synthetic data, without having
to store it on disk, by generating it dynamically in Futhark.

## What about unit tests?

All of the discussion above concerns *integration testing*, where the
full functionality (or close to it) of the compiler is exercised. What
about unit testing, where the functionality of individual small
components is verified? I am hardly averse to unit testing in general,
but for compiler development, I must admit that I find integration
tests to provide more *bang for the buck*. In particular, compiler
passes often take quite complicated inputs (entire ASTs), for which
the representation is not stable. Keeping unit tests for such
functions updated is not impossible, but it is time consuming.

By writing small test programs, and perhaps augmenting with things
like the `structure` tests above, we can obtain many of the benefits
of unit testing, with less maintenance overhead. Futhark does have a
[small collection of unit
tests](https://github.com/diku-dk/futhark/tree/master/unittests),
mainly for verifying the functionality of particularly subtle core
components, like [graph
colouring](https://github.com/diku-dk/futhark/blob/master/unittests/Futhark/Optimise/MemoryBlockMerging/GreedyColoringTests.hs)
or [index function
operations](https://github.com/diku-dk/futhark/blob/master/unittests/Futhark/IR/Mem/IxFunTests.hs)
(for the [array
representation](2024-03-06-array-representation.html)). However,
compared to the 2293 test programs, the 2985 source lines of unit
tests do look pretty meagre.

We also have [a collection of manually written library
tests](https://github.com/diku-dk/futhark/tree/master/tests_lib) for
testing the C and Python APIs, as well as some [completely ad hoc
tests](https://github.com/diku-dk/futhark/tree/master/tests_adhoc)
that are just shell scripts that check various things in the auxiliary
tooling. These are not terribly interesting.

## Wrapping up

Futhark is a project that even in the best case [will never have many
resources](2018-06-18-designing-a-programming-language-for-the-desert.html).
Therefore, all our tooling has been built with a sense of minimalism
and simplicity, and often sacrificing flexibility. Yet I think we have
ended up with tools that work pretty well for our purposes, and where
we can usually get our work done. The main missing feature is some
form of property based testing, which I hope we can add one day.

Although not originally developed for that purpose, the testing and
benchmarking tools are also [fairly easy to explain to
users](https://futhark-book.readthedocs.io/en/latest/practical-matters.html).
In most other languages, testing tools are libraries that are built in
the language itself. This is not possible Futhark, because it is not a
general-purpose language, so we had to build tools that are outside
the language itself. This can easily result in tools that are just
plain bad, but I think we managed to do alright.
