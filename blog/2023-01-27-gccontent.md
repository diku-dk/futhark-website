---
title: Solving a parallel programming problem with a list homomorphism
author: Troels Henriksen
description: "Or the clickbait title: Outperforming C, Rust, etc with Futhark, list homomorphisms, an expensive GPU, and ignoring the rules."
---

One programming task I greatly enjoy is solving problems by devising
new reduction operators.  Futhark is slightly unusual among parallel
languages as it allows arbitrary reduction operators instead of
predefining just a few common cases (e.g. sum, product, maximum).
From time to time it is nice to be reminded that it is worth the
bother to maintain this capability. However, most programmers do not
have experience solving problems in this way, and don't know how to go
about it. In this post I will go through an example of how I analyse a
simple problem and devise a `map`-`reduce` composition that solves it.

The problem we will look at was devised by [Samuel
Lampa](https://genomic.social/@shl) and is called
[gccontent-benchmark](https://github.com/samuell/gccontent-benchmark).
We are given a big file that looks something like this:

```
>Y dna_rm:chromosome chromosome:GRCh37:Y:2649521:59034049:1
AGTTATCAGTGTACTGAAAAATTCCAGAAGTGCCTATTTTCAGAACAAATACTTGTTCTG
AATNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
NNNNNNNNNNNNNNNNNNNNNNNNNNAGACACCAAACTAACAAACAAGAGAGTATTCTAA
TGAATAAATATATAGACTTTATTGCACTTGTAGGAATATGATTTGATTTAATAAACTAAT
...
```

Our task is to count the percentage of C/G characters relative to all
C/G/A/T characters.  Further, lines beginning with `>` should be
skipped.  Finally, these files can be very large and it should not be
assumed that they will fit entirely in memory.  All the existing
implementations read it line by line, but I will admit up front that
we are going to cheat and read more at a time for performance reasons.
Also, we will slightly extend the problem and treat `>` at *any*
position as a comment extending to the end of line.

At first glance, this problem looks ill suited for Futhark.  IO and
text processing is not what the language was made for.  On second
glance, this assessment remains correct.  Futhark is most certainly
not necessary or perhaps even useful here.  But sometimes, life is
about doing unnecessary or useless things because they are fun.  In
fact, I would say life ought mostly be about that.

## Parallelising

Let us consider a file to be an array of ASCII characters.  We want to
compute a value of type

```Futhark
type count = {gc: i32, total: i32}
```

from which we can easily compute the percentage:

```Futhark
def pct (c: count) = f64.i32 c.gc / f64.i32 c.total * 100
```

We will eventually need a function for adding together counts, as well
as a zero count:

```Futhark
def count_add (x: count) (y: count) = {gc=x.gc+y.gc, total=x.total+y.total}
def count0 : count = {gc=0,total=0}
```

To figure out how to parallelise something like this, I like to start
by considering the case where the input is split up into two *chunks*,
each chunk processed independently into a partial result called a
*summary*, and these summaries then combined to form the final result.
Intuitively, a summary contains the partial result for a contiguous
chunk of the input (which as a special case can be *the entire
input*).  The main question is what these summaries should contain,
and how they should be combined.  (Later we'll talk about how to
compute the summaries in the first place.)

Suppose we define the summary as being just the count:

```Futhark
type summary = count
```

Unsurprisingly, this is not good enough.  Consider the following input
split in two at the blank space:

```
ATCA>AT CATTG
```

The summary for the first chunk is `{cg=1,total=4}`, because the two
characters following the comment are ignored.  The summary for the
second chunk is `{cg=2,total=5}`, because it has no way of knowing
that it is part of a comment begun in the first chunk.  This kind of
non-local information can only be propagated when we combine the
summaries, but in this case that it is impossible.  There is nothing
in the summary that records that the chunk ends in a comment.  We can
fix that by adding more information to the summary type:

```Futhark
type summary = {c: count, comment: bool}
```

For the example above, the two chunks now have these summaries:

```
{c={cg=1,total=4}, comment=true}
{c={cg=2,total=5}, comment=false}
```

Now we can define a function for combining two summaries:

```Futhark
def redop (x: summary) (y: summary) =
  {c = if x.comment then x.c else x.c `add_count` y.c,
   comment = x.comment || y.comment}
```

This computes the correct summary for the example above:
`{c={cg=1,total=4}, comment=true}`.  Unfortunately, it is still not
fully correct.  The `>` comments are *line comments* [as is
proper](2017-10-10-block-comments-are-a-bad-idea.html) and should
extend only to the next newline.  Ponder a new example, where we write
newlines as `\n`:

```
ATCA>AT CAT\nCATT
```

The comment starting in the first chunk shouldn't completely nullify
the second chunk!

To handle this, we modify our summary to contain *two* `count`s:

* A count for characters *preceding* a newline, which therefore should
  be nullified when the preceding block contains a comment.

* A count for characters *following* a newline, which should not be
  nullified by comments.

Further, the summary should record whether the chunk contains a
newline at all, so we can determine whether a comment starting in the
left chunk extends to the end of the right chunk.  The full type is as
follows:

```Futhark
type summary = { befnl: count,
                 aftnl: count,
                 hasnl: bool,
                 comment: bool
               }
```

For the most recent example we have these summaries:

```
{befnl={cg=1,total=4}, aftnl={cg=0,total=0}, hasnl=false, comment=true}
{befnl={cg=1,total=3}, aftnl={cg=1,total=4}, hasnl=true,  comment=false}

```

Out combination function is now:

```Futhark
def redop (x: summary) (y: summary) =
  {befnl = x.befnl,
   aftnl = if x.comment then x.aftnl `count_add` y.aftnl
           else x.aftnl `count_add ` y.befnl `count_add` y.aftnl,
   hasnl = x.hasnl || y.hasnl,
   comment = y.comment || x.comment && (!y.hasnl)}
```

Note how we use the `hasnl` field of the second chunk to decide
whether to propagate the `comment` field of the first chunk.

By repeatedly applying `redop` we can combine *many* summaries rather
than just two.  If `redop` is
[associative](https://en.wikipedia.org/wiki/Associative_property) (it
is), we can do so in parallel with a [parallel
reduction](https://en.wikipedia.org/wiki/Reduction_operator).

We can now combine a bunch of summaries.  How do we compute them in
the first place?  Intuitively, we need to split the input into chunks,
compute a summary for each chunk, then combine them.  It's essentially
[divide-and-conquer](https://en.wikipedia.org/wiki/Divide-and-conquer_algorithm).
How about simply splitting as much as possible, into *single-element
chunks*?  Since we can combine any number of summaries, this should
work fine.  We can compute these summaries simply by `map`ing this
function over the input:

```Futhark
def mapop (b: u8) =
    {befnl = match b
             case 'G' -> {gc=1,total=1}
             case 'C' -> {gc=1,total=1}
             case 'A' -> {gc=0,total=1}
             case 'T' -> {gc=0,total=1}
             case _   ->  count0,
    aftnl = count0,
    hasnl = b == '\n',
    comment =   b == '>'
  }
```

That means the entire thing ends up being a `map`-`reduce`
composition, where the neutral element is the summary for an empty
chunk:

```Futhark
def summary0 = {befnl=count0, aftnl=count0, hasnl=false, comment=false}

def gc (str: []u8) = reduce redop summary0 (map mapop str)
```

In the original problem description it was mentioned that the program
should process single lines at a time, because the dataset may be too
large to fit in memory.  Now, individual lines are probably too small
to be worth processing in parallel, but we can still abide by the *spirit* of
the rule by letting the user pass in a summary of a *preceding* chunk:

```Futhark
entry gc_chunk (s: summary) (str: []u8) : summary = s `redop` gc str
```

A driver program can then read chunks of the input file of whatever
size it wants and gradually feed them to the Futhark program through
its C API.  The driver program [can be seen
here](https://github.com/diku-dk/gccontent-benchmark/blob/futhark/futhark/gc.c).
(It is not really very interesting.)  I use a chunk size of 1MiB.
Larger is better, but not substantially so for reasons we're about to
get to.


## Performance

So how fast is it?  Just like [last time I did something like
this](https://futhark-lang.org/blog/2019-10-25-beating-c-with-futhark-on-gpu.html),
I must emphasise that Futhark is a poor fit for writing command line
programs with short runtimes.  Initialising the GPU easily takes
hundreds of milliseconds, which is going to be a significant fraction
of the total runtime.

Using the `time(1)` command to measure total process runtime, [this
hand-written C
program](https://github.com/samuell/gccontent-benchmark/blob/master/c.001/gc.c)
takes 0.631s on my Ryzen 1700X.  This is going to be our yardstick.
The Futhark program, compiled to *sequential* CPU code, takes 1.114s -
almost twice as long.  Not great, but also not a bad showing for a
high level language and an algorithm designed for maximum parallelism.

If we ask the Futhark compiler to generate *parallel* multicore CPU
code, the runtime drops to 0.233s - not too bad!

If we compile the Futhark program for GPU execution, then my AMD RX
7900 takes 0.630s to run it - same as sequential execution!  This is
clearly terrible, and the problem is that GPU initialisation *alone*
takes 400ms.  This is not a problem for long running processes, but
rather crippling here.  Even if we ignore the initialisation time, the
remaining 200ms are largely spent on ferrying data from main memory to
the GPU, rather than doing actual computation.  If we use [Futhark's
own benchmarking
methodology](https://futhark-book.readthedocs.io/en/latest/practical-matters.html#benchmarking),
which ignores startup costs and ensures the initial input data is
already GPU-resident, we can measure that the actual computation time
is a mere 0.023s.

Again, let me emphasise that Futhark is not well suited for this kind
of IO-oriented programming.  However, the core algorithmic problem
turned out to be nicely expressible as a `map`-`reduce` composition,
and I wanted to show how I go about constructing the necessary
operators.

## Some theory

The theory that links divide-and-conquer parallelism to `map`-`reduce`
compositions is the [theory of list
homomorphisms](https://sigkill.dk/writings/par/lhomo.html).  This
problem, and most interesting instances of `map`-`reduce`, are known
as *near homomorphisms* because they require us to carry around extra
information, in addition to the desired result, in order for the
reduction operator to work (e.g. the `comment` and `hasnl` fields).
In this post I used an intuitive, informal, example-driven approach to
derive the operator, but there is also a [principled
technique](https://link.springer.com/chapter/10.1007/3-540-61550-4_166)
for deriving it from a specification.  In practice, I find that the
principled approach much too tedious to actually carry out when doing
real programming (although I naturally still ask my students to do so
[in our course on data parallel
programming](https://github.com/diku-dk/dpp-e2022-pub)).
