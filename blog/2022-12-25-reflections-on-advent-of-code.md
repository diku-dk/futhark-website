---
title: Reflections on Advent of Code 2022 in Futhark
description: Someone else did it last year, so we had to do it again.
---

[Advent of Code](https://adventofcode.com/) is a website by [Eric
Wastl](http://was.tl/) that since 2015 has published a daily
programming puzzle from the 1st to 25th of December.  It is also a
contest, where people compete to see who can finish them first.  I
don't like programming competitions myself, but I enjoy the social
aspect of AoC where you can discuss solutions with your friends and
colleagues, and most of the problems are simple and interesting enough
that they don't take too long.  Futhark is not a good choice for
solving these kinds of puzzles - there is no guarantee that any
meaningful parallelism is present, and the dearth of libraries for
basic data structures somethings make solutions more tedious than one
would prefer.  Still, I decided to give it a try.

I [also did Advent of Code in Futhark in
2018](2018-12-25-futhark-0.8.1-released.html#advent-of-code).
However, because Futhark is not a very good language for string
processing, I wrote Python programs to parse the text input and
generate values in Futhark's native data format.  But then in 2021,
Snektron did [Advent of Code in Futhark *including*
parsing](https://github.com/Snektron/aoc21) (probably because this was
not nearly as difficult as [implementing a compiler in
Futhark](https://github.com/Snektron/pareas)).  I decided I also had
to do that.

And so I did!  [Here is Advent of Code 2022 fully implemented in
Futhark.](https://github.com/athas/aoc22) The puritan will note that
there still is a [tiny Python
program](https://github.com/athas/aoc22/blob/main/txt2fut.py) that
prepends a header so that Futhark will be able to recognise a file as
a byte array.  The input parsing was much less problematic than I
expected, and for some problems were actual the only place where
parallelism was possible!  The main task is splitting the input into
strings, and I just so happened to have [recently written a word
splitter](https://futhark-lang.org/blog/2021-10-16-explicit-existentials.html#one-somewhat-interesting-usage-of-explicit-existential-quantification)
that was easy to convert to a line splitter.  Beyond that, ad-hoc
parsing methods were quite sufficient.

The Advent of Code problems are not specifically designed for parallel
execution, but I was amazed at how many of them actually permitted
meaningful use of parallel algorithms this year - although the problem
input sizes were usually much too small for parallelism to be
beneficial.

Bottom line up front: I can definitely recommend designing your own
programming language and then solve Advent of Code problems with it.
Here follows a brief summary of my observations for each problem.

* [Day 1](https://adventofcode.com/2022/day/1)
  ([.fut](https://github.com/athas/aoc22/blob/main/1.fut)): Frankly
  much more complex than I expected for a Day 1 problem, involving
  both segmented reductions and radix sort.  I was a bit worried here.

* [Day 2](https://adventofcode.com/2022/day/2)
  ([.fut](https://github.com/athas/aoc22/blob/main/2.fut)): Solving a
  bunch of rock-paper-scissors problems in parallel.  Quite trivial.

* [Day 3](https://adventofcode.com/2022/day/3)
  ([.fut](https://github.com/athas/aoc22/blob/main/3.fut)): Also a
  fairly trivial map-reduce, where I'm mostly proud of using bit
  vectors to represent sets of a small domain, then bitwise-or to
  perform set union.  Low-level bit fiddling is delightfully
  convenient in Futhark.

* [Day 4](https://adventofcode.com/2022/day/4)
  ([.fut](https://github.com/athas/aoc22/blob/main/4.fut)): Had the
  first complex parsing problem (each line must be split into fields),
  which I did sequentially.  Beyond that, it was another map-reduce.

* [Day 5](https://adventofcode.com/2022/day/5)
  ([.fut](https://github.com/athas/aoc22/blob/main/5.fut)): Probably
  the worst parsing experience of the entire series, and the actual
  problem (simulating a crane moving around stacks of crates,
  [Hanoi](https://en.wikipedia.org/wiki/Tower_of_Hanoi)-style) is
  completely sequential.  I do know of [one
  person](https://github.com/namibj/advent_of_code_2022/blob/a3e43c93fa7840a4ff52cbb4a8888ad373d86a80/src/day5_fut.fut#L203-L474)
  who managed to come up with a [list
  homomorphism](https://sigkill.dk/writings/par/lhomo.html)
  formulation of the problem, but it is very complicated.

* [Day 6](https://adventofcode.com/2022/day/6)
  ([.fut](https://github.com/athas/aoc22/blob/main/6.fut)): Again a
  map-reduce, and again using bit vectors to represent sets.  I
  enjoyed this one.

* [Day 7](https://adventofcode.com/2022/day/7)
  ([.fut](https://github.com/athas/aoc22/blob/main/7.fut)): Oh no,
  this is very inappropriate for Futhark.  Here we have to reconstruct
  a tree given a description of a walk of the tree.  Fortunately,
  Aaron Hsu has written [an entire PhD
  thesis](https://scholarworks.iu.edu/dspace/handle/2022/24749) about
  tree processing in array programming, and I was able to use his
  technique (which is APL folklore I think) to construct an ancestor
  tree.  I think my implementation is slightly less efficient than it
  could be - particularly the sequential `while` loop per node where I
  search left in the node array to find a parent - but it worked fine
  for this problem.

* [Day 8](https://adventofcode.com/2022/day/8)
  ([.fut](https://github.com/athas/aoc22/blob/main/8.fut)): This
  problem is about determining line-of-sight, and when reading the
  problem description I immediately recalled Guy Blelloch discussing
  exactly this problem in his paper [*Prefix Sums and their
  Applications*](https://www.cs.cmu.edu/~guyb/papers/Ble93.pdf)!  The
  solution is a lovely use of minimum-scans in various directions
  across the grid.  I think this is one problem where the
  bulk-parallel Futhark programming style leads to significantly
  *nicer* code than just typing out an imperative loop.  I am quite
  happy with this one.

* [Day 9](https://adventofcode.com/2022/day/9)
  ([.fut](https://github.com/athas/aoc22/blob/main/9.fut)): Not so
  difficult to do sequentially, very tricky in parallel.  So tricky,
  in fact, that I [wrote a dedicated blog post about
  it](2022-12-10-case-study.html).

* [Day 10](https://adventofcode.com/2022/day/10)
  ([.fut](https://github.com/athas/aoc22/blob/main/10.fut)): Normally
  simulating a virtual machine is a pretty sequential problem, but
  when the only operation is incrementing a register
  ([associative](https://en.wikipedia.org/wiki/Associative_property)),
  you can do it in parallel - including using a scan to also compute
  all intermediate states.

* [Day 11](https://adventofcode.com/2022/day/11)
  ([.fut](https://github.com/athas/aoc22/blob/main/11.fut)): I see I
  left a comment stating *"This one is Death"*, but I don't recall
  exactly why.  The parsing is certainly messy.  I managed to
  parallelise it partially, although it is not work efficient.  Part 2
  of this problem can only be solved if you recognise a certain
  property of prime numbers, which is not the kind of barrier I like
  in AoC problems (more on this in a bit).

* [Day 12](https://adventofcode.com/2022/day/12)
  ([.fut](https://github.com/athas/aoc22/blob/main/12.fut)): First
  [BFS](https://en.wikipedia.org/wiki/Breadth-first_search) of the
  year - or at least the first that I recognised.  I suspected that
  BFS's would probably crop up many times after this, so I decided to
  [actually put together a small BFS
  library](https://github.com/athas/aoc22/blob/main/bfs.fut), but then
  I never used it again.  Oh well.

* [Day 13](https://adventofcode.com/2022/day/13)
  ([.fut](https://github.com/athas/aoc22/blob/main/13.fut)): *Very*
  nasty.  Probably my worst solution this year.  The problem is
  basically about defining a certain comparison function on nested
  lists, which Futhark is very unsuited for.  I ended up writing
  something similar to what I would write in Haskell, but it results
  in excessive allocation in Futhark, due to the use of arrays instead
  of lists.

* [Day 14](https://adventofcode.com/2022/day/14)
  ([.fut](https://github.com/athas/aoc22/blob/main/14.fut)): Despite
  the fact that [falling sand simulators can actually be parallelised
  quite nicely](https://github.com/athas/diving-beet), AoC problems
  tend to prefer a simpler model of sand physics that invariably
  requires a sequential algorithm.  Only the input parsing is parallel
  here.  [Also uncovered a compiler
  bug.](https://github.com/diku-dk/futhark/issues/1798)

* [Day 15](https://adventofcode.com/2022/day/15)
  ([.fut](https://github.com/athas/aoc22/blob/main/15.fut)): Lovely
  combination of various map-reduces.  Seems a bit slow, so I'm
  probably missing some mathematical trick, but that's OK - you don't
  have to be good at mathematics if you ~~are beautiful~~ have an
  expensive GPU.

* [Day 16](https://adventofcode.com/2022/day/16)
  ([.fut](https://github.com/athas/aoc22/blob/main/16.fut)): This
  problem was about finding a needle in an astronomically large
  haystack - a heuristic search, basically.  You simply *cannot* solve
  this without doing something clever and nonobvious.  I did not like
  this problem at all, and many of my friends and colleagues stopped
  at this point (see also the break in the [global
  stats](https://adventofcode.com/2022/stats)).  I'm sure this problem
  was fun for people who care about the competitive part of AoC, or
  who have enough practice with programming puzzles to quickly come up
  with a workable heuristic, but for me the social aspect of AoC is
  lost when the problems become so time-consuming that the people
  around me no longer have the time to participate.

* [Day 17](https://adventofcode.com/2022/day/17)
  ([.fut](https://github.com/athas/aoc22/blob/main/17.fut)): Tetris
  simulation.  The most sequential of them all.  Solution of part 2
  depends on detecting fixed points using a heuristic.  I was very
  worried at this point that the remaining AoC problems would all be
  trick puzzles (although it turned out that the remaining problems,
  with one exception, were absolutely *delightful*).

* [Day 18](https://adventofcode.com/2022/day/18)
  ([.fut](https://github.com/athas/aoc22/blob/main/18.fut)): I should
  probably have used a BFS for this one, but I used a
  [stencil](https://en.wikipedia.org/wiki/Cellular_automaton) instead!
  Definitely a brawns-over-brains case, but I feel that I abused
  stencils less this time than in 2018.

* [Day 19](https://adventofcode.com/2022/day/19)
  ([.fut](https://github.com/athas/aoc22/blob/main/19.fut)): Similar
  to Day 16, although slightly easier.  I was really tired of these
  kinds of problems, so I didn't bother tweaking my heuristic much.
  This has the slowest runtime of all my solutions (about 7s in
  parallel on my machine), partially because my state deduplication is
  very crude (constant sorting).

* [Day 20](https://adventofcode.com/2022/day/20)
  ([.fut](https://github.com/athas/aoc22/blob/main/20.fut)): Shifting
  elements around in a circular sequence.  The most elegant way of
  implementing this is with doubly-linked lists, so I used arrays and
  crude index arithmetic.  Not elegant, not efficient, and not
  meaningfully parallel.  Still fun, though.

* [Day 21](https://adventofcode.com/2022/day/21)
  ([.fut](https://github.com/athas/aoc22/blob/main/21.fut)): This
  problem is about evaluating an expression tree.  My implementation
  is parallel but not efficient, as I repeatedly inspect *all* nodes
  to see whether their children have computed a value yet.  The second
  part is (essentially) about finding a value such that the formula
  becomes zero.  I solved this with Newton's Method and Futhark's
  recent support for [Automatic
  Differentiation](https://www.autodiff.org), which is extremely
  overkill, but made this one of my favourite problems this year.
  It's always nice when you get an excuse to pull out the fun toys.

* [Day 22](https://adventofcode.com/2022/day/22)
  ([.fut](https://github.com/athas/aoc22/blob/main/22.fut)): Folding a
  cube and then walking around on its surface.  Inherently sequential.
  I'm not very experienced at programming with coordinate space
  transformations, so my solution is quite hacked-up.

* [Day 23](https://adventofcode.com/2022/day/23)
  ([.fut](https://github.com/athas/aoc22/blob/main/23.fut)): Best
  solved with a stencil, so of course I used a (sort of) spatial data
  structure instead.  Very enjoyable and with lots of meaningful
  parallelism (even if my constant work to maintain the spatial
  structure is not very efficient).

* [Day 24](https://adventofcode.com/2022/day/24)
  ([.fut](https://github.com/athas/aoc22/blob/main/24.fut)): Also very
  delightful.  Essentially a BFS through a cylinder, where each
  cross-sectional slice of the cylinder can be computed in constant
  time from the initial slice.

* [Day 25](https://adventofcode.com/2022/day/25)
  ([.fut](https://github.com/athas/aoc22/blob/main/25.fut)):
  Straightforward map-reduce.  Gave me a reason to consider how to
  convert to bases where digits can be negative, which I've never had
  a chance to do before.

I am overall pleased with my implementations.  Also, I encountered far
fewer compiler bugs this year than in 2018, so it seems that Futhark
is actually getting better.  I think it is a shame that Day 16 (and
probably 19) weeded out so many participants, because the following
problems were much more tractable and fun.
