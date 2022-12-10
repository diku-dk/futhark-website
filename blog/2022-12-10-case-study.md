---
title: "A case study in parallelisation: Advent of Code 2020, day 10"
description: This problem does not look very parallel at first glance, but actually allows a quite elegant implementation.
---

I am currently participating in the [Advent of
Code](https://adventofcode.com/), with the goal of solving every task
in Futhark.  I [did this once
before](https://futhark-lang.org/blog/2018-12-25-futhark-0.8.1-released.html),
but I like to think that both Futhark and myself have improved since
then.  Inspired by [Snektron's solutions from last
year](https://github.com/Snektron/aoc21) I'm trying to do *everything*
in Futhark - including the input parsing - and also to come up with
data-parallel solutions, even when the problem is not really suited
for it.  I doubt Advent of Code considers parallelism to be a goal
when they come up with their problems, so there will certainly be some
that have to be solved sequentially, but so far I've managed to
parallelise all except one.  I will do a proper retrospective when
(if) I finish, but in this post I want to take a closer look at [the
day 9 problem](https://adventofcode.com/2022/day/9).  It is a problem
that at first glance looks very sequential, and to be honest is
probably *best* solved using a sequential algorithm, but a
[work-efficient](https://sigkill.dk/writings/par/cost.html#work-effiency)
algorithm actually turns out to be possible.

## Problem statement

I'll skip the elf metaphors and go straight to the problem.  We are
simulating two points in a discrete 2D grid, the *head* and the
*tail*.  These two points must never be more than one grid cell apart,
but they *may* occupy the same cell.  Whenever the head moves, the
tail must also be moved appropriately, to make sure it is still
touching.  Examples from the problem statement:

```
.....    .....    .....
.TH.. -> .T.H. -> ..TH.
.....    .....    .....

...    ...    ...
.T.    .T.    ...
.H. -> ... -> .T.
...    .H.    .H.
...    ...    ...

.....    .....    .....
.....    ..H..    ..H..
..H.. -> ..... -> ..T..
.T...    .T...    .....
.....    .....    .....

.....    .....    .....
.....    .....    .....
..H.. -> ...H. -> ..TH.
.T...    .T...    .....
.....    .....    .....

```

Given an initial position of the head and tail, we are then given a
sequence of head movements, and asked to simulate the corresponding
movement of the tail.  The final answer requested by the problems
statement is the number of distinct coordinates touched by the tail,
but the interesting part is the simulation itself.

## Analysis

At first glance, this looks pretty sequential, like most time-stepped
simulations.  However, there is hope: it is straightforward to, in
parallel, compute all the intermediate positions occupied by the head,
given a list of movements.  This is because the head movement is just
addition of 2D coordinate points, which is
[associative](https://en.wikipedia.org/wiki/Reduction_operator), and
so we can compute them with a scan.  For example, we represent a
movement upwards as `(0,1)`.  In Futhark we can implement it like
this:

```Futhark
type pos = (i32,i32)
def pos_add (a: pos) (b: pos) = (a.0 + b.0, a.1 + b.1)
def coords moves = scan pos_add (0,0) moves
```

Then we observe that after every movement of the head and
corresponding adjustment of the tail, the position of the tail can be
described relative to the head - and there are only nine possible
states (the eight neighbouring grid cells and the cell containing the
head itself).  If we can only compute the position of the tail
relative to that of the head, for every step of the simulation, then
we can find the absolute positions of the tail.

Okay, so how do we do that?  It turns out that the relative position
of a tail *after* a move depends on the movement of the head and the
relative position of the tail *before* a move.  For example:

* If the tail overlaps the head and we move the head right, then the
  new position of the tail is to the left.

  ```
  ....    ....
  .H.. -> .TH.
  ....    ....
  ```

* If the tail overlaps the head and we move the head left, then the
  new position of the tail is to the right.

  ```
  ....    ....
  .H.. -> .HT.
  ....    ....
  ```

* If the tail is down-left from the head and we move the head right, then
  the new position of the tail is to the left.

  ```
  .....    .....
  ..H.. -> ..TH.
  .T...    .....
  .....    .....
  ```

We can imagine implementing this as some function full of control
flow:

```Futhark
type dir = #U | #D | #L | #R | #UL | #UR | #DL | #DR | #C

def step (head_mov: dir) (tail_rel: dir) : dir =
  match (head_mov, tail_rel)
  case (#R, #C) -> #L
  case (#L, #C) -> #R
  case (#R, #DL) -> #L
  ...
```

The `tail_rel` is the state of our simulation, while the `head_mov`
the head motion of the next timestep (which does not depend on the
state).  We can simulate the movement of the tail simply by applying
this `step` function repeatedly.  Essentially we have a sequential
loop where this function is the loop body.  Where is the parallelism?

Now, there is a trick that allows you to "parallelise" any sequential
loop.  It's based on the observation that we can view the body of a
loop as a function from one loop state to another (like the `step`
function above), and if we construct a list of the functions
corresponding to each iteration of the loop, we can *compose* all
those functions in parallel, as function composition is an associative
operator.  The result of such a reduction will be a single function
from initial state to final state, while the result of a *scan* will
be a list of functions from initial state to corresponding
intermediate states - which is what we want to get the intermediate
tails.

The reason this doesn't *actually* let us naively parallelise any
sequential loop is that the result of function composition is really
just the two functions concatenated.  The result actually performing
such a reduction with composition is essentially just a big unrolled
loop - the final function is as sequential and as costly as the
original loop was.  *But* if we can come up with a different
representation for our function, one that allows a more efficient
definition of composition, then we might have a chance.

## The trick

The `step` function above has a quite small domain - the `dir` type
has only 9 possible values, for a total of *81* possible inputs.
Further, the `head_mov` argument does not depend on the simulation
state, but simply on which time step we are in.  This means we can
really think of *step* as a family of 9 functions of type `dir ->
dir` - a function with a domain of size *9*.  Such a function can
easily be represented as a lookup table!  A table that given a
current relative position produces the new relative tail position.

Let us number the potential relative positions from 0 to 8:

```
0 1 2
3 4 5
6 7 8
```

A value of 0 means *upper left* (`#UL` in the `dir` type) and a value
of *4* means *center* (`#C`).

For an *upwards* movement of the head, we can then explain the
movement of the tail with the following table:

```
3 4 5
6 7 8
7 7 7
```

For example, if the tail was previously *down and to the right*,
corresponding to position 8 in the first table, we look at the
bottom-right corner in the last table and find a 7 - meaning that the
tail will now be directly below the head.

Since each entry of these tables can be represented in 4 bits, we can
use a 64-bit integer to store a table:

```Futhark
type perm = u64
```

The name `perm` is because it is a permutation-like structure,
although it is strictly speaking not a permutation.

We define a small utility function for converting integer arrays to
this more efficient representation - uses of this function will fold
away any array literals:

```Futhark
def tovec (xs: [9]u64) : perm =
  let f i = xs[i32.u64 i]<<(i*4)
  in f 0 | f 1 | f 2 | f 3 | f 4 | f 5 | f 6 | f 7 | f 8
```

And a function for looking up a numbered field in one of these tables:

```Futhark
def perm_lookup (i: u64) (x: perm) : u64 =
  (x >> (i * 4)) & 0b1111
```

Then we can define a function that, given a head direction, produces
the corresponding table.

```Futhark
def perm (d: dir): perm =
  match d case #U  -> tovec [3,4,5, 6,7,8, 7,7,7]
          case #D  -> tovec [1,1,1, 0,1,2, 3,4,5]
          case #L  -> tovec [1,2,5, 4,5,5, 7,8,5]
          case #R  -> tovec [3,0,1, 3,3,4, 3,6,7]
          case #UL -> tovec [4,5,5, 7,8,5, 7,7,8]
          case #DL -> tovec [1,1,2, 1,2,5, 4,5,5]
          case #UR -> tovec [3,3,4, 3,6,7, 6,7,7]
          case #DR -> tovec [0,1,1, 3,0,1, 3,3,4]
          case #C  -> tovec [0,1,2, 3,4,5, 6,7,8]
```

And finally, the crucial piece - composition of `perm`s.  This is done
exactly the same way you compose permutations (and is the reason I
named the type `perm`):

```Futhark
def perm_compose (a: perm) (b: perm): perm =
  tovec [perm_lookup (perm_lookup 0 a) b,
         perm_lookup (perm_lookup 1 a) b,
         perm_lookup (perm_lookup 2 a) b,
         perm_lookup (perm_lookup 3 a) b,
         perm_lookup (perm_lookup 4 a) b,
         perm_lookup (perm_lookup 5 a) b,
         perm_lookup (perm_lookup 6 a) b,
         perm_lookup (perm_lookup 7 a) b,
         perm_lookup (perm_lookup 8 a) b]
```

That's basically it.  We can now compute a function for each
intermediate step simply by saying

```Futhark
map perm dirs |> scan perm_compose (perm #C)
```

where we use `perm #C` as the neutral element (when the head does not
move, neither does the tail).  The end result is an array of
functions, represented as an array of `perm`s.

[The full program](https://github.com/athas/aoc22/blob/main/9.fut) has
a bunch more code to then go from the relative tail positions to the
absolute positions, but it's embarrassingly parallel and not
particularly interesting.

This trick, of coming up with a bespoke representation of a
small-domain function and using that to parallelise an otherwise
sequential loop, is known to the literature before, but it is the
first time I have had a chance to try it myself.  I'm quite pleased
with how well it turned out.
