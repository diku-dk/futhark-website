---
title: Futhark is a low level language
description: Is this title clickbait?  Read inside to find out!
---

One thing that makes discussions between programmers so lively is that
we lack firm objective definitions of terms such as "strongly typed"
or "low level".  A common recourse is to look for some snappy quote by
a dead computer scientist.  While Dijkstra tends to be my favourite
arms supplier when fighting about terminology, here we find Alan
Perlis ready to be of service:

> "A programming language is low level when it requires attention to
> the irrelevant." - [Alan Perlis, 1982](http://www.cs.yale.edu/homes/perlis-alan/quotes.html)

Of course, pithy quotes such as these tend to raise more questions:
*what* is irrelevant?  I think all programming languages require the
user to care about things that are not central to whatever problem
they are trying to solve, and so by Perlis' quote they would *all* be
low level.  Maybe so and that makes the definition less useful.
(Except to motivate constant progress towards ever-more high level
languages, which Perlis would likely strongly agree with.)  The
definition of "irrelevant" probably depends on the context, and even
languages that are by most standards considered quite high level may
have aspects that are of a low level nature.

## You can't have it all

Which brings us to Futhark, the most prominent among all functional
array language named after the Runic alphabet.  Futhark was originally
conceived as a very high level language for parallel programming,
although I'm not sure there was a shared understanding among the
initial participants about what that meant exactly.  As for me, I was
intoxicated by the experience of working on an optimising compiler at
all, and I was infused with naive ideas about the compiler being able
to do *anything!*  Just write the code and the compiler will figure
out how to make it run fast.

I still remember the precise situation where I was disabused of that
notion and became perhaps not that much wiser, but at least a good bit
more cynical.  During my master's studies I was in a meeting with my
advisers [Fritz Henglein](http://hjemmesider.diku.dk/~henglein/) and
[Cosmin Oancea](http://hjemmesider.diku.dk/~zgh600/), and we ended up
discussing the problem of multiplying multiple matrices.  Suppose we
have three matrices *A, B, C*.  Since matrix multiplication is
associative, we can multiply in any order we wish and still get the
right result: *(AB)C = A(BC)*.  But one order of operations may be
asymptotically much better than the other, depending on the sizes of
*A, B, C*.  In principle, a compiler could generate code that at
runtime uses to the most efficient ordering based on the actual sizes
observed.

Now, Futhark has neither matrices nor matrix multiplication as a
language concept.  You can model them using two-dimensional arrays and
`map`-`reduce` compositions (and it will run quite fast too), but the
compiler has no idea about the algebraic properties of the resulting
code, so there is no way it can start doing re-association
optimisations.  I recognised this at the time (I was naive, not
ignorant!), but mumbled something about optimisations such as these
perhaps being merely a special case of some more fundamental principle
that could be applied at the level of `map` and `reduce`.  I was
grasping at straws, and I knew it.  This was the first of many times
that I encountered useful optimisations that Futhark could *never* do,
but that might be possible in a *more* high level language.

While unpleasant at first, saying "no, we cannot do that and probably
never will" became quite a foundational principle for Futhark, and
helped conceptualise in my own mind what the language is fundamentally
*about*, and what the compiler will do for you.

## What Futhark is and is not about

So what are those principles?  What is the nature of Futhark?  At the
most basic level, Futhark is a language of array values, coupled with
a handful of primitive functions that transform arrays.  Futhark will
store these arrays in memory in a relatively straightforward way, and
it will execute the code you write more or less in the way that you
write it.  In particular, the compiler will not change your
algorithms, and except for pathological cases involving dead or
redundantly duplicated computation, it will not fiddle with the
asymptotic complexity of your program.

Futhark will also not mess around with data types: if you want an
integer, *you* have to decide how many bits you need.  There is no
[bignum](https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic)
type that tries to model mathematical integers (unless you write one
as a library).  There is also no type of rational numbers,
computational reals, or other exotic types that model mathematical
objects and allow you to avoid worrying about floating-point roundoff
or cancellation.  When you ask for an array, you get an array on the
heap, [even if it has a small constant size and it might really be
used as a tuple](2019-01-13-giving-programmers-what-they-want.html).
If your array is [mostly
zeroes](https://en.wikipedia.org/wiki/Sparse_matrix), too bad - they
still get stored individually in memory.  If you want a fancy sparse
representation, you have to write the code yourself, in terms of the
dense arrays that Futhark makes available.  The language provides
useful abstraction mechanisms for doing so (modules, polymorphism,
higher-order functions), but the compiler will ultimately not
understand the algebraic properties of your abstraction, and cannot
optimise based on them.

In this way, Futhark is definitely low level.  *Many* programmers
probably feel that fiddling with the number of bits in a float is well
inside the realm of Perlis's notion of *irrelevant*.

But it gets worse: unusually among functional languages, Futhark does
not support recursion.  When you want to work with a structure such as
trees, you have to model them as arrays.  There are [pretty cool
algorithms for doing
so](https://research.nvidia.com/sites/default/files/publications/karras2012hpg_paper.pdf),
but it definitely leads to significant "attention to the irrelevant"
if you just want to build a tree.  Similarly, irregular arrays -
arrays where the elements have different sizes - must also be encoded
using [various
techniques](https://futhark-book.readthedocs.io/en/latest/irregular-flattening.html).
Terribly irrelevant!

So at this point, what *does* Futhark even do for you?  Well, if you
tell it which arrays you want, it will figure out where and how to put
them in memory.  If you tell it about parallel operations such as
`map` and `reduce`, it will figure out how to execute them
efficiently.  And perhaps most importantly, if you write a bunch of
small functions using these concepts and then put them together, the
compiler will work very hard to remove the overhead of all those
intermediate results.  When you nest parallel constructs, it will work
very hard to [figure out a good way to map the levels of application
parallelism to the available levels of hardware
parallelism](2019-02-18-futhark-at-ppopp.html).  And the compiler will
look at how you actually traverse those arrays in memory, and ensure
they are stored in whatever order leads to the most efficient
traversal.

## On the clickbait title

Obviously, describing Futhark as a low-level language is a provocation
and not a serious attempt at placing it in some taxonomy.  But if you
compare the code that a Futhark programmer writes with the code that
ultimately gets executed, you will find that most of what Futhark does
is [boil away abstractions such as higher-order
functions](2018-04-10-futhark-0.4.0-released.html#higher-order-functions),
and generate a huge amount of tedious boilerplate for you.  The scalar
computation in the original program remains more or less unmolested.
Any array-typed variable can probably be found as a heap-allocated
slab of memory, except for the ones that are fused away entirely.  One
might even say that the heart of the Futhark compiler is little more
than a fancy macro expander with delusions of grandeur.

Ultimately, while I don't expect or hope that users will have to know
about everything the compiler does, they should have an understanding
about what it does *not* do.  The compiler was not made by a sorcerer;
it is not magical.  It is not even particularly smart.  It will take
care of array layouts and exploiting parallelism, but you are still
responsible for writing good code.  It will not touch your algorithms.

## And now for something completely different

I started writing this post over a month ago, but got sidetracked on
account of Putin's invasion of Ukraine.  I am agonised to see a
country with a long and difficult past struggle to emerge into liberty
and democracy, only to be dragged backwards into tragedy by the vain
ambitions of one man and the people he has misled.  I encourage all
readers to put pressure on their political leaders to ensure constant
and thorough support for the people of Ukraine in all ways, as they
fight for the hope of a free future.  I also suggest donating to the
humanitarian organisations active in Ukraine, or [even directly to the
Ukrainian defence forces](https://war.ukraine.ua/donate/).  Few
conflicts are truly black-and-white, and neither is this one, but the
utter blackness of Putin's naked aggression renders any Ukrainian
blemishes invisible in contrast. Слава Україні!
