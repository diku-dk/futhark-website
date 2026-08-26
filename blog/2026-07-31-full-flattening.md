---
title: Full flattening of nested data parallelism
description: We finally caught our white whale, and now we can compile any Futhark program to GPU code (minor fine print applies).
---

Bottom line up front: this long blog post describes the culmination of a
years-long effort that means that we can now, modulo compiler bugs and a few
unimportant edge cases, compile *any* Futhark program to parallel GPU code.

## Introduction

One of the most interesting implementation challenges in Futhark is how to map
*nested parallelism*, in which parallel operations can contain other parallel
operations, to *flat parallelism*. This challenge arises because much
interesting hardware (such as GPUs) does not efficiently support nested
parallelism, and even on CPUs, arbitrary nesting may have significant run-time
costs. Futhark has for a long time supported flattening only a limited (although
important) subset of programs, called *uniform nested parallelism* (as opposed
to *nonuniform*), which meant that there were programs that we were unable to
compile to GPU code. In 2022 I began an [experimental implementation of "full
flattening"](https://github.com/diku-dk/futhark/pull/1740), which as of this
writing is the longest open pull request on the Futhark repository. I got far
enough to build a proof of concept, but the sheer amount of effort involved led
to the work stagnating - until now. Largely due to the efforts of students, we
are now close to having a complete reimplementation of flattening in Futhark.

The main contributions have been by [Cornelius
Sevald-Krause](https://di.ku.dk/english/staff/?pure=en/persons/700865) who
implemented function and `match` lifting [in his BSc
thesis](https://futhark-lang.org/student-projects/cornelius-bsc-thesis.pdf), and
particularly by Amirreza Hashemi whose MSc work has been a whirlwind of
finishing *everything else*. This includes various crucial optimisations in
order to reach functionality and performance parity with the [old
flattener](https://github.com/diku-dk/futhark/blob/a3f103a43256b847cfd0a13dca70e818b2f34204/src/Futhark/Pass/ExtractKernels.hs).
I cannot overstate how impressive it is to come up with a systematic and
maintainable replacement for a ten-year-old compiler pass that is full of
optimisation heuristics.

This post will introduce the basic problem of flattening, go through some of the
various challenges, and explain how we have resolved them in the
soon-to-be-merged work on the Futhark compiler. Flattening has a long academic
pedigree, and I will try to link relevant other work, but this post is not a
systematic survey of prior work. I will ignore most of the nontrivial
bookkeeping that a compiler needs to do to actually implement all of this stuff,
and instead focus on the intuition of how programs are transformed.

I should mention up front what the goal is of this work: the new flattening
transformation should not reduce performance for programs that already worked
with the old implementation, but we are less concerned with the performance of
programs that did not work before. As we shall see, achieving *truly* good
performance on arbitrarily nonuniform nested parallelism is a major research
problem in itself, and one that we intend to address incrementally in the time
to come.

Also, this is a long post. I have tried to make everything here relatively
high-level, but you may want to skip to the reflections at the bottom if things
drag on too much for your taste.

## Basics of flattening

To motivate flattening, we'll be assuming a slightly caricatured view of what a
GPU can do. Expanding to full GPU functionality does not fundamentally change
the story; it just means I would need more convoluted examples. Also, despite
this post focusing on GPUs, you can mentally substitute "efficient but
restricted processors" instead - these concerns crop up with any kind of vector
processing.

In the language of functional programming, we'll start by assuming that a GPU
can efficiently execute a `map` where the function contains sequential code, and
where all memory for intermediate results can be allocated in advance. To avoid
writing `zip`/`unzip` all the time, we'll also suppose that our `map`s accept
any number of arrays. This, for example, is something we can efficiently execute
on a GPU:

```Futhark
map (\x y -> x + y) xs ys
```

(Incidentally, when describing the transformations to follow, it would be much
simpler if `map` accepted its arrays *before* the lambda, like we do in
Futhark's IR - but I didn't want to go that far in a blog post.)

It is easy to imagine that this can be turned into a GPU program with one thread
per iteration of the `map`. Apart from a single `map` with sequential code,
we'll also allow any nesting of `map`s. So this is also efficient:

```Futhark
map (\xs ys -> map (\x y -> x + y)
                   xs ys)
    xss yss
```

It is similarly easy to imagine that these `map`s can be collapsed, with one GPU
thread for every iteration of the innermost `map`.

In addition to arbitrary sequential code, we will also allow the innermost
operation to be a *single* parallel operation, such as a scan, a reduction, a
transpose - the precise collection doesn't really matter. Then this is also
efficient:

```Futhark
map (\xs -> reduce (+) 0 xs)
    xs
```

Essentially, "efficient GPU code" means a tower of `map`s on top of some single
parallel primitive operation (or sequential code). The reason these patterns are
efficient is because we know how to generate efficient GPU *kernels* from these
patterns. This has some relation to
[skeletons](https://en.wikipedia.org/wiki/Skeleton_(computer_programming)), but
we don't generally use that term, since we intend to use these patterns as a
reasoning mechanism for the compiler, rather than a user-visible programming
model. In the actual compiler, we of course have dedicated constructs to
reliably express "a `map` with something specific inside of it", but to minimise
the syntax in this post, I will mostly stick with the towers-of-`map`s.

Of course, Futhark programmers might not write programs that look like that.
Such bold and free-thinking people might instead write this:

```Futhark
map (\xs -> let y = reduce (+) 0 xs
            let zs = map (+y) xs
            in zs)
    xss
```

This does fit one of the patterns we discussed: if we discard the parallelism of
the inner `map` and `reduce`, then this is a `map` containing sequential code.
This is an important choice when compiling a parallel language: you don't *have*
to exploit all the parallelism. Semantically, most parallel languages don't
*require* parallel execution in order to be correct; it's just an option the
implementation can take. Of course, we should not go overboard - a parallel
language that does not exploit *any* parallelism might well be accused of
[violating the contract](2022-01-27-cost-models-are-contracts.html).

In this case, we can rewrite the program to be one that is semantically
identical, but is expressed in terms of the efficient patterns. To make it clear
what is going on, first let us add some type annotations, where we assume that
the shape of `xs` is `[n][m]`:

```Futhark
map (\xs -> let y : i32 = reduce (+) 0 xs
            let zs : [m]i32 = map (+y) xs
            in zs)
    xss
```

Then we split apart the outer `map` into two separate `map`s, with the first one
returning an array of intermediate results that are passed into the second
`map`:

```Futhark
let ys =
  map (\xs -> reduce (+) 0 xs)
      xss
let zss =
  map (\xs y -> map (+y) xs)
      xss ys
```

These two `map`s now match the patterns that we know how to turn into efficient
GPU code. The transformation we did here is often called *loop distribution* or
*fission*, and is the counterpart to loop fusion. For a purely functional
language, the following equation holds:

```Futhark
map f ∘ map g = map (f∘g)
```

(The rule is more convoluted once multiple input arrays are allowed, but let us
ignore that.) Usually we apply the rule left-to-right in order to perform fusion
and avoid having to store intermediate arrays in memory, but we can also apply
it right-to-left, in which case it provides us a way to decompose complex `map`s
into sequences of trivial `map`s that just perform one single operation (or a
compound one - we can apply fission to the degree we want, which I will return
to). This is the main technique we use for cutting apart an arbitrarily complex
data parallel program into smaller manageable chunks. As far as I know, it was
pioneered by Guy Blelloch in the early 90s as part of his work on
[NESL](https://www.cs.cmu.edu/~scandal/nesl.html), although it was (and is)
sometimes called "vectorization", because its output, when fully applied,
consists of `map`s and `scan`s with trivial operations inside, which can be seen
as "vector operations". However, there are still two major problems we need to
address before we have all the components of a working flattening algorithm.

## Irregular intermediate arrays

Consider this program:

```Futhark
map (\m -> let xs : [m]i64 = iota m
           let y : i64 = reduce (+) 0 xs
           in y)
    ms
```

Recall that `iota m` produces an array `[0, ..., m-1]` of size `m`. This program
has the same issue as above, in that the parallel operations are not perfectly
nested. So let us try to fission (fise?) the `map`:

```Futhark
let xss : [n][???]i64 =
  map (\m -> iota m)
      ms
let ys =
  map (\xs -> reduce (+) 0 xs)
      xss
```

Notice the question marks in the type of the `xss` binding - what size should we
put there? Intuitively, the operation `map (\n -> iota n) ns` produces an
*irregular* array, where the rows have different sizes, depending on the values
of the input array `ns`. One might expect that `map (\n -> iota n) [1,2,3]`
produces the array `[[0],[0,1],[0,1,2]]`, but unfortunately, Futhark does not
allow irregular arrays; neither in the source language nor in our IR. The
reasons are motivated by performance, as once you have irregular arrays, you
either need to represent arrays as arrays-of-pointers, or lose random access
indexing. Neither are palatable when you want to target vector processors. That
means we don't want to allow irregular arrays in the source language, nor in the
IR.

This issue is the root of the limitation on our original flattening
transformation, which is that it cannot handle *nonuniform nested parallelism*,
meaning parallelism of a size that varies between iterations of outer parallel
dimensions. In the example above, `m` is local to the outermost `map`, so the
`iota` and `reduce` (both of which have a parallel width of `m`) are irregular.
Our only choice would then be to sequentialise this code. Now, in the actual
compiler, the `iota` would be fused into the `reduce` (using a representation I
will not go into), but if we ignore that part, it seems like we are violating
one of our constraints, which is that all memory for intermediate results must
be preallocated - and here we need an array of size `m`, where `m` can be
different for each thread. In this case, the problem can be solved by an
inspector that extracts a *slice* of the program that computes all intermediate
sizes (but not any other values), which is then used to compute memory
requirements and organise intermediate results in memory. This can be done
*sometimes* (roughly, when there is no tricky business with sequential loops),
but it is a best-effort kind of thing, and it does not bring back parallelism.

Anyway, the ability to handle irregular arrays is clearly crucial to the general
goal of handling nonuniform nested parallelism. The solution is to figure out a
representation of irregular arrays as flat (one-dimensional arrays), which still
allow the parallel execution of the operations we need. The central building
block is an encoding of a two-dimensional irregular array as a *data vector* and
a *flag vector*, both of the same size. The data vector is merely the rows of
the array concatenated, while the flag vector indicates whether its
corresponding element is the beginning of a new row. Example:

* **Nested irregular array:** `[[1], [2,3], [4,5,6]]`
* **Value vector:** `[1,2,3,4,5,6]`
* **Flag vector:** `[true, true, false, true, false, false]`

(The restriction to two-dimensional arrays is without loss of generality: we can
always collapse extra inner dimensions and have auxiliary arrays telling us how
to reconstruct them. For simplicity, we will not go into further detail.)

We call this a *segmented array*, and the "irregular rows" are called
*segments*. The array above has three segments, of size 1, 2, and 3
respectively. In practice, we often use certain additional auxiliary arrays, for
tracking the size of each segment (the *shape vector*, in this example
`[1,2,3]`), the beginning offset of each segment in the data vector (the *offset
vector*, `[0,1,3]` here), and arrays associating each element of the data array
to its segment index (`[0,1,1,2,2,2]`) and in-segment index (`[0,0,1,0,1,2]`).
These can all be efficiently computed from the flag vector, however.

The fundamental operation on this representation is the *segmented scan*, which
can be defined in terms of the standard `scan` as follows:

```Futhark
def segscan 't [n]
            (op: t -> t -> t)
            (ne: t)
            (flags: [n]bool)
            (vals: [n]t) : [n]t =
  let pairs =
    scan (\(v1, f1) (v2, f2) ->
            let f = f1 || f2
            let v = if f2 then v2 else op v1 v2
            in (v, f))
         (ne, false)
         (zip vals flags)
  let (res, _) = unzip pairs
  in res
```

We can apply it to the representation of the irregular array above to obtain the
result of (inclusively) scanning each segment, concatenated into a single array:

```
> segscan (+) 0 [true, true, false, true, false, false]
                       [1,2,3,4,5,6]
[1, 2, 5, 4, 9, 15]
```

This is the value array for a segmented array representing
`[1,2,5]++[4,9]++[15]`, using the original flag array unchanged. Since [scans
are such an important building block in parallel
algorithms](https://www.cs.cmu.edu/~guyb/papers/Ble93.pdf), it is no surprise
that once we can scan these segmented arrays, we can do all kinds of other
things as well. For example, we can do a segmented *reduction* by first
performing a segmented scan, then extracting the final element of each scanned
segment:

```Futhark
def segreduce 't [n]
              (op: t -> t -> t)
              (ne: t)
              (flags: [n]bool)
              (vals: [n]t) : ?[m].[m]t =
  segscan op ne flags vals
  |> zip (rotate 1 flags)
  |> filter (.0)
  |> map (.1)
```

```
> segreduce (+) 0 [true, true, false, true, false, false]
                         [1,2,3,4,5,6]
[1, 5, 15]
```

Similarly, we can construct a segmented `iota`, which takes as input a *shape
vector* of type `[]i64`, and morally maps `iota` over this array. The
construction is based on the observation that a normal `iota` can be written as
an [exclusive prefix sum](../examples/exclusive-prefix-sum.html) over an array
comprising `n` copies of 1:

```Futhark
def iota n = expresum (replicate n 1)
```

A segmented `iota` is then just a segmented scan over a segmented array where
the data vector is all `1`, and the flag vector has a `true` when a new `iota`
begins, which can be determined by an exclusive prefix sum of the shape array.
Since our segmented scan is inclusive rather than exclusive, we have to subtract
from each element at the end:

```Futhark
def segiota [k] (ns: [k]i64) : ?[m].[m]i64 =
  let m = i64.sum ns
  let offsets = expresum ns
  let flags = spread m false offsets (replicate k true)
  in segscan (+) 0 flags (replicate m 1)
     |> map (\x -> x - 1)
```

Incidentally, the segmented `iota` of a shape vector gives the in-segment index
for the segmented representation.

Essentially, whenever we talk of *segmented `foo`*, we mean an operation that
intuitively performs the job of `map`ing an operation `foo`, either consuming or
producing a segmented array. As long as these segmented operations follow our
rules for what we can turn into efficient (or just executable) GPU code, then we
are fine - and you may note that all the segmented operations described so far
are fully parallel and completely flat, in the sense that they do not contain
any nested parallelism, despite in some sense operating on nested structures. In
other fields, a segmented operation is sometimes called a *batched* operation,
although sometimes this implies that the segment sizes are all the same.

This is then the core of how Futhark's new flattening transformation works: use
fission to split `map`s into `map`s of very simple operations, which temporarily
introduces a notion of irregular arrays, then rewrite each of those `map`s to
their corresponding segmented form, which consume and produce arrays in the
segmented representation. This means that as long as we have a *flattening rule*
telling us how to generate segmented code for `map foo` for any possible `foo`,
then we can flatten any program. This is a nice compositional property that
means we can treat each language construct in isolation.

The next problem, of course, is that some of those `foo`s are mighty tricky.

## Flattening control flow

The trickiest situation occurs when `map`ing a conditional. Futhark's
intermediate representation has some complicated forms of control flow
(multiway-branching and [loops](../examples/loops.html)), but to illustrate the
problem and solution, we'll just deal with a plain `if`. The problem occurs when
we have code like this:

```Futhark
map (\b x -> if b then foo x
                  else bar x)
    bs xs
```

Here `foo x` means some arbitrary code that uses `x`, not an actual function
`foo` (we'll get to that problem below). Fission won't help us further, as the
`if` is not a composition of smaller functions. What we need is a flattening
rule for `if`.

The approach we use was invented by Blelloch (as so many things in this field),
and is called *branch packing*. The idea is to partition the iterations of the
`map` into those where the condition is true, and those where the condition is
false, execute each of those separately, and combine the results. The price is
that we execute both branches separately and spend time partitioning and
recombining data. Handwaving a little bit, we flatten code like the above into
this:

```Futhark
let (xs_true, xs_false) = partition_inputs bs xs
let xs_true' = map (\x -> foo x) xs_true
let xs_false' = map (\x -> bar x) xs_false
let result = combine_inputs bs xs_true' xs_false'
```

In practice, `partition_inputs` also has to return some auxiliary data that
`combine_inputs` uses to recombine the results in the original order. It's not
hard, just fiddly. The two `map`s may also need further flattening of course, as
`foo` and `bar` can be arbitrary code.

Loops are handled similar in spirit - keep an array of which iterations of the
map have yet to finish, and keep going until they are all done. The shape of the
flattened code is an outer loop whose body is parallel. It takes a lot of data
movement to repeatedly filter out the ones that are done, so it is not
particularly efficient - we'll look at some more efficient special cases below.

## Flattening functions

But what if `foo x` really *does* mean applying the function `foo` to `x`?
Unsurprisingly, for a functional language, function applications *do* occur.
Although [Futhark's current inlining strategy](2024-10-28-inlining.html) means
we could perhaps ignore them, one of the goals of improving flattening is to
eventually be less crude with regards to inlining.

The flattening rule for functions is conceptually quite simple: an expression
`map f xs` is flattened to a call to the *lifted function* `f' xs`, where `f'`
is generated from `f` in a systematic manner. The nomenclature of "lifting" is
because we "lift" a function of type `a -> b` to be a function of type `[]a ->
[]b`, that is, the function `f'` is "segmented `f`". Because we want to deal
with segmented arrays, and functions of more than a single argument, the true
transformation at the type level is more complicated: each scalar parameter is
turned into a (normal) one-dimensional array, and each array parameter is turned
into its segmented representation (flag and value vector), and similarly for the
return type. The body of the function is then flattened as if it was inside a
`map` nest.

It is a very convenient (and to me a little surprising) property that flattening
composes so nicely, and it even turns out that recursive functions don't need
any special handling - it just works. This is due to the *very* nice property
that lifting an already lifted function is *the lifted function itself*, meaning
we do not need to generate more than one lifted function, no matter how deeply
nested it may occur.

At this point in the post, and to the best of my knowledge, we have reproduced
Blelloch's flattening as it existed in NESL in the mid 90s, and which was also
implemented for such languages as
[cuNESL](https://dl.acm.org/doi/10.1109/ICPP.2012.21), which compiled a subset
of NESL to GPUs years before [work began on
Futhark](2021-12-19-past-and-present.html). While flattening is (almost)
preserving of the time asymptotics of the source program (there are some edge
cases for conditionals I won't get into), the performance on real hardware is
not good when flattening is applied naively. The reasons are basically due to
locality: flattening tends to increase array sizes asymptotically, and produce a
lot of very small parallel kernels, each of which manipulate large arrays in
memory. Let us now try to address some of these issues.

## Cheaper replication

Looking under the rug just a bit, we see that one thing I swept under it was how
to handle free variables in the functions we `map` with. I mentioned that as
long as we had a flattening rule for `map foo` for every `foo`, then we could
flatten arbitrary programs, but in the general case `foo` is a lambda that can
access arbitrary variables in scope, like for example this matrix
multiplication:

```Futhark
let res =
  map (\xs -> map (\ys -> let zs = map (*) xs ys
                          in reduce (+) 0 zs)
                  yss)
      xss

```

Applying fission, we are left with this:

```Futhark
let zsss = map (\xs -> map (\ys -> map (*) xs ys) yss) xss
let res =
  map (\zss -> map (\zs -> reduce (+) 0 zs)
                   zss)
      zsss

```

You should note the big three-dimensional `zsss` array, despite matrix
multiplication only involving two-dimensional arrays. This overhead, where the
three-dimensional *code* nesting of the original program results in
three-dimensional *data* nesting in the intermediate results is a common
downside of flattening. I usually describe it as flattening turning *time* into
*space*. It does not affect the asymptotics, as matrix multiplication is already
cubic time, but it obviously affects real performance tremendously.

In actual Futhark, this issue does not occur, because we do not treat the `map`
and `reduce` as separate constructs - they are fused into a combined form in the
IR, and treated atomically. But as we shall see, that does not solve the entire
problem.

For simplicity, we do not want our flattening rules to be concerned with free
variables. In order to establish the property that a lambda has no free
variables, we replicate each free variable and instead pass it as an additional
input to each map. Writing `rep` as a shorthand for replicating the necessary
amount, we get:

```Futhark
let zsss = map (\xs yss' -> map (\ys xs' -> map (*) xs ys) yss' (rep xs))
               xss (rep yss)
let res =
  map (\zss -> map (\zs -> reduce (+) 0 zs)
                   zss)
      zsss
```

The new parameters have been marked with a prime symbol, and are in some sense
only administrative - `yss'` really is just `ys`. The reason I don't just write
`replicate` here is that the arrays to be replicated may in some cases be
irregular themselves due to fission, in which case the replication is a
*segmented replicate* - a parallel operation, but a somewhat complicated one.

One property of our implementation is that we have not extended the IR in any
way. This means that when we generate (say) a segmented `iota`, we generate a
function that contains IR code corresponding to the scans, scatter, etc. that we
need. The consequence is that the compiler does not have a high level
understanding of the code that is produced by flattening, and it does not
understand that fancy segmented replicates can in many cases be simplified away
(e.g., indexing a segmented replicate is equivalent to an appropriate indexing
of the original array). Perhaps that will change in the future, but for now we
have no way to clean up the mess, so perhaps it is better to avoid making a mess
in the first place.

In our flattening algorithm, we therefore track some arrays as "replicated"
using a symbolic representation at compile-time, and handle them specially
whenever they are passed as inputs to inner `map`s. Whenever we would be forced
by unfortunate circumstance to actually return these to outside code, we could
always manifest them, but this turns out to be needed very rarely. Also, this
issue occurs only for variables that are free in a lambda, but bound inside an
enclosing `map`. Variables that are free in the entire nest, such as global
variables, need not be replicated.

I should note that a different approach to tackling the space blowup issue for
flattening is to replace `map`ing of arrays with streaming of sequences, which
allows for incrementally producing flattened data, rather than all-at-a-time.
This was investigated by Frederik M. Madsen [in his PhD
thesis](http://hiperfit.dk/pdf/meisner_madsen_thesis_revised.pdf). This requires
a rather different language (and solving different research problems), so it is
not a direction we took.

### An aside on Data Parallel Haskell

Another solution comes from [Data Parallel
Haskell](https://wiki.haskell.org/GHC/Data_Parallel_Haskell) (DPH) and is
discussed in the paper [Work efficient higher-order
vectorisation](https://dl.acm.org/doi/10.1145/2364527.2364564), and is based on
a different representation of segmented arrays that distinguishes "physical"
from "virtual" segments. This differs from Futhark's implementation in that it
also exists at run-time (and is therefore more flexible). As an aside, I will
note that DPH is probably the most thorough treatment of flattening since
Blelloch's original work. A lot of the DPH work was highly inspirational to our
own efforts, although DPH spent a lot of energy on tackling a richer source
language than we are interested in (in particular, they [can handle recursive
data
types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/fsttcs2008.pdf)).

Ultimately, DPH did not perform all that well in practice, and it was
[ultimately
abandoned](https://mail.haskell.org/pipermail/ghc-devs/2015-January/007986.html)
due to lack of funding. My *impression* is that DPH struggled because it relied
on sophisticated fusion after applying flattening in order to avoid excessive
intermediate results, and this fusion optimisation never panned out. Futhark
does things slightly differently, in that we aggressively perform fusion
*before* flattening, and then try hard not to fission *too much* during
flattening, such that the flattened code does not need any fancy optimisation
(although we still [perform low-level memory
optimisations](2022-11-03-short-circuiting.html), of course).

DPH had some very important offshoots however, such as the array libraries
[Repa](https://hackage.haskell.org/package/repa) and
[vector](https://hackage.haskell.org/package/vector), and the embedded language
[Accelerate](https://www.acceleratehs.org/). Accelerate in particular is
important prior work for Futhark as a whole, as it demonstrated just how nice
(and fast) functional array programming can be. Accelerate did not (and does
not) support nested parallelism, perhaps because of its authors' experience with
DPH, and this is one reason why support for nested parallelism was such a mark
of distinction for Futhark when we first released it.

## Vectorization avoidance

DPH did make one very important observation that we pretty much copied wholesale
for Futhark's flattening. [Vectorization
avoidance](https://dl.acm.org/doi/10.1145/2430532.2364512) is the idea that
sometimes it is more efficient to *not* flatten something fully. As a slightly
contrived demonstration, consider this program:

```Futhark
map (\x y -> x * 2 + y * 3 - 1)
    xs ys
```

This is not what the program actually looks like to Futhark, as the compiler IR
uses [administrative normal form](https://en.wikipedia.org/wiki/A-normal_form),
a 3AC-like design where arguments to functions (or builtin operations) must be
variables or constants. The input to flattening looks more like this:

```Futhark
map (\x y -> let a = x * 2
             let b = y * 3
             let c = a + b
             let d = c - 1
             in d)
    xs ys
```

We call such `let`-bindings "statements". If we aggressively fission this `map`
in order to flatten it, we are left with this:

```Futhark
let as = map (\x y -> x * 2) xs ys
let bs = map (\y -> y * 3) ys
let cs = map (\a b -> a + b) as bs
let ds = map (\c -> c - 1) cs
```

If we turn every `map` into a GPU kernel that reads and writes large arrays in
global memory, then we have increased overall memory traffic significantly.
Blelloch's original flattening algorithm did this - partially because it *is*
very principled, and partially because it is amenable to machines where parallel
operations can only do a single thing - in practice, NESL compiled to a portable
bytecode called VCODE, which had interpreters for various vector machines of the
time. (This reminds me that it would be fun to write a VCODE interpreter in
Futhark.)

However, the issue occurs even without excessive fission. Consider this program:

```Futhark
map (\b x -> if b then x + 2 else x - 2) bs xs
```

Do we *really* want to apply the `if` flattening rule to this, which involves
relatively expensive filtering and moving stuff around in memory? No, that would
[go insufficiently fast](../hedgehogs.html).

Inspired by DPH, our flattening transformation categorises each statement by
whether it is "scalar", meaning it exhibits no meaningful parallelism, *and* any
arrays it uses are *uniform*. Uniformity here (and in the next section) means
that the size of an array is invariant to the map nest in which it occurs. This
is important because it means that fissioning that statement does not result in
irregular intermediate arrays, and that if we want to preallocate memory for the
statement, we will know its size before executing its `map` nest.

It may be a bit surprising that the rule is not just that something is "scalar"
if it involves only primitive (non-array) types, but Futhark programs often
contain sequential loops that do sequential things via [in-place
updates](2022-06-13-uniqueness-types.html), and it makes no sense to flatten
these. We even have some slightly fuzzy heuristics, because it also makes no
sense to flatten something like a `replicate` (even though it is in principle
parallel) if it is only used to construct a scratch array that is then operated
on completely sequentially. This heuristic may require tweaking in the future -
we basically just hammered on it until it behaved reasonably for all of the
patterns and benchmarks we could think of.

Once statements have been classified, we then group the statements of a lambda
by whether they are scalar, and treat sequences of scalar statements as atomic
"super-statements" that are not individually fissioned off. This means we can
efficiently handle a `map` that contains both stretches of scalar code, as well
as important parallel constructs (like further nested `map`s) that really need
to be fissioned and flattened. All this comes from the nice property that we are
free to choose how much to decompose a function into constituent functions, as
long as we are willing to live with the consequences of our choice.

## Exploiting uniformity

So far we have mostly looked at how Futhark has replicated known approaches to
flattening, with the main tweak being aggressive fusion *prior* to flattening,
and the context of targeting GPU execution, which only cuNESL did previously.
But Futhark has one very important advantage that no previous language did:
[size types](2023-05-12-size-type-challenges.html), which are present in both
the source language and the IR, and which allow program transformations to
reason about the symbolic shapes of arrays.

In the case of flattening, shape information allows us to distinguish nesting
from *guaranteed uniform* to *possibly nonuniform*. As a reminder, uniform nested
parallelism is when the parallel dimensions are uniform in the map nest, meaning
invariant to the outer parallel dimensions. When this is the case, we do not
need to represent arrays using the segmented representation, but can use normal
multi-dimensional arrays. Apart from saving the cost of constructing and the
metadata arrays such as the flag vector, this also results in code that is much
easier for subsequent compiler passes to analyse. While we have little hope for
analysing the result of full flattening (which tends to just be a soup of
segmented scans), the Futhark compiler has passes that are able to perform
optimisations such as loop tiling on uniformly flattened code, but only if it
can figure out the looping structure.

The uniform case also allows for much simpler flattening rules. The general
flattening rule for `for`-loops is very nasty, in fact so nasty that I could not
bring myself to add it to this post, but the flattening rule for a uniform
`for`-loop (where the trip count is also uniform) is quite straightforward. The
program fragment

```Futhark
map (\xs -> loop xs' = xs for i < k do
              f xs')
    xss
```

flattens via loop interchange to

```Futhark
loop xss' = xss for i < k do
  map f xss'
```

but *only* if `f` is size-preserving and `xss` is not irregular.

The uniform flattening rule for loops is one of my favourites, because it shows
that flattening can to a large extent be seen as a transformation that "pushes
the `map`s" to the innermost level (and vectorization avoidance as a heuristic
that makes it not go all the way).

Flattening guided by size information is a *big* win, and we were fortunate that
the Futhark compiler already tracked all this information precisely. I remember
hearing about other implementations of flattening that got stuck on having to
come up with all this size analysis on their own, on an IR that was not designed
for it. As of this writing, all [Futhark benchmark
programs](https://github.com/diku-dk/futhark-benchmarks) exhibit only uniform
nested parallelism (because that's all that used to work!), so handling this
efficiently was critical, and I am very pleased that Amirreza (who implemented
this part) managed to get it to work as well as I had hoped for. It's quite the
experience to have a speculative plan go this well.

## All the other stuff from a decade of flattening programs

While our old flattener was limited, it was hardly unsophisticated. It was the
core of Futhark's compilation process, and Futhark really is already quite fast
in many cases. A new implementation of flattening would have to implement enough
of its tricks in order to reach the same performance on real programs.

In addition to many heuristics and optimisations, it supported a crucial feature
that we call [incremental flattening](2019-02-18-futhark-at-ppopp.html). The
idea is that instead of always trying to exploit *all* parallelism, which may
come with overhead, we turn the program into multiple versions, such that each
level of each `map` nest is present in both a sequentialised and flattened
version, and at run-time we pick the version that is parallel enough to saturate
the hardware, but no more. This is based on the idea that parallelism beyond
what the machine can exploit is simply overhead - for example, applying the
costly nonuniform `if` flattening rule is often not worth the gain of
parallelism. Further, sequential code is often subject to locality
optimisations, such as loop tiling. Which version is "best" depends on both the
program, the workload, and the hardware, and in practice must be determined
through an autotuning technique discussed in [this
paper](https://futhark-lang.org/publications/tfp21.pdf).

Amirreza managed to re-implement incremental flattening using the same technique
(and to a degree, the same code) as in our previous work, and it generates all
the same versions as before - plus a few more, as we now also treat nonuniform
nesting as parallel (this also causes trouble, more on that below).

Incremental flattening not only discriminates based on how much parallelism is
exploited, but also on *where* in the hardware hierarchy it is mapped to. This
functionality is core to most instances where Futhark produces *truly* efficient
code, similar to what a human expert would write. It is also part of the new
flattening pass, but if you are interested read [the pertinent parts of the old
blog
post](2019-02-18-futhark-at-ppopp.html#incremental-flattening-for-intra-group-parallelism)
as the mechanism is the same.

I have hopes for doing optimised flattening inside GPU thread blocks, where the
impact of flattening's less-than-ideal locality is less pronounced, but both the
code and the idea will have to wait for a future blog post.

### The weird performance regressions

The goal for the new flattening transformation was to be performance-neutral
relative to the old one. In the future we hope to do better, but for now the
goal was to do *more* without anything getting worse. Of course, given that we
were replacing a relatively unsystematic transformation with a more principled
one, I realised it was impossible for the change to have zero impact, but
overall the vast majority of our benchmarks see no change in performance.

Initially we did encounter regressions in many programs. The source of this
turned out to be nonuniform nested parallelism that the old flattener would
silently treat as sequential, and where treating it as sequential really is the
optimal strategy. As an example, consider the following function from the
benchmark program
[hand.fut](https://github.com/diku-dk/futhark-benchmarks/blob/master/adbench/hand.fut):

```Futhark
def angle_axis_to_rotation_matrix (angle_axis: [3]f64) : [3][3]f64 =
  let n = f64.sqrt (angle_axis[0] ** 2 + angle_axis[1] ** 2 + angle_axis[2] ** 2)
  in if n < 0.0001
     then identity 3
     else let x = angle_axis[0] / n
          let y = angle_axis[1] / n
          let z = angle_axis[2] / n
          let s = f64.sin n
          let c = f64.cos n
          in [ [ x * x + (1 - x * x) * c
               , x * y * (1 - c) - z * s
               , x * z * (1 - c) + y * s
               ]
             , [ x * y * (1 - c) + z * s
               , y * y + (1 - y * y) * c
               , y * z * (1 - c) - x * s
               ]
             , [ x * z * (1 - c) - y * s
               , z * y * (1 - c) + x * s
               , z * z + (1 - z * z) * c
               ]
             ]
```

This function is used inside of a `map`, and since the conditional `n < 0.0001`
can vary between different iterations of the `map`, this is a nonuniform `if`
due to control flow, although the size of the array it produces is a uniform
`[3][3]`. The old flattener could not handle nonuniform `if` in any special way
and just turned this entire thing into sequential code, while the new flattener
will apply fission and the `if` flattening rule in order to exploit the
parallelism of the `identity 3` function call - which constructs a *3x3*
identity matrix. The amount of parallelism here is completely irrelevant
compared to the bookkeeping cost of flattening `if`, so this caused a
substantial performance regression. Incremental flattening did not save us here,
because the function is used in a context where *sibling* code contains
important parallelism, and incremental flattening is all-or-nothing at each
level of the `map` nest.

The solution is to add a `#[sequential]`
[attribute](2020-06-28-attributes-in-futhark.html) to the `if` that indicates to
the compiler that it should just be sequentialised. We already use this
attribute in other programs to guide incremental flattening along the right
paths, or just to cut down on compile time by not generating unnecessary
versions. Hence, while using an attribute like this is hardly elegant, it means
the new flattener does not need an entirely new workaround when it does the
wrong thing - the old workarounds can still be used.

Sadly, the attribute solution only works for code the user can see. Futhark also
contains a sophisticated implementation of [automatic
differentiation](https://autodiff.org/) (real post yet unwritten, but see [the
documentation](https://futhark-lang.org/docs/prelude/doc/prelude/ad.html) and
[this post](2026-02-23-accumulators.html)). When applying automatic
differentiation, the compiler generates substantial and quite complicated code,
including various parallel operations, but since it is code that is not written
by a human, there is no way for a human to insert attributes. This means we do
have one program (`hand.fut` again, actually) that in its Jacobian computation
has up to *2x* slowdown on some (not all) workloads, because the adjoint code is
overly parallelised. On the other hand, the related benchmark
[gmm.fut](https://github.com/diku-dk/futhark-benchmarks/blob/master/adbench/gmm.fut)
sees a *4x speedup* on some Jacobian computations, due to the additional
parallelisation of its adjoint code! I played a little bit with our heuristics
to see if I could come up with a simple fix, but ultimately decided to leave it
until we have a better idea of what the *right solution* is. This means that
there are some programs that will change in performance once the new flattener
is merged.

## How it actually looks

I figured some might be curious about what the IR will actually look like
post-flattening, so I will provide some examples of flattening here. These are
real IR examples, so they are necessarily quite verbose. I have however removed
safety checks and [source provenance](2025-07-29-tracking-source-locations.html)
to make it slightly terser. Feel free to skip this section if you dislike
reading long reams of tedious machine-generated code (there'll be enough of that
in the future, it seems).

Consider this (source) function definition:

```Futhark
def f [m] (ns: [m]i64) =
  map (\n -> i64.sum (iota n)) ns
```

This function contains nonuniform nested parallelism, as the size `n` is
nonuniform in the `map` nest. After flattening it will look like this:

```
fun
  f_7412 (m_7572 : i64,
          ns_7573 : [m_7572]i64)
  : {*[m_7572]i64} = {
  let {m_7777 : i64,
       segiota_flags_7778 : [m_7777]bool,
       segiota_offsets_7779 : [m_7572]i64,
       segiota_elems_7780 : [m_7777]i64} =
    apply builtin/segiota(m_7572, ns_7573)
    : {i64, *[?0]bool, *[m_7572]i64, *[?0]i64}
  let {redomap_7790 : [m_7777]i64} =
    segscan(thread; ; )
    (gtid_7791 < m_7777) (~phys_tid_7792)
    : {bool, i64} {
      let {flag_7793 : bool} =
        segiota_flags_7778[gtid_7791]
      let {x_7794 : i64} =
        segiota_elems_7780[gtid_7791]
      return {returns flag_7793,
              returns x_7794}
    }
    ({false, 0i64},
    ,
    \ {x_flag_7795 : bool,
       eta_p_7796 : i64,
       y_flag_7797 : bool,
       eta_p_7798 : i64}
      : {bool,
         i64} ->
      let {x_7799 : bool} =
        logor(x_flag_7795, y_flag_7797)
      let {x_7800 : i64} =
        if y_flag_7797
        then {eta_p_7798} else {
          let {+_res_7801 : i64} =
            add64(eta_p_7796, eta_p_7798)
          in {+_res_7801}
        }
        : {i64}
      in {x_7799, x_7800})
    \ {seg_flag_7802 : bool,
       x_7803 : i64}
      : {i64} ->
      {x_7803}
  let {segred_7808 : [m_7572]i64} =
    segmap(thread; virtualise; )
    (gtid_7809 < m_7572) (~phys_tid_7810) : {i64} {
      let {n_7811 : i64} =
        ns_7573[gtid_7809]
      let {cond_7813 : bool} =
        eq_i64(n_7811, 0i64)
      let {segment_res_7814 : i64} =
        if cond_7813
        then {0i64} else {
          let {offset_7812 : i64} =
            segiota_offsets_7779[gtid_7809]
          let {binop_x_7816 : i64} =
            add_nw64(n_7811, offset_7812)
          let {i_7817 : i64} =
            sub_nw64(binop_x_7816, 1i64)
          let {x_7818 : i64} =
            redomap_7790[i_7817]
          in {x_7818}
        }
        : {i64}
      return {returns segment_res_7814}
    }
  in {segred_7808}
}
```

Instead of using `map` and `scan`, the IR has dedicated constructs such as
`segscan` and `segmap`, that can represent multidimensional *uniform* segmented
operations. Since this program is nonuniform, all of these are one-dimensional
(`(gtid_7809 < m_7572)`), and encode the structure using flag arrays. The
program first does a segmented iota through a call to a builtin function
`builtin/segiota`, then performs a segmented scan, and finally extracts the last
element of each segment. Note that `builtin/segiota` returns not just the data
and flag vectors of the segmented representation, but also an offset vector that
denotes the start offset of each segment. This is used in the final `segmap`.
Generally the builtin nonuniform segmented operations return more than the
minimum auxiliary information, because the additional arrays often turn out to
be necessary, and usually have to be computed internally anyway.

## The future

The new flattener is not merged yet, but it will be soon. The only work that
remains to be done is general simplification and addressing various TODO
comments - the performance fluctuations discussed above will be left for the
future.

In NESL (and DPH), flattening was used to support irregular arrays as a
programming construct in the source language. In Futhark, our goals are more
modest: we want to support nonuniform nested parallelism, but we have no plans
for adding irregular arrays to the source language or to the IR. Flattening
handles irregular intermediate arrays using a bespoke representation that is
internal to the flattening pass itself. I am not opposed to irregular arrays in
principle, if they can be handled efficiently, but many important Futhark
features such as size types and our
[FFI](2022-07-01-how-futhark-talks-to-its-friends.html) might get a lot more
complicated if we added them. I prefer solving small problems well than big
problems poorly, and it has already taken *years* to solve the problems that
arise from Futhark's fairly small design - and we're not done yet. I'm not very
eager to go looking for even larger problems.

That said, I do think we can expose flattening as a programming construct in
*some way*, which will allow for new ways of writing data parallel programs, in
particular if we also allow recursion. The main reason for not allowing
recursion so far was that we did not have a compilation strategy for recursive
functions on GPUs, but the flattening transformation will now move the recursion
to the CPU, so this argument does not hold anymore. That, however, is a topic
for a future post - this one got long enough.
