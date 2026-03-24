---
title: Scan-scatter fusion
author: Troels Henriksen
description: Fusing an unusual pattern that occurs frequently in some kinds of programs.
---

This post is about the compiler optimisation with the coolest name: Fusion. In
the functional literature it is sometimes called deforestation, which sounds
rather less cool, and in fact somewhat undesirable. But I digress already. The
post to follow is at a high level about a new optimisation we wanted to add to
the Futhark compiler to handle a reasonably common (and quite important) class
of parallel programs, how we changed our approach to make it tenable, how we
also had to adjust our Futhark programming style to cater to this optimisation,
and why that is not quite as horrible as it sounds. I will note in advance that
the vast majority of the work was done by [William Henrich
Due](https://williamdue.github.io/), who is a PhD student here at
[DIKU](https://diku.dk), and one critical idea was suggested by [Cosmin
Oancea](https://hjemmesider.diku.dk/~zgh600/).

## Background on fusion

Fusion is an optimisation that combines together data parallel operations in
order to avoid storing intermediate results in memory. The simplest example is
`map`-`map` fusion, which occurs when the output of a `map` is passed to another
`map`. For example, the expression

```Futhark
map (\y -> y + 2) (map (\x -> x * 2) xs)
```

could be rewritten through fusion to

```Futhark
map (\x -> x * 2 + 2) xs
```

which, if we imagine the straightforward execution on a computer, performs fewer
reads and writes to memory. Except for edge cases fusion does not affect the
asymptotics of a program, but it is an important source of constant-time
improvements. What is particularly important is that it allows a program to be
structured as many small modular operations, which are easier to maintain than a
single big one, without suffering any overhead. Indeed, I think the main
advantage of optimising compilers is not to magically make code faster, but
rather to allow us to write more modular and abstract code without suffering
performance penalties.

The example above is a case of *vertical fusion*, where two operations are
connected in a producer-consumer relationship. Another case of fusion is
*horizontal*, where two parallel operations on the same input can be combined
into a single parallel operation. For example,

```Futhark
(map (\x -> x + 2) xs,
 map (\x -> x * 2) xs)
```

could be turned into

```Futhark
unzip (map (\x -> (x+2, x*2)) xs)
```

which means we have to read `xs` only once instead of twice. This example is
cluttered by the need for `unzip` - in the actual Futhark compiler, we use a
slightly different representation where operations such as `map` can take an
arbitrary number of inputs, and where the result is implicitly `unzip`ed, to
simplify internal bookkeeping.

There is also a combination of the two called *diagonal fusion*, but it requires
explaining fusion in terms of dataflow graphs, and is not necessary to
understand this post.

Fusion is probably *the* most important single optimisation that makes data
parallel languages fly. Apart from `map`-`map` fusion, a real fusion engine will
have a collection of rewrite rules for all other primitive constructs in the
language: `map`-`reduce`, `map`-`scan`, etc., with complexities to allow for
`zip`/`unzip`, intermediate operations such as transpositions and reshapes, and
so on. These add complexity without adding much to the story, so I will elide
them from this post - the overall goal remains unchanged, which is to reduce the
number of distinct data parallel operations.

The set of available fusion rules is often somewhat inaccurately called a
"fusion algebra". Ideally, we want to have a fusion rule for every point in the
cartesian product of the data parallel operators. However, it is not always
possible to write such a rule, since the result of a fusion rule must be
expressible within the language. We can of course add more primitive operations
to handle ever more baroque instances, but since that also grows the number of
combinations to handle, it's not really a practical approach. Further, some
combinations are more fundamentally impossible to fuse. Futhark is like most
languages in that we implement the cases that are easy, as well as the difficult
cases that actually occur in the kinds of programs people write.

## Scans, scatters, and filters

One of the most important data parallel operations is
[`scan`](../examples/scan-reduce.html). It computes the reductions of all
prefixes of an array:

```Futhark
    scan (+) 0 [1, 2, 3, 4]
 == [reduce (+) 0 [1],
     reduce (+) 0 [1,2],
     reduce (+) 0 [1,2,3],
     reduce (+) 0 [1,2,3,4]]
 == [1, 3, 6, 10]
```

When `scan` is used with the `+` operator it is called a *prefix sum*. One
interesting use of scans is for filtering an array such that we keep only those
elements that satisfy a predicate. Suppose we wish to remove the negative
elements from this array:

```Futhark
let as = [-1, 2, -3, 4, 5, -6]
```

For each element, see if we want to keep it:

```Futhark
let keep = map (\a -> if a >= 0 then 1 else 0) as
-- [ 0, 1, 0, 1, 1, 0]
```

Then compute the prefix sum of the `keep` array:

```Futhark
let offsets1 = scan (+) 0 keep
-- [ 0, 1, 1, 2, 3, 3]
```

This is an interesting array. First, the last element indicates the size of the
final filtered list. Also, for the elements we wish to keep (i.e., the ones
where the corresponding element in `keep` is 1), `offsets1` contains the
1-indexed positions in the final result of all the elements we wish to keep. we
can turn it into 0-indexed positions simply by subtraction:

```Futhark
let offsets = map (\x -> x - 1) offsets1
-- [-1, 0, 0, 1, 2, 2]
```

Now we can see that element `as[1]` should be in position `offsets[1]` (0) in
the result, `as[3]` should be in `offsets[3]` (1), and so on. But how do we
actually *construct* that result? To do that, we need another primitive:
`scatter`. The expression `scatter xs is vs` computes equivalent of the
imperative pseudocode:

```
for j < n:
  i = is[j]
  if i is a valid index for xs:
    xs[i] = vs[j]
```

That is, `scatter xs is vs` is a kind of parallel [in-place
update](2022-06-13-uniqueness-types.html): we update the array `xs` at the
positions stored in `is` with the values in `vs` and return the updated array.
As a convenient special case, out-of-bounds writes are ignored. This is just
what we need for filtering! Based on the arrays computed above, we can produce
the final result as follows:

```Futhark
scatter (replicate (last offsets1) 0)
        (map2 (\i k -> if k == 1 then i else -1)
              offsets keep)
        as
-- [2, 4, 5]
```

We first produce an empty "scratch" array of zeroes to scatter into. Then we map
the indexes for the elements we wish to discard to `-1`. We can turn it into a
generic function that is polymorphic in the predicate and element type, and
which also handles empty arrays:

```Futhark
def filter [n] 'a (p: a -> bool) (as: [n]a) : []a =
  let keep = map (\a -> if p a then 1 else 0) as
  let offsets1 = scan (+) 0 keep
  let empty_output = n == 0 || last offsets1 == 0
  in if empty_output
     then []
     else let scratch = replicate (last offsets1) as[0]
          in scatter scratch
                     (map2 (\i k ->
                              if k == 1
                              then i - 1
                              else -1)
                           offsets1
                           keep)
                     as
```

The central technique is using `scan` to compute some indexes that are then
passed to a `scatter`. This is a pattern that occurs in many similar functions,
such as `partition` and `filter`. And more generally, it occurs when
implementing complicated parallel algorithms, either directly or as a result of
[flattening](https://en.wikipedia.org/wiki/Flattening_transformation). Therefore
we want to handle it efficiently, in particular, we want to avoid intermediate
results through fusion.

## Fusion in filter

The definition of `filter` above contains some opportunities for fusion. The
Futhark source language cannot directly express them, but it is not difficult to
imagine how it works. First, the `map2` that is passed to `scatter` can be fused
with the `scatter.` We then consider the fusion of the `map` that produces
`keep`. Unfortunately, with the way the function is written, fusion is a bit
tricky, as `keep` is used both as the input to `scatter` (after the first step
of fusion, anyway) but also as input to the `scan`, and the result of the `scan`
is used to compute `num_to_keep` which is a [control
dependency](https://en.wikipedia.org/wiki/Control_dependency) on the `scatter`.
Essentially, we need the result of the `scan` before we even know whether to run
the `scatter` at all, which prevents the two from ever being fused. We also use
`last offsets1` to compute the target array.

Accessing intermediate results in the way we do in `filter` is called a fusion
"hindrance", and is something to be avoided. We can write the function in a
somewhat different way to get better fusion properties:

```Futhark
def filter [n] 'a (p: a -> bool) (as: [n]a) : []a =
  let keep = map (\a -> if p a then 1 else 0) as
  let offsets1 = scan (+) 0 keep
  let scratch = copy as
  let res =
    scatter scratch
            (map2 (\i k ->
                     if k == 1
                     then i - 1
                     else -1)
                  offsets1
                  keep)
            as
  let filter_size = if n == 0 then 0 else last offsets1
  in take filter_size res
```

Now we over-allocate the scratch array for the result by copying the input,
meaning we don't need to look at `offsets1` before the very end, in order to
figure out how many elements of the result (`filter_size`) we actually want. One
problem is that we have to make all of `offsets1` available just so that `last`
can read the final element. We will return to this issue, but let us ignore it
for now. To focus on one subproblem at a time, we define `semifilter`, which
elides the computation of `filter_size` and merely returns the full `res` array:

```Futhark
def semifilter [n] 'a (p: a -> bool) (as: [n]a) : []a =
  let keep = map (\a -> if p a then 1 else 0) as
  let offsets1 = scan (+) 0 keep
  let scratch = copy as
  let res =
    scatter scratch
            (map2 (\i k ->
                     if k == 1
                     then i - 1
                     else -1)
                  offsets1
                  keep)
            as
  in res
```

We will return to `filter` proper later, but this reduced form makes it easier
to discuss an important issue, namely the fact that that we have a `scan` going
(ultimately) into a `scatter` - and this is not traditionally a
producer-consumer relationship that is fusible in data parallel languages.
Futhark's `scan` implementation is pretty fast - on GPU it uses a [decoupled
lookback scan on
GPU](https://research.nvidia.com/sites/default/files/pubs/2016-03_Single-pass-Parallel-Prefix/nvr-2016-002.pdf)
and a [similar approach on
CPU](https://dl.acm.org/doi/10.1145/3649169.3649248) - but storing all of
`offsets1` in memory, just to read it again in the `scatter`, seems very
wasteful. Given how commonly this pattern occurs in certain parallel algorithms,
we thought it was worth trying to figure out whether we could extend Futhark's
fusion algebra to fuse `scan` and `scatter`, and so we began an effort [a bit
over a year ago](https://github.com/diku-dk/futhark/pull/2217). We got
distracted along the way, for example by [implementing a library of data
parallel containers including hash
tables](https://github.com/diku-dk/containers), but it still turned out to be a
lot more challenging than we had anticipated.

## Fusing scan and scatter

To boil it down to the essentials, the problem is to fuse compositions like
this:

```Futhark
scatter dest (scan (+) 0 xs) vs
```

Actually, it doesn't matter which of `scatter`s two input arrays the `scan`
belongs to - in Futhark's actual IR, they are treated the same.

Further, we might also want to handle the case where the result of a `scan` is
passed to a `map`:

```Futhark
map f (scan (+) 0 xs)
```

Our first step was to think about the code that is ultimately executed for a
`scan`. At some point, late in the execution, we have an output element in our
hand, and write it to some position in the result array. It seems fairly
straightforward to either compute some other index to put it (for the `scatter`
case), or apply some function to it (for the `map` case), without having to
modify the somewhat intricate synchronisation required for the parallel part of
the `scan` implementation. And indeed, the code generation is easy enough - the
challenge is how to modify the IR to express the fact that we want to do
something to the result of a `scan` before writing the result array.

The `scan-map` case was simple enough - we just tacked a final lambda on to our
internal representation of scans, which starts out as identity. While the
internal representation cannot be exactly be represented in the source language,
we can define a function `maposcanomap` that resembles the the internal
construct well enough to explain the idea:

```Futhark
def maposcanomap [n] 'a 'b 'c 'd
                 (as: [n]a)
                 (f: a -> (b, c))
                 (op: b -> b -> b)
                 (ne: b)
                 (g: b -> c -> d) : [n]d =
  let (bs, cs) = unzip (map f as)
  let bs' = scan op ne bs
  let ds = map2 g bs' cs
  in ds
```

There is:

* An input array `as`.
* A function `f` that produces two values, one of which will contribute to the
  scan, and the other of which will be passed to the function `g`.
* A scan operator `op` with neutral element `ne`.
* A function `g` that is passed elements of the `scan` result along with the
  corresponding non-scan value produced by `f`.

During code generation, we pretty much do a normal scan, store the non-scanned
result from `f`, do the scan as normal, and then apply the `g` function at the
end before writing the results. Extending the fusion algebra was easy, as it
just involves appending things to the `g` function - very similar to `map-map`
fusion.

The trouble arose when trying to extend this to also handle the `scatter` case.
In the Futhark source language, `scatter` is a primitive ([in some sense it has
to be](2019-04-10-what-is-the-minimal-basis-for-futhark.html)), and in the IR it
was sort of tacked onto our representation for `map` in a way that was difficult
to extend to `scan`. For prototyping, we simply invented an ad-hoc IR construct
for "scan fused with `scatter`", but I was very concerned about bolting it onto
our general representation for fused operations.

Fortunately, Cosmin Oancea made a crucial observation: why do we even have
`scatter` in the IR at all? Why not simply express it in terms of
[accumulators](2026-02-23-accumulators.html)? While accumulators are most often
used in conjunction with a binary combining operator, we also support not having
an operator at all, in which case an "update" just overwrites whatever value is
present at the index being updated. While we cannot express the idea exactly in
the source language, the idea is that the source expression

```Futhark
scatter xs is vs
```

is turned into essentially the following core IR:

```Futhark
with_acc xs (\xs_c xs_acc ->
  map2 (\i v xs_acc' ->
          update_acc xs_acc' i v)
       is vs)
```

The key advantage is that there is no actual `scatter` here - the parallelism is
expressed as a `map`, and any fusion rules also merely have to take the `map`
into account. Essentially, the `scan-scatter` case becomes subsumed into the
`scan-map` case. We may also have to move code into the `with_acc` lambda to
expose fusibility, but this is comparatively simple, and more importantly,
Cosmin had already implemented this as part of the work we did to support
automatic differentiation.

This solution ticks all of the boxes for me. It addresses a thorny problem and
it does it by *removing* concepts. We already had accumulators, they serve a
purpose and are necessary, so by using them in more cases we reduce the overall
complexity of the compiler. Now, accumulators *are* more flexible than scatters,
so I wondered if there were cases we could optimise before that we cannot
optimise anymore, but this turned out not to be the case - or at least I have
not noticed them yet.

It is very rare that I encounter such cases, and I am still unreasonably chuffed
about this solution. Many thanks to Cosmin for this insight.

## Returning to `filter`

Our spirits buoyed by this solution, let us return to the `filter` function:

```Futhark
def filter [n] 'a (p: a -> bool) (as: [n]a) : []a =
  let keep = map (\a -> if p a then 1 else 0) as
  let offsets1 = scan (+) 0 keep
  let scratch = copy as
  let res =
    scatter scratch
            (map2 (\i k ->
                     if k == 1
                     then i - 1
                     else -1)
                  offsets1
                  keep)
            as
  let filter_size = if n == 0 then 0 else last offsets1
  in take filter_size res
```

While we find that this *does* now manage to fuse all data parallel operations
into a single operation, which can then be compiled to a single GPU kernel, it
ends up writing the entire `offsets1` array to memory, merely so that we can use
it in the definition of `filter_size`. This is inefficient because `offsets1` is
a pretty large array, and we only use a single element of it. I considered
adding an ad-hoc simplification rule for the case where the result of a scan is
indexed, but halfway through the (nasty) implementation, I realised that perhaps
I could just rewrite the definition of `filter` instead. The trick is to add
another `scatter` that produces a single element array containing the equivalent
of `last offsets1`, which we then retrieve. Specifically, I now compute
`filter_size` like so:

```Futhark
  let filter_size =
    (scatter [0]
             (tabulate n (\j -> if j == n - 1 then 0 else -1))
             offsets1)[0]
```

The trick is that we write to index `-1` for all elements except the one that
corresponds to `last offsets1`, which is written to index 0. We then retrieve
this element by indexing. The `scatter` fuses horizontally and vertically with
the others that share the same input array (`offsets1`), ultimately producing
two arrays: the size-`n` `res` array, and the size-1 array containing
`filter_size`.

I was a bit unsure at first of whether this approach was merely a hack that
exploited internal compiler details. Most people I showed it to initially
thought it was ugly. But I eventually convinced myself that they are all wrong.
While this *is* indisputably very subtle and tricky code, it does not rely on
arbitrary implementation choices, but falls out quite naturally from the fusion
algebra. The main thing to keep in mind when writing fusible code is to avoid
using intermediate results in a scalar way (such as by indexing), and to ensure
that all input arrays have the same size. This code merely follows these rules.

The downside of working with an obscure programming paradigm ([higher order data
parallel programming](2020-05-03-higher-order-parallel-programming.html)) is
that there are not many yardsticks for what good code looks like. The upside is
that any programmer in the community constitutes a significant fraction of the
community, and so their (e.g., *my*) opinion carries weight.

## Performance

The whole point of this exercise was performance, so let us talk performance.
First a disclaimer: at the time this blog post is written, the implementation is
not finished, and performance has unsurprisingly turned out to depend heavily on
how certain parameters related to efficient sequentialisation are configured.
Ultimately these must be tuned (preferably
[automatically](https://github.com/diku-dk/futhark/issues/2402)) for the
hardware and program. I have not done so here because I am ~~lazy~~ concerned
with out-of-the-box performance, and we are still tweaking the default
heuristics.

On an AMD RX 7900 XT GPU, using the [HIP
backend](2024-07-17-opencl-cuda-hip.html), the compiler augmented with
`scan-scatter` fusion takes *3.7ms* to filter a hundred million 32-bit integers,
removing roughly half of them. This is a speedup of *2.08x over the baseline
compiler. This is pretty much to be expected as fusion roughly halves the amount
of memory traffic. To put the number into perspective, a prefix sum on the same
input takes about half the time, and a plain copy of the input can be done in a
third of the time.

By playing around with the sequentialisation factor (I guess I wasn't that lazy
anyway), we can drop the time taken to perform the filter to *2.2ms*, compared
to *1.3ms* for copying it - an overhead of *1.7x*.

For larger programs, the story remains a bit more muddled. In general things get
faster, since *fusion does that*, but the extra register pressure has somewhat
unpredictable performance effects on code that was already pushing the limit.
Further, the ability to fuse `scan` as a producer causes new and interesting
code paths to be taken in other parts of the compiler, so things that by happy
accident compiled in some fortunate way before may not do so anymore, and each
of these cases must be investigated. In particular, [incremental
flattening](2019-02-18-futhark-at-ppopp.html) is always a bountiful source of
dubious heuristics that [needed at least one workaround in a
benchmark](https://github.com/diku-dk/futhark-benchmarks/commit/f85c110601055b7c0a8fd143d9c490cb8fc6e5c8).

Also, I should note that we have still not reached *quite* the performance of
some CUDA implementations filtering we wrote by hand. We are still investigating
the cause, but it seems related to low level issues related to register usage
and efficiently passing values from the first function to the second function
(the `c` type in `maposcanomap`). Hopefully we can address this, but I am OK
with postponing it until later, as the issue is unrelated to IR design and
fusion rules.

While I am mostly concerned with parallel performance, I will note that the
fused `filter` is compiled to a single loop when using the sequential `c`
backend, quite similar to what you might write by hand.

## Conclusions

Fusing `scan` and `scatter` is a useful thing to do for a data parallel language
that wants to efficiently run programs that perform partitioning or filtering of
arrays. For Futhark we found an approach that was not particularly invasive, as
it can be subsumed into `map` fusion with accumulators. We can only fuse
"chains" of operations that contain a single `scan` - this is not a fundamental
algorithmic restriction, and to my knowledge the
[Accelerate](http://acceleratehs.org/) folks are working on a representation
that can handle any sequence of `scan`s and `scatter`s (with some restrictions).

The question is which problems exist that contain such patterns, and whether
they are worth the implementation complexity. I have actually become quite
interested in performing a study of how data parallel operations are composed in
real programs, in order to motivate new directions for fusion. It would be
*particularly* interesting to do this in a polyglot setting, to study the
differences between how programmers express algorithms in different data
parallel languages.
