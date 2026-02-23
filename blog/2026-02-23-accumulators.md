---
title: The why and how of parallel in-place random-access accumulation
author: Troels Henriksen
description: To support automatic differentiation we had to come up with a new language construct, but it is too powerful for normal people (and their type systems).
---

The Futhark intermediate representation (IR), used by the compiler, is very
similar to the source language used by programmers. While the IR lacks syntactic
sugar and facilities like polymorphism and higher order functions, the language
constructs are fundamentally the same - with one exception, which is the topic
of this post. It turned out that when implementing [automatic
differentiation](http://autodiff.org/) a couple of years ago, we needed to
efficiently express certain patterns of computation that were simply beyond our
available programming vocabulary. When we finally came up with an extension that
had the properties we needed, it is still not clear how we could expose it in a
safe manner in the source language, and so it remains an IR plaything. In this
post I will describe the need for this extension, how we designed it for the IR,
and why it is still not available for use in the source language. [Bottom line
up front](https://en.wikipedia.org/wiki/BLUF_(communication)): the idea is to
encode certain effects using linear values.

## Basics of automatic differentiation

*If you do not care about the background of the problem, then feel free to skip
this section.*

Automatic differentiation (AD) is a program transformation for computing
mathematical [derivatives](https://en.wikipedia.org/wiki/Derivative) of computer
programs that implement mathematical functions. As a Haskell programmer, AD
reminds me of monads in that it is notoriously difficult to explain, and once
you understand it, you lose the ability to explain it to others. Every paper on
AD seems to start with an incomprehensible introduction - feel free to peruse
the one in [the paper we wrote on AD in
Futhark](https://futhark-lang.org/publications/sc22-ad.pdf). I really need to
write a proper post about AD from a Futhark perspective at some point, but it is
not necessary to have an understanding of AD to understand this post. If you are
truly curious about how AD works, then [this
paper](https://jmlr.org/papers/volume18/17-468/17-468.pdf) is in my opinion the
best introduction.

AD can be used for many things ([I use it for surface normals
here](https://futhark-lang.org/examples/literate-video.html)), but one common
use is in function optimisation. A *cost function* is a function that takes as
input some parameters and returns a single number, denoting the "error" of the
function relative to some goal. The problem is then tweaking the parameters to
make the return value as small as possible. This is basically how machine
learning works. If we can find the
[gradient](https://en.wikipedia.org/wiki/Gradient) of the function, then we can
nudge the parameters along the direction of the gradient, and gradually decrease
the error.

AD lets us easily compute the gradient of functions that are too awkward to
differentiate by hand. The basic idea is that at the bottommost level, the
execution of a function for *some input* consists of a dataflow graph where each
node represents some primitive operation on numbers, such as an addition,
multiplication, square root, etc. In this view, control flow and memory
operations have been "unrolled" - we focus on a *concrete execution* for fixed
input; essentially a recording of the low-level arithmetic operations. To take
the derivative of such a graph, we can apply the [chain
rule](https://en.wikipedia.org/wiki/Chain_rule) and the [partial
derivatives](https://en.wikipedia.org/wiki/Partial_derivative) of all primitive
operations and simply propagate information through the graph. Some
implementations of AD, such as
[CoDiPack](https://scicomp.rptu.de/software/codi/) actually work by recording
such a graph during runtime and differentiating it afterwards, while others
(such as the one in Futhark) implement AD as a program transformation.

There are two main ways to perform automatic differentiation. The simplest is
called *forward mode*, and works by pairing up each normal input with a
*tangent* - essentially, we turn all numbers into pairs of numbers. Each
primitive operation of numbers then becomes an operation on pairs of numbers.
Using the [chain rule](https://en.wikipedia.org/wiki/Chain_rule) and the
[partial derivatives](https://en.wikipedia.org/wiki/Partial_derivative) of all
primitive operations in our language, we can then compute a partial derivative
for each input to the function. The forward mode of AD is very simple to
implement, and can in many languages [simply be done as a
library](https://oizin.github.io/posts/autodiff-forward/index.html). However,
the downside of the forward mode of AD is that if we have *n* inputs, then we
also need to do *n* passes over the program - this is not efficient for cost
functions, which often have an enormous number of inputs but only a single
output.

While the forward mode of AD works by attaching extra information ("tangents")
to the inputs and then propagating it down through the dataflow graph, the
*reverse mode* attaches extra information (called "sensitivity", "cotangents",
or "adjoint") to the *results*, and then passing it *backwards* through the
dataflow graph, again by using the chain rule. If we actually have a dataflow
graph representing the function execution, then this is not so difficult to do,
but if we want to implement it as a program transformation, then it becomes
quite mind-bending: we essentially have to construct a differentiated program
that runs the original ("primal") program *in reverse*, where *all reads of a
variable become a write*.

Describing this process in full detail is far beyond the post, but the idea is
that each scalar variable `x` in the original program becomes associated with
cotangent variable `x_ct`, and whenever the original program had an operation
such as

```
z = x * y
```

then the differentiated program will contain statements

```
x_ct += y * z_ct
y_ct += x * z_ct
```

where we *propagate* the accumulated cotangent of the `z` variable into the
cotangents of `x` and `y`. The important point is this: Every *read* of `x` in
the original program becomes an `+=` *write* in the derivative.

The notation used above is imperative, and `+=` does not make much sense in a
purely functional language such as Futhark. However, it is not difficult to
imitate `+=` by simply re-binding a variable to a new name - that is how we
always simulate state. We have to be careful when this happens inside of control
flow such as `if` or `loop` but it mostly becomes a book-keeping challenge about
keeping track of which variable represents the most updated cotangent, and not
something that is truly difficult.

But consider now this Futhark expression fragment:

```Futhark
let ys = map (\i -> xs[i]) is
```

Here we are using an array of indexes, `is`, to access another array, `xs`. This
means that the data dependencies are dynamic - they depend on the concrete
values of `is`. As with scalars, the cotangents of an array `xs` is represented
by an array `xs_ct`, and similarly for `ys`/`ys_ct`. We need to update the
elements of `xs_ct` based on the values of `ys_ct`, but also while taking into
account which elements of `xs` contributed to each value of `ys`. Supposing `is`
is of length `n`, then in in imperative notation it is not so difficult to
express this:

```
for j < n:
  xs_ct[is[j]] += ys_ct[j]
```

But in a parallel and purely functional setting, this seemingly simple program
is not easy to express at all. At the machine level, the `+=` accumulation
itself needs some care if we want to run the loop in parallel, but it can be
implemented using atomic updates or some other synchronisation mechanism. The
real problem is that this loop, where we do a *write to some arbitrary element
of an array* is not easily expressible using our existing data-parallel
vocabulary. For this simple case, where each iteration accesses only a single
element, we can actually express the differentiated program using [generalised
histograms](https://futhark-lang.org/blog/2018-09-21-futhark-0.7.1-released.html#histogram-computations),
but this is not possible when the `map`ped function contains a loop and indexes
`xs ` a dynamic number of times. For simplicity of exposition, I will stick to
the simpler case, even though it is actually a special case that does not need
any fancy techniques.

The root problem is that we are updating an array that originates from outside
the `map`ped function - it is a free variable. One solution, which you will find
in some implementations of AD, is to simply eliminate the free variables and
rewrite the original program to look like this:

```Futhark
let xs_copies = replicate n xs
let ys = map2 (\xs_copy i -> xs_copy[i]) xs_copies is
```

However, the problem is that the modified program is asymptotically less
efficient than the original - we are replicating the `xs` array `n` times (where
`n` i the size of `is`). We can *perhaps* avoid actually manifesting the
replicated array by using a clever representation, but differentiating the
program above will produce something like this:

```Futhark
let xs_copies_ct = map2 (\i y_t -> replicate m 0 with [i] = y_t)
                        is
                        ys_ct
let xs_ct = map (reduce (+) 0) (transpose xs_copies_ct)
```

This *works*. However, it works by producing a two dimensional array
`xs_copies_ct` with rows that each contain a single nonzero value, which we add
together to produce the final cotangent array `xs_ct`. This is again
asymptotically inefficient because we spend a lot of time adding together
zeroes. This can be avoided if we represent the `xs_copies_ct` array using some
sparse representation and a more clever way of summation - but the Futhark IR
*does not have* a datatype for sparse arrays, and it becomes tricky to detect
and exploit the sparsity once we do not statically know how often the `xs` array
is indexed inside the `map`.

## Language approaches

To restate the problem, our goal is that we want to be able to express
computation equivalent to this imperative pseudocode:

```
for x in xs:
  # ... arbitrary control flow, maybe nested loop ...
    i = ...    # something complicated
    v = ...    # something complicated
    ys[i] += v # where ys is bound outside the loop
```

Where we also want the outermost loop to be expressed via data-parallel language
constructs, as is always the case in Futhark's IR, and naturally not sacrifice
our purely functional semantics.

The issue is basically that we want to perform a side effect.
[Dex](https://github.com/google-research/dex-lang), a functional language with
many AD-related innovations, had the same problem and addressed it in quite a
natural way: by adding an effect system. Using an effect system, a program *can*
perform side effects (such as writing to the element of an array), but the type
system makes precise when it happens. Further, the effects are controlled: while
the Dex accumulation effect allows *writes* into an array, that array cannot be
*read from* while being accumulated into. Assuming the accumulation effect is
associative and commutative (which is the case for addition), then the updates
can be done in parallel without any risk of race conditions.

I have a hunch that side effects, possibly in the form of algebraic effect
handlers, may be the next big thing in functional languages. Newer languages
such as [Flix](http://flix.dev/) and [Koka](https://koka-lang.github.io) are
investigating how to implement them efficiently and make the ergonomics
tolerable for everyday programming. Why, then, did we not take this approach in
Futhark?

The reason is while some people claim that programs are mainly for computers to
run, others that [they are for people to
read](http://raganwald.com/2016/03/17/programs-must-be-written-for-people-to-read.html),
but I think that programs are for computers to *re-write*. Most of the Futhark
compiler consists of passes that rewrite Futhark programs, and most of these
passes are based on local rewrites where expressions are rewritten based on
local information. A lot of Futhark's optimisation power comes from the fact
that the IR is very easy to analyse. In particular, the following principles
apply to the IR design:

1. All data dependencies are explicit. An expression depends only on those
   variables it explicitly references.

2. Expression ordering is unimportant as long as data dependencies are
   respected.

3. Duplicating expressions is harmless.

4. The result of every expression is bound to a variable, and if that variable
   is not used, then the expression can be removed.

We do violate these principles slightly when it comes to [uniqueness
types](2022-06-13-uniqueness-types.html), and unsurprisingly, this has also been
the part of the IR that has caused the most difficulty when writing
optimisations. But even they mostly impose constraints on expression ordering
and duplication, and do not violate the principle of explicit data dependencies
(and the duplication constraint can be solved with copying).

Adding accumulation effects would violate principle 3 above - a statement `ys[i]
+= v` does not produce a value that is explicitly referenced by some subsequent
computation, but is instead evaluated solely for its side effects. This means
that *every transformation that moves or duplicates code* would need to consider
the implications for effects. Compilers for imperative languages have this
problem *all the time*, but we implementers of functional languages are quite
spoiled, and I'd rather not burden every part of the Futhark compiler by adding
an effect system. However, I still want the nice operational advantages of being
able to poke directly at an array element. What is to be done?

## Accumulators

Our solution to the conundrum is either a hack or an elegant extension, based on
your perspective. While it has certain *ad-hoc* elements to it, it preserves the
principles of the IR, and is semantically clean.

Specifically, our solution was to extend the Futhark IR with a new fundamental
type, called an *accumulator*, and some additional builtin operations. An
accumulator type `acc(t)` essentially denotes a write-only view of an array of
type `t`. For example, the type `acc([n]f64)` represent a write-only view of an
array of type `[n]f64`. We then add a builtin operation `update_acc` for
updating an accumulator at a given index. This operation is polymorphic in the
array shape and so cannot be given a type using Futhark function type notation,
but we can give a specialisation for `[n]f64`:

```
update_acc : acc([n]f64) -> i64 -> f64 -> acc([n]f64)
```

That is, you pass the accumulator, an index, a value, and get back a new
accumulator. As a minor detail, we ignore out-of-bounds writes - this turns out
to be convenient in some cases.

Accumulators are constructed using a function `with_acc` that you pass an array
and a function. The function is then called with the accumulator, and must
return the updated accumulator, at which point it is converted back into an
array:

```
with_acc : [n]f64
        -> (acc([n]f64) -> acc([n]f64))
        -> [n]f64
```

Semantically, this is a *trivial* construct. We can model it like this in
Haskell:

```Haskell
data Acc t = Acc [t]

with_acc :: [t] -> (Acc t -> Acc t) -> [t]
with_acc xs f = let Acc xs' = f (Acc xs) in xs'

update_acc :: Acc t -> Int -> t -> Acc t
update_acc (Acc xs) i x =
  if i >= 0 && i < length xs
    then Acc (take i xs ++ [x] ++ drop (i + 1) xs)
    else Acc xs
```

In this model, an accumulator is just a list, and updating it changes the
corresponding element of the list.

Of course, the goal is that in Futhark, `update_acc` will directly update the
array underlying the accumulator, with constant cost, which is not the case for
the Haskell model. Some constraints are needed for this to be safe.

First, while an array is being used as underlying an accumulator, it must not be
possible to access the original array, as this would allow observation of
intermediate states. That is, the array passed to `with_acc` must become
inaccessible. Fortunately, this is actually expressible using Futhark's
[uniqueness types](2022-06-13-uniqueness-types.html) - we simply *consume* the
array passed to `with_acc`.

Second, an accumulator must not be used multiple times. This can also be ensured
by making `update_acc` consume the accumulator.

Finally, the accumulator returned by `update_acc` must be used for something.
This is actually ensured by `with_acc`, as the function passed *must* return an
accumulator, and the only way to obtain an accumulator is to return the one
passed to the function, or one returned by `update_acc.`

Type system aficionados may recognise that these are basically the properties of
[linear
types](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems).
While Futhark does not have a true linear type system, the uniqueness type
system can be used to simulate linearity constraints if we are careful.
Essentially, we are encoding effects using linear values.

We've still not been sufficiently careful however, as it is still possible to
mess up. Specifically, if we *nest* `with_acc`s, then there is nothing
preventing us from returning the same accumulator multiple times, or returning
the wrong one:

```
with_acc xs (\xs_acc ->
  with_acc ys (\ys_acc ->
    xs_acc)
  ys_acc)
```

This is pretty bad, as it would allow us to observe an array while an
accumulator exists for it. Our solution is to refine the accumulator type by
associating each with a unique *certificate* that acts a bit like a [region
tag](https://en.wikipedia.org/wiki/Region-based_memory_management) and uniquely
identifies the `with_acc` that gave rise to it. Specifically, an accumulator now
has type `acc(c,t)`, where `c` is the name of a variable of type `unit` bound by
`with_acc`, which now has the following type:

```
with_acc : [n]f64
        -> ((c: unit) -> acc(c, [n]f64) -> acc(c, [n]f64))
        -> [n]f64
```

This means that the following will now be a type error:

```Futhark
with_acc xs (\xs_c xs_acc ->
  with_acc ys (\ys_c ys_acc ->
    xs_acc)
  ys_acc)
```

The inner `with_acc` expects the function to return something of type
`acc(ys_c,[n]f64)`, but it actually returns something of type
`acc(xs_c,[n]f64)`. [Thus we are prevented from accidentally doing the wrong
thing.](https://futhark-lang.org/blog/2023-01-18-how-we-make-the-compiler-crash.html)

The treatment above assumes that accumulation is about *overwriting* a value in
an array, but our motivation is, well, *accumulation*, with an operator such as
addition. We then further extend `with_acc` to accept a *combining operator*
that is used to update the array element with the new value:

```Futhark
with_acc : (f64 -> f64)
        -> [n]f64
        -> ((c: unit) -> acc(c, [n]f64) -> acc(c, [n]f64))
        -> [n]f64
```

For AD, it will always be some appropriate addition operator, but in general it
can be any associative and commutative operator.

I rather like our design for accumulators: it is semantically simple, data
dependencies are explicit, and it piggybacks on the existing
uniqueness/consumption system that the compiler already understands and handles
correctly. There is one new edge case to consider, in that duplicating an
accumulation cannot be fixed by copying, as accumulators cannot be copied, but
this has turned out not to be a big issue in practice, as the compiler generally
avoids duplicating code. I should clarify that I am not averse to rewriting big
swathes of the compiler when needed, but adding actual effects to Futhark would
impose a *tax* on all future compiler passes as well. This is a cost I would
prefer to not pay if we can avoid it.

The implementation is also straightforward. Code generation for a `with_acc`
does basically nothing, except some book-keeping to relate an accumulator with
an array. When generating code for `update_acc`, the compiler looks up which
array the accumulator corresponds to, and simply generates the necessary memory
write.

There is one design aspect left to discuss, which is the reason we went to all
this trouble in the first place: how accumulators interact with parallelism.

## Parallel accumulators

The crucial aspect of accumulators that makes them safe is that they are used in
a linear fashion. That, however, means that we cannot simply use them from
within a parallel `map`. Consider:

```Futhark
with_acc (+) xs (\xs_c xs_acc ->
  map (\i -> update_acc xs_acc i 1)
      is)
```

Two problems arise:

1. We are consuming `xs_acc` multiple times because of the enclosing `map`,
   which is not allowed.

2. We are returning an *array* of accumulators, rather than a single one, which
   is not what `with_acc` expects. We could index the array to pick out an
   arbitrary accumulator (they all reference the same underlying array after
   all), but this breaks linearity.

We need a way to split a single accumulator into multiple accumulators, and to
join them again. Our original approach was to just add such constructs to the
IR, but we had a hard time figuring out the properties that governed safe use:
in particular, indexing arrays of accumulators is basically never the right
thing to do, unless you index *all of them*, and then later join them. After a
while we realised that the only use we ever made of these split/join operations
was to split just before a `map`, and join immediately after. This made us
decide to simply build split/join of accumulators directly into `map` itself,
which in the IR is a language construct, not a function, and thus can have
ad-hoc rules. In a sense, we decided that an accumulator can implicitly be
treated as an array of accumulators, and that constructing an array of
accumulators (as happens when `map` returns them) implicitly joins them into a
single accumulator. That is, any construction of a type `[m]acc(c,[n]f64)`
immediately becomes `acc(c,[n]f64)`. We then write the example above simply as:

```Futhark
with_acc (+) xs (\xs_c xs_acc ->
  map2 (\xs_acc' i -> update_acc xs_acc' i 1)
      xs_acc
      is)
```

Conceptually, each iteration of the `map2` is given its own logical accumulator,
which is then implicitly joined at the end.

This seems a bit ad-hoc, and whenever I end up doing ad-hoc things for
operational reasons, I want to make sure that the underlying semantics remain
clean. And indeed, we can still model this notion of accumulators in a
semantically clean way in Haskell. We have to change our representation from an
accumulator being a *list of values* to instead being a *list of the desired
updates*, that is, a list of index/value pairs:

```Haskell
data Acc t = Acc [(Int, t)]

with_acc :: (t -> t -> t) -> [t] -> (Acc t -> Acc t) -> [t]
with_acc op xs f =
  let Acc ixs = f (Acc []) in foldl update xs ixs
  where
    update xs (i, x) =
      if i >= 0 && i < length xs
        then take i xs ++ [op (xs !! i) x] ++ drop (i + 1) xs
        else xs

map_acc :: (Acc t -> v -> Acc t) -> Acc t -> [v] -> Acc t
map_acc f acc vs =
  let accs = map (f (Acc [])) vs
   in Acc $ unAcc acc ++ concat (map unAcc accs)
  where
    unAcc (Acc ixs) = ixs

update_acc :: Acc t -> Int -> t -> Acc t
update_acc (Acc ixs) i x =
  Acc ((i, x) : ixs)
```

Now `update_acc` merely adds an index/value pair to the list stored in the
`Acc`, and not until `with_acc` finishes do we manifest the updates to the
target list. Again, this is a *semantic model*: it's not the kind of code we
expect the compiler to generate, and the operational properties are all wrong.
But it tells us that the *meaning* of a Futhark IR program that makes use of
accumulators is not particularly hard to understand. The model of `map_acc` in
Haskell is particularly simple, as it just concatenates the updates produced by
the function.

Operationally, `map`ing an accumulator doesn't do anything. It's the same array
being referenced by all iterations, after all. We just have to be careful that
the updates are properly synchronised, which can be done in various ways
depending on the operator - for the case of AD and when using a parallel
backend, it will often be some kind of atomic addition.

## Accumulators in the source language

Accumulators have been part of the Futhark IR since we made the initial release
of the AD transformation, but they are not part of the source language. This is
despite accumulators potentially being useful for things beyond expressing the
output of reverse-mode AD. The reason we have not exposed them is not because we
think the compiler should keep all the fun to itself, but because the
constraints we have imposed to guarantee safe use are not expressible in the
source language.

As a reminder, safety in the core language depends on certificates and
linearity, and flexibilty depends on special-casing `map`.

To start with, the source language *is* powerful enough to encode linearity. In
a [previous blog
post](https://futhark-lang.org/blog/2024-04-18-random-numbers-with-uniqueness-types.html)
I demonstrated how to exploit this to avoid bugs when managing random number
generator states.

The problem is that the Futhark source language has very few built-in
constructs - everything is exposed through higher order functions, and higher
order functions all follow the same rules. For accumulators, the main
problematic rule is that when passing a function `f` to some higher order
function (such as `map`), then `f` may not consume any free variables - this is
because the type checker cannot reason about how many times `f` is applied. For
example, it means that nesting `with_acc` would be heavily restricted - the
outer accumulator cannot be updated inside the innermost `with_acc`.

Further, since `map` is just like any other function to the type checker, we
cannot allow implicit splitting/joining as in the IR (where `map` is a language
construct), and explicit splitting/joining of accumulators has unclear semantics
to me.

In truth, we *have* exposed accumulators in a limited *and undocumented* form,
purely for [the purpose of writing
tests](https://github.com/diku-dk/futhark/tree/master/tests/accs). Please do not
use them in real programs; it would make me sad. Anyway, [the
interface](https://github.com/diku-dk/futhark/blob/master/tests/accs/intrinsics.fut)
is this:

```Futhark
type~ acc 't

val scatter_stream [k] 'a 'b :
       (dest: *[k]a)
    -> (f: *acc ([k]a) -> b -> acc ([k]a))
    -> (bs: []b)
    -> *[k]a

val reduce_by_index_stream [k] 'a 'b :
       (dest: *[k]a)
    -> (op: a -> a -> a)
    -> (ne: a)
    -> (f: *acc ([k]a) -> b -> acc ([k]a))
    -> (bs: []b)
    -> *[k]a

val write [n] 't :
       (acc: *acc ([n]t))
    -> (i: i64)
    -> (v: t)
    -> *acc ([n]t)
```

The nomenclature is a little different: we say `write` instead of `update_acc`.
Further, we do not expose `with_acc` directly, but rather provide two constructs
that combine `with_acc` with a `map` over some input array. Also, the `acc` type
itself does not have the certificate we use in the IR, because the inability to
nest accumulators makes it unnecessary.

I don't think the inability to program with accumulators in the source language
is a *big* problem, but I have wondered a few times what it would take to make
it possible. Currently my hunch is that if we add some notion of "linear
function" that a higher order function can use to indicate that a provided
function is only applied once, or perhaps encode consumption using an effect
system, then we may be able to loosen the restrictions on consuming free
variables, which may make it possible to expose `with_acc` in a more general
way. I am reluctant to complicate the Futhark type system [for the usual
reasons](2018-06-18-designing-a-programming-language-for-the-desert.html), but I
am quite interested in the research challenge of compiling a language with
effects to one that does not have effects, but which instead models effects
using linear variables - like the Futhark IR. But perhaps this is best
investigated by designing a new language that just happens to target the Futhark
IR.
