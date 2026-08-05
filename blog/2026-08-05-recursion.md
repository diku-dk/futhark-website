---
title: Finally adding recursive functions to Futhark
description: For a long time, Futhark was highly unusual among functional languages by not supporting recursive functions, but this may change - although you still may not want to use recursive functions.
---

Futhark is unusual among functional languages by not currently supporting
recursive functions. This is not because we think recursion is inherently bad -
the Futhark compiler itself contains plenty of recursive definitions. The reason
is that until recently, we did not have a good answer for how to compile
recursive functions in all positions on every backend, and in Futhark we prefer
making only promises we can keep. This post will explain why recursion has been
a touchy subject for all of Futhark's existence, why we think we can finally
address it, some unexpected trouble that arose when I thought it would be easy,
and why Futhark programmers still may want to not use it much.

I considered titling this blog post "restoring recursion to Futhark", since
*technically* the very earliest drafts of Futhark did support recursive
functions [until they were removed in
2017](https://github.com/diku-dk/futhark/issues/273). That means we have been
without them for most of Futhark's lifetime, and particularly during the era
where Futhark grew into a proper usable language with features like
[modules](2017-01-25-futhark-module-system.html), [higher-order
functions](2018-04-10-futhark-0.4.0-released.html), and [size
types](2019-08-03-towards-size-types.html). Further, they never worked all that
well, and would crash the compiler if you used recursion in the wrong place.

## Why not recursion

The reason we do not currently have recursive functions is fairly simple: they
do not work well in GPU kernels, for multiple reasons. The most immediate one is
that GPU threads necessarily have tiny stacks. Another reason is that it is also
impractical (or slow, or impossible) to allocate memory from within GPU code,
meaning all memory has to be pre-allocated in advance - this is difficult when
you have a recursive function, where the total memory requirements depends on
dynamic control flow decisions. Plain sequential `loop`s are simpler, because
once a loop iteration is done, you never return to it - but for (non-tail)
recursive functions, you have to keep the activation record intact for an
unbounded period of time.

It is not difficult to imagine a rule that says "you can have recursive
functions, but not in code that ends up running on a GPU". What is more
difficult is precisely specifying such a rule. Futhark is not a "GPU language",
but rather a hardware-agnostic data parallel language, and we do not want to
complicate the type system to track whether a function contains recursion.
Futhark has several compiler backends that target CPUs, and for those recursion
is straightforward to implement, but the *possibility* of compiling a program
for a GPU means that language restrictions are composed of the union of the
restrictions of all supported compiler targets.

This is because the overarching dream of my own research is that if you have a
function `f`, then I want to be able to `map` that function `f` efficiently over
a collection `xs`, no matter the size of the collection `xs`, what `f` might do,
or what machine this is running on. Since people keep coming up with new
machines and new `f`s, I will probably spend my entire career on this, but I
digress. My point is that I would not be satisfied with a solution that is
complicated or lacks orthogonality, and I don't want functions that cannot be
`map`ped.

It turned out that lack of recursion is not *that* big a deal in a functional
array language like Futhark. Since the core datatype (arrays) is not inductive,
there is not that much to recurse over, and Futhark provides [loop
syntax](../examples/loops.html) for expressing certain kinds of tail recursion
(although we do not plan to support actual tail call optimisation for reasons
[previously explained](2026-01-20-why-not-tail-recursion.md)).

Still, there are some cases where recursion is the natural way to solve
problems, mainly recursive divide-and-conquer. Many of the algorithms in [A
Library of Parallel
Algorithms](https://www.cs.cmu.edu/~scandal/nesl/algorithms.html) are quite
awkward to express in Futhark, and require manual transformation and flattening
(but do run fairly fast once that is done). It sure would be nice if we would
write code as nice as what Guy Blelloch did in NESL.

## Handling recursion with flattening

There are many special cases of recursion that can be handled in various
efficient ways (tail recursion is one), but before we can get to that, we need a
*general* solution for *all* cases, even if that general solution will in the
future merely be a *fallback* solution. Our general solution (which is only
necessary when using a GPU backend, remember) comes from
[flattening](2026-07-31-full-flattening.html). When flattening an expression
`map f xs` where `f` contains recursion, the recursion is effectively
"interchanged" with the `map`, meaning the recursion ends up *outside* of all
parallel code sections. From an operational perspective, after flattening all
recursive control flow is executed on the CPU, while the GPU performs fully flat
parallel operations. To give an intuition for how flattening works in this case,
suppose we have the hello world of recursive functional programming:

```Futhark
def fact n = if n == 0 then 1 else n * fact (n-1)
```

And now suppose we are mapping this function over an array of integers:

```Futhark
map fact ns
```

Obviously this is not a useful thing to do, but it serves as a simple example.
Among various boilerplate, flattening will produce a *lifted* form of `fact`
that is as recursive as the original `fact`, but operates on an array `ns`
instead of a single `n`. This lifted function simulates a "single iteration" of
the recursion, and then performs a recursive call with those elements of `ns`
that have not yet reached their base case. Simplifying the book-keeping and
notation and adding explanatory names, it looks like this:

```Futhark
def fact_lifted ns =
  let (ns_zero, ns_nonzero) = partition_inputs ns
  let ns_nonzero' = map2 (*) ns_nonzero (fact_lifted ns_nonzero)
  let result = combine_inputs (map (const 1) ns_zero) ns_nonzero'
  in result
```

As in the flattening post, I'm mostly hand-waving how `combine_inputs` manages
to put things together in the right order. The key property is that
`fact_lifted` is only ever invoked from the CPU. Of course, while this works, it
is in many cases horrendously slow - both `partition_inputs` and
`combine_inputs` hide a lot of data movement, but in the near term, *general
correctness* is all we care about - performance can come later.

I thought that once we had an implementation of full flattening, recursion would
be easy to add. Of course, nine years of intensive development of the compiler
meant that many parts assumed non-recursive programs, and had to be repaired.
Most of these changes were not very interesting, but a few uncovered issues that
I had not predicted.

## Handling recursion in the type checker

Futhark's type checker [was recently largely
rewritten](2026-07-21-rewriting-the-type-checker.html), and proper support for
recursion was part of the motivation. Since our type checking algorithm is
basically conventional Hindley-Milner, supporting recursion was straightforward,
although I had a moment where I realised that I actually didn't *know* how to do
it, because all my Hindley-Milner implementation experience was for nonrecursive
languages.

Similarly to Standard ML, but unlike Haskell, Futhark does not support
[polymorphic recursion](https://en.wikipedia.org/wiki/Polymorphic_recursion) -
any recursive call must instantiate the recursive function with the same type
arguments. This is largely driven by Futhark's compiler implementing parametric
polymorphism through monomorphisation, which does not traditionally mesh well
with polymorphic recursion ([although some forms can be
handled](https://dl.acm.org/doi/10.1145/3720472)).

Yet *complete* monomorphic recursion turned out to interact poorly with size
types. Consider this reasonable function for summing arrays whose size is a
power of two:

```Futhark
def sum [n] (xs: [n]i32) =
  if n == 1 then xs[0]
  else sum (map2 (+) (take (n/2) xs)
                     (take (n/2) (drop (n/2) xs)))
```

Technically, this is polymorphic recursion: we are defining the function for an
input of type `[n]i32`, and the recursive call is with an argument of type
`[n/2]i32`. However, monomorphisation does not take specific sizes into account,
so there is no reason to rule out this definition. Hence, Futhark allows
*size-polymorphic* recursion, but still not polymorphism in type parameters.

As a minor detail, Futhark programs are type checked in strict top-to-bottom
order, and for this reason we do not allow mutual recursion. There is no deep
reason why not. In fact, the main obstacle is that we would need to come up with
a syntax for indicating that two functions are part of the same "binding group",
or whatever we end up calling it.

## Higher-order recursive functions

The most subtle problem we had to address turned out to involve
defunctionalisation. As a reminder to those readers who do not maintain
encyclopedic mental notes of Futhark implementation details, Futhark implements
higher-order functions via defunctionalisation, where any use of a higher-order
function is turned into a higher-order function by specialising at compile-time
based on the higher-order arguments provided. As an example, if we have

```Futhark
def apply (f: i32 -> i32) (x: i32) = f x
```

then an application `f (\x -> x + 1) 2` will result in a specialised function

```Futhark
def apply_specialised (x: i32) = (\x -> x + 1) x
```

and the original call will be rewritten to `apply_specialised 2`. There's a
little more to it than this, since the higher-order functions may be closures,
so the specialised functions have parameters corresponding to captured
environments - see [our paper](../publications/tfp18.pdf) for details. That does
not matter for the problem I would like to discuss, however. Consider a
recursive higher-order function like this:

```Futhark
def naughty (f: i32 -> i32) (x: i32) =
  if x == 0 then f 42 else naughty (\y -> f y + 1) (x-1)
```

This function contains a recursive call to itself, but the higher-order
parameter `f` is passed an a new function *that captures the old function* in
every recursive call. If we imagine what this might look like when evaluated in
a normal functional language, we can think of a linked list of closures, with
each `f` linking to the previous one. From a Futhark perspective, this is
already something that raises suspicion, since even though we add recursive
functions, we still do not want recursive *data*. But the real problem is
actually that the "shape" of the function passed to `naughty` cannot be known
statically, since it depends on the depth of the recursion, which is controlled
by the dynamic value `x`. Any attempt to defunctionalise `naughty` results in an
infinite number of specialisations, and indeed, that is what happened.

The simplest solution to this problem is to not allow recursive higher-order
functions at all, meaning they do not have to be defunctionalised at all. I do
not actually have any compelling examples of recursive higher-order Futhark
functions at hand, but it still felt a bit too restrictive. After all, the
coolest part of creating a programming language is when people use it to [write
programs you had not thought
of](https://dl.acm.org/doi/10.1145/3471873.3472976). Hence we impose a slightly
simpler restriction: in a function, any recursive call must be provided
arguments for all of its higher-order parameters, and they must be syntactically
identical to the corresponding parameter in the definition. Intuitively it means
you need to pass the same functions in the recursive call as were passed to you.
This bans `naughty` above, and means that when generating a specialised version
of a higher-order function, we can assume that any occurrence of that function
in its own body will refer to the *same* specialisation, neatly tying the knot.
This rule will not work with mutually recursive functions, but we'll cross that
bridge if we ever get to it.

Although a bit ad-hoc (syntactic checks?!), it is at least a simple rule, and I
greatly prefer slightly inflexible rules to subtle but more flexible rules, and
surely the inability to handle mutual recursion will not be a problem.

## Lambda lifting can introduce mutual recursion

Apart from monomorphisation and defunctionalisation (and many other things), the
Futhark compilation pipeline *also* performs [lambda
lifting](https://en.wikipedia.org/wiki/Lambda_lifting). This is a program
transformation where any lambdas are turned into named top level functions. The
purpose is to simplify subsequent compiler passes and code generation. As an
example, consider this contrived version of `fact`, where we add a useless
lambda:

```Futhark
def fact_lam (n: i64) =
  if n == 0 then 1 else (\n' -> fact_lam n' * n) (n-1)
```

Applying lambda lifting to this program will result in this, where `lifted_lam`
corresponds to the lifted lambda:

```Futhark
def lifted_lam (n: i64) (n': i64) =
  fact_lam' n' * n

def fact_lam' (n: i64) =
  if n == 0 then 1 else lifted_lam (n-1)
```

Now we have a problem: `fact_lam'` calls `lifted_lam`, and `lifted_lam` calls
`fact_lam'`. These functions are mutually recursive. However, it turns out not
to be a real problem, as lambda lifting a function that obeys the restrictions
on recursive higher order functions does not produce the "infinitely growing"
cases that can loop defunctionalisation. Hence, as long as we ensure that the
various compiler passes downstream of lambda lifting can handle mutual
recursion, we will be fine. It did take me by surprise for a minute, though.

## That's it

This is what it took to implement recursive functions. Fortunately, the code
generator was already able to deal with recursive functions in CPU code, largely
because it tends to *just work* in a straightforward implementation. Now, I
should re-emphasize that the big hammer for handling recursion, flattening, will
often produce *dreadful* code. We really need to look into which special
patterns can be recognised and handled more efficiently. I also found through my
(very sparse) experiments that writing recursive data parallel code can quickly
consume a lot of memory, since your "Stack frames" end up containing large
arrays that are memory-managed [as
monoliths](2018-01-28-how-futhark-manages-gpu-memory.html), in contrast to
classic functional languages that allow for more fine-grained liveness tracking.
I am slightly worried that future Futhark programmers will tend to reach for
recursion even in cases where it is not the best tool for the job.

As of this writing, Futhark is not enabled in Futhark's `master` branch (nor in
any release), but the [pull request that adds recursive
functions](https://github.com/diku-dk/futhark/pull/2505) merely removes the
check that disables it, so all of the supporting code is already merged and
active. It is also possible to experimentally allow recursion by setting a
secret environment variable appropriately, although if any users want to
experiment, it is best to do that on the branch that implements flattening (or
just wait for it to be merged, which will probably be soon).

I still have not said much about how to actually *use* recursion to write
potentially interesting programs. That will have to wait for the next post,
because it turns out that to exploit the full power of data-parallel recursive
programming, we need to add a new programming construct that more directly
exposes flattening to the user.
