---
title: What we ended up doing about alias propagation
description: This story is over for now, and hopefully few Futhark users will notice that anything changed.
---

I recently wrote [a post about open questions regarding alias propagation in the
Futhark type checker](2026-09-22-aliasing.html). The questions have now been
resolved, and while I am not going to re-explain the entire context for this
post (it is *by far* the most complicated corner of the Futhark type system, in
the bad way), I want to summarise some of the most important design conclusions.
Some of these deviate from what I normally consider good taste in language
design, but they result in a design that is simple and supports the kind of
patterns that appear in real Futhark code. I will also touch on why I have
reasonable confidence that the design is sound, although only time will tell
whether it is also *good*.

## Context and main decisions

The basic problem is that in order to ensure safe use of [in-place
updates](2022-06-13-uniqueness-types.html), the Futhark type checker must track
whether two objects may potentially be *aliased*, meaning they share memory at
run time. Alias analysis has to be conservative, because while
over-approximating the aliases of a variable, under-approximating can lead to
unsoundness.

To a large extent, alias analysis can be done using fairly intuitive rules
stating how the aliases of an expression alias its subexpressions. The main
challenge is function calls. How can we know whether a function result aliases
its input? In Futhark, this is part of the function type. A function with a
return type of `t` indicates that the result may alias one of its parameters,
while a function with a return type of `*t` indicates that the result is
*fresh*, meaning it does not alias the parameters to the function. As an
example, this is the type of `reverse`, which returns a lazy "view" of the
input:

```Futhark
val reverse [n] 't : [n]t -> [n]t
```

And this is the type of `copy`, which returns a fresh copy of the input:

```Futhark
val copy 't : t -> *t
```

This by itself is simple enough. The problem arises when we have polymorphic
higher order functions. Consider `apply`, the function that applies a given
function to a given argument, which has this type:

```Futhark
val apply 'a 'b : (a -> b) -> a -> b
```

Since `apply` must be applicable to all kinds of functions, both `reverse` and
`copy`, it cannot claim that the result is fresh. But intuitively, it is clear
that `apply reverse x` should alias `x`, while `apply copy x` should be fresh.
Yet since `apply` declares a non-fresh result, we seem forced to conservatively
deduce that the result aliases `x`. This is sound because it is an
over-approximation, but it means that higher order functions have bad ergonomics
when they interact with aliases. The function `apply` is of course a bit
contrived in this form, but it is exactly the type of the pipeline operator
`|>`, which *is* commonly used in Futhark.

One way of fixing it would be to allow "freshness polymorphism" in the type
system. We could imagine giving `apply` this type, where the result of `apply`
is as fresh as the result of the function it is given:

```Futhark
val apply 'a 'b : (a -> F b) -> a -> F b
```

This would work, but also complicate the user-facing part of the type system.
Since the only purpose of alias analysis is to secure a small part of the
language, I'd rather avoid complicating the type language.

However, the idea of having a more elaborate type system for reasoning about
"freshness polymorphism" is a good idea; we just don't want those types to ever
appear in interfaces. The solution is to *infer* those more precise types, based
on the normal polymorphic types of Futhark functions, by exploiting
*parametricity*. If we look at the type of `apply`, it is clear that the `b`
that is being returned can *only* come from the function - so we can infer that
it must be as fresh as that function. Since `b` is a type parameter, the
implementation of `apply` cannot get a value of that type from anywhere else.

This perspective allows us to handle cases like `apply reverse x` and `apply
copy x` precisely. The rule is that if a function result is a type parameter
that occurs only once in the result, and the only way to get a value of that
type is to apply a given function parameter, then the result inherits the
freshness of that function. For example, consider this higher-order function:

```Futhark
val apply2 'a 'b : (a -> b) -> (a -> b) -> a -> a -> (b, b)
```

Here we cannot assume that `apply2 copy copy x y` has no aliases, because we do
not know whether `apply` internally applies only one of the functions and just
returns the same value twice. But now consider this more precise type:

```Futhark
val apply2 'a 'b 'c : (a -> b) -> (a -> c) -> a -> a -> (b, c)
```

Now we do know, by parametricity, that these `b`s and `c`s can only come from
those function applications.

The idea is not so difficult, but I have agonised a lot over the implementation.
Since this work is done as part of the [road towards Futhark
1.0](2026-08-26-towards-1.0.html), I very much want to avoid adding accidental
unsoundness. For that reason, this type refinement is extremely conservative.
Specifically, it only kicks in for function applications where the function is a
polymorphic variable, and the function has been fully applied to all of its
arguments. This basically means that refinement only takes place when you are
not doing tricks with partial application, and otherwise you get the "basic"
interpretation of the type, without taking advantage of parametricity. For
example, don't do this:

```
let foo = apply
in foo copy x
```

And don't do this:

```
let foo = apply copy
in foo x
```

Or rather, do it if you want, but you will get over-approximated aliases for the
results. The reason is that while handling the above is possible in a way that I
think is sound, it requires *much* more type-checking machinery, more
complicated book-keeping, subtle side conditions, and results in inscrutable
type errors - and it mostly just supports code that does not look all that
natural. It rankles me to have basically syntactic constraints in a type system,
but since this does not affect the semantics of execution, but only refinement
of certain types, I believe I can live with it.

## Why I think this design is sound

This part of the Futhark type system has been a perennial source of bugs, and we
have continuously underestimated how many edge cases would be encountered.
Pretty much every new feature has introduced unforeseen interactions. I was
naturally quite anxious about adding more flexibility. The most principled
solution is to formalise the system and prove it sound. I generally do not use
much formal methods in my work, as I find that they add too much friction when
researching new optimisations and similar, but in this case we have a type
system that is fairly stable (modulo the changes above), and we just want to
know that it actually works. While proving optimisations sound can be
challenging, proving soundness of type systems is not so bad, and as a PL
researcher, I do of course have some training in the area.

Unfortunately, "some training" does not go that far. While I can read and write
formal definitions of dynamic and static semantics, understand judgments and the
implications of various soundness theorems, I was never particularly good at
*proving* theorems, and I do not have the inclination or time to get better.
Further, nowadays a proof really ought to be mechanised, and I *really* do not
have the time or inclination to become a good Rocq or Lean user.

Many readers will probably be thinking about the elephant in the room: AI-driven
coding agents. My thoughts on these are complicated and to some extent still
undecided, but this seemed like a case where they might serve. I could specify a
small model language, including its dynamic and static semantics ("type rules"),
a soundness theorem for what must hold for well-typed programs, and have an
agent construct a Rocq proof. I don't have to understand how the proof works, as
long as I understand the theorem that it proves and the language and semantics
that it proves it for (which I do, because I defined them).

So that is what I did. I will not go into detail on the formalisation, although
it has some interesting parts, but it is basically a *tiny* subset of Futhark
that contains just three kinds of values: tuples, functions, and *objects*, with
the latter taking the same role as arrays in Futhark. The language has
let-binding, control flow, and creation and consumption of objects. The dynamic
semantics produce a "trace", essentially a sequence of events, indicating when
objects (which have identity) are observed and consumed. A trace is *good* when
an object is never observed after it is consumed. The correctness property is
that for a well-typed program, evaluating that program must always produce a
good trace. This property does not say anything about reusing memory, but it is
clear that if a consumed object is never used again, the memory it resides in
can be reused.

I started out with a trivial language, and gradually extended it, eventually
reached parametricity-based refinement of higher-order functions. Along the way,
the AI agent wrote and maintained the correctness proof, which in many cases
also involved adding additional preconditions to the type rules. In most cases,
these were mostly mechanical changes in order to make the proof go through, but
there were a few (somewhat uninteresting) tightenings of the type rules. One
interesting part of the experience is that I removed some features (such as
refinement through partial application) because they required semantic objects
in the type rules that I felt were much too complicated.

The implementation of alias propagation and checking in the Futhark compiler is
not directly based on the Rocq implementation. Rather, it is a reimplementation
in Haskell based on the same algorithm and semantic objects. Futhark has a bunch
of features that are not part of the Rocq proof (like sum types), and that is
certainly a place where errors can (and have) still sneak in, but I am now
confident that *a sound system exists*, and the question is just whether it is
the system we implemented in the compiler.

I am uncertain what to do about the formalisation. The proof itself is
completely uninteresting - it is five thousand lines of machine-generated
brute-force proof-by-induction. My Rocq knowledge is not great, but it looks
very clumsy. I am fairly convinced it is not worth reading, and contains no
great insights regarding proof techniques. It is only interesting as a
certificate that a desired property holds. Whether that type system is
interesting to others is again unclear. I obviously think it leads to a useful
or pleasant programming experience in Futhark, but at a theoretical level, its
advantage compared to affine or uniqueness-based type systems is mainly in
convenience.

## The future

I intend to take a break from working on this part of the type system. It has
some restrictions for the sake of simplicity, and even some restrictions
compared to the formalisation. Maybe we will address these in the future. The
main limitation is that if you apply the identity function to a tuple, as in `id
(x,y)`, then you lose precision in the aliasing information, as the resulting
tuple will be treated as having mutually aliased components. The formalisation
does track this precisely, but I left it out of the compiler implementation for
simplicity. We'll see whether anyone notices.
