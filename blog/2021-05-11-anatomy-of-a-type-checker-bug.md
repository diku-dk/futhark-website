---
title: Anatomy of a type checker bug
author: Troels Henriksen
description: The type checker had a soundness bug and here is how I solved it.
---

I recently fixed a bug in the Futhark type checker (or type rules -
this distinction is pretty blurry when the language is practically
defined by its implementation).  At first I feared a deep conceptual
problem that would be difficult to fix, but the eventual solution
ended up being fairly clean and simple.  I think the entire process is
a good case study of what such bugs look like, and so I'm writing up
the problem and its solution for those readers who just can't get
enough of hearing about other people's buggy compilers.  I'm also
writing it up as a warning against the complexities of [substructural
type
systems](https://en.wikipedia.org/wiki/Substructural_type_system) -
avoid implementing them if at all possible (and as we shall see, the
Futhark type system has some quirks that make the situation even
worse).

## Background

Futhark is a language in the ML family, and its type system is mostly
conventional.  Parametric polymorphism and the like behaves as one
would expect.  But there are some novelties.  Perhaps the most unusual
one is a form of [uniqueness
types](https://en.wikipedia.org/wiki/Uniqueness_type), which we added
to support certain important patterns in high-performance array
programming.  For example, the expression `A with [i] = x` produces a
the array `A`, except that the value at index `i` has been replaced by
`x`.  However, it is quite inefficient to copy all of `A` just to
modify a single value.  Instead, the Futhark type rules require that
the original array `A` is not used afterwards.  This enables the
compiler to *implement* the expression as an in-place update of
whatever memory `A` happens to be stored in, even though
*semantically*, there are no side effects.  However, the requirement
that `A` is "not used afterwards" can get pretty subtle.  Here's a
simple example:

```Futhark
let B = A
let C = A with [i] = X
```

Since `B` is just another name for `A`, we also must not use `B`
afterwards.  By defining various *aliasing rules*, the type checker
can track how different variables may (potentially) refer to the same
underlying "value", and when a value is then *consumed* in a
`with`-expression, we mark all of the aliases as similarly consumed
and inaccessible.  This is necessarily conservative:

```Futhark
let C = if p then A else B
let D = C with [i] = x
```

In the above we must treat `C` as potentially aliasing *both* `A` and
`B` - hence neither of these may be used after the `with`-expression.
This is slightly complicated to implement in a type checker, but
tractable.  But then what about the following?

```Futhark
let B = f A
let C = A with [i] = X
```

Now `B` is produced by applying some function `f`.  Does the result
alias `A`?  Does it alias some other random value that might exist
somewhere?  Who knows!  Well, the type checker will have to know.
This is precisely the problem that is solved by uniqueness types, by
having each function encode which of its arguments it *consumes*, and
which of its return values are guaranteed to be *unique* (have no
aliases).  For example, the type of `reverse` in Futhark is as follows:

```Futhark
val reverse [n] 't : [n]t -> [n]t
```

Given an array of `n` `t`s (whatever that is), it returns another
array of `n` `t`s.  The `reverse` function does not consume its input,
meaning we can `reverse` an array and then still use it afterwards,
but it also does not return a unique array, meaning that the result of
`reverse` will be considered to alias its arguments.  In contrast,
this is the type of `copy`:

```Futhark
val copy 't : t -> *t
```

Given any value of some type `t`, `copy` returns a *unique* `t`
(that's what the asterisk means), implying that the result is
guaranteed to be free of aliases.  Now consider a function `update`:

```Futhark
val update [n] 't : *[n]t -> i64 -> t -> *[n]t
```

This function takes a *unique* array as its first argument, and also
returns a unique array.  This means that after an application `update
A i x`, the array `A` will be considered *consumed*, and must not be
used again - just in a `with`-expression.

Once you get used to them, these rules provide a fairly simple way of
writing code with an imperative cost model, yet purely functional
semantics.  Importantly, they are *opt-in* - if you don't need this
feature, you can mostly ignore these details (except that type
inference may sometimes put mysterious asterisks on your return types,
when it figures out that a function result will never have aliases).

## The problem

Futhark's implementation of uniqueness types is a
[substructural](https://en.wikipedia.org/wiki/Substructural_type_system)
and *non-syntactic* type system, which in practical terms means that
it can be *really tricky* to implement correctly.  In particular the
tracking of transitive aliases is not just an implementation
challenge, but it can also sometimes be difficult to explain to the
user why something is not allowed:

```Futhark
let B = A
let C = B
let D = A with [i] = x
```

After the above, `C` must be treated as consumed, as `C` aliases `B`,
and `B` aliases `A`, which is consumed in the `with`-expression.

But still, just track the names, right?  Well, even though this type
system feature has been around [since before Futhark was called
Futhark](https://github.com/diku-dk/futhark/issues/9), we *still*
discover fundamental and slightly embarrassing bugs in our
implementation from time to time.  The latest bug concerns the case
when *there are no names to track*:

```Futhark
let dup x = (x,x)

let main (xs: []i32) : (*[]i32, *[]i32) =
  dup (copy xs)
```

Here we have a function `dup` that duplicates its input.  It has the following type:

```Futhark
val dup 't : t -> (t,t)
```

Nothing unexpected - it doesn't consume its argument, and it returns
two non-unique results, meaning that they will be assumed to alias the
argument.

The `main` function takes a non-unique array as input and expects to
return two *unique* arrays.  It does this by first copying `xs` to
remove all aliases (fine) and then duplicates the resulting alias-free
array, producing *two* alias-free arrays, as far as the type checker
is concerned.  But clearly things have gone awry - `dup` does not
produce two *copies*, it just produces the same value *twice*.  This
should not type check!  Let us try rewriting the `main` function a bit:

```Futhark
let main (xs: []i32) : (*[]i32, *[]i32) =
  let tmp = copy xs
  in dup tmp
```

Now the type checker will construct an array `tmp` with no aliases
(fine), and the call to `dup` will then produce two arrays, both of
which alias `tmp` (and hence each other).  When we then claim that
these two arrays, which alias each other, constitute two *unique*
arrays, the type checker will complain:

```
A unique tuple element of return value of "main" is aliased to some other tuple component.
```

So the problem is that nameless intermediate results inhibit our
ability to track aliases correctly, because alias tracking is based on
names!  If something does not have a name, *then it is invisible*.
That's bad.  In this case, as in many others that involve holes in our
type checker, we were actually saved by the Futhark compiler's design,
where the *core language* is (almost) as tightly typed as the source
language, and [assigns names to all intermediate
results](https://en.wikipedia.org/wiki/A-normal_form).  This *defence
in depth* has proven extremely useful in avoiding code generation bugs
(most become compiler crashes instead), but bugs are still bugs, and
should be fixed.  Anything that passes the source language type
checker only to crash in the core language is *our fault*, not the
users'.

## The solution

If the problem is that intermediate results do not have names, then
the solution is intuitively obvious: give them names.  Specifically,
after every function application, we invent a "fictional" name that
does not correspond to any variable in the program, and add it to the
aliasing of the result type.  In effect, we pretend that every
function application is `let`-bound, but we don't actually change the
AST.  This allows us to track that the two values produced by `dup`
both alias the same underlying value, and thus reject the program.

Done!  Well, almost.  This solves the *soundness* issue, but there's
more to a compiler than just avoiding errors.

Consider this program, which suffers from a very similar problem:

```Futhark
let main n =
  let (a,b) = let y = iota n
              in (y,y)
  let a = a with [0] = 0
  let b = b with [0] = 0
  in (a,b)
```

Here the problem is that `a` and `b` both alias `y`, so we should not
be allowed to consume both of them, but `y` has gone out of scope by
the time the `with`-expressions occur.  We address this by using the
"fictional" name to track the aliases, but the error message suffers:

```
Consuming "internal_app_result", but this was previously consumed at 4:7-10.
```

Compiler errors that just vomit out some internal name or data
structure are not exactly *rare* in research compilers, but it is a
rare programmer who likes to be faced with them (except the compiler
developers themselves, who will be pleased to be shown exactly what is
wrong!).

To improve matters, I added [a
table](https://github.com/diku-dk/futhark/blob/20d144e2f1b3ce8a587b98bf0110108ee2c12f07/src/Language/Futhark/TypeChecker/Terms.hs#L349)
that maps these names to [a description of the application that gave
rise to
them](https://github.com/diku-dk/futhark/blob/20d144e2f1b3ce8a587b98bf0110108ee2c12f07/src/Language/Futhark/TypeChecker/Terms.hs#L324-L328),
which is then used to [synthesise a error
message](https://github.com/diku-dk/futhark/blob/20d144e2f1b3ce8a587b98bf0110108ee2c12f07/src/Language/Futhark/TypeChecker/Terms.hs#L330-L335):

```
Consuming result of applying "iota" (at 2:23-28), but this was previously consumed at 4:7-10.
```

The phrasing is perhaps clumsy, but at least it is relatively clear,
and *points to the source of the problem*.  The idea of using a
mapping from compiler-generated names to "reasons" for their existence
was first used to make [size type errors less
incomprehensible](2020-03-15-futhark-0.15.1-released.html#size-types),
and I think it works pretty well.  Futhark's error messages are still
not as good as those produced by some other compilers, but given our
limited resources, I think they are pretty decent.
