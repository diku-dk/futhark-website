---
title: Why not tail recursion?
author: Troels Henriksen
description: Futhark has an unusual restriction on functions, but there is a good reason for it.
---

*This post was inspired by [this discussion on
/r/ProgrammingLanguages](https://www.reddit.com/r/ProgrammingLanguages/comments/1qgbq1i/why_not_tail_recursion/).*

Futhark is unusual among functional programming languages in not allowing
functions to be recursive. This is to some extent justified by Futhark being a
parallel language, and recursive looping being an intrinsically sequential
operation, and therefore less crucial. However, most nontrivial parallel
algorithms still involve some amount of sequential looping, and so this
capability cannot be left out entirely. The reason Futhark still does not allow
recursive functions is due to our goal of generating code for restricted
targets, most notably GPUs, where recursion is at best inefficient and often
impossible, due to restrictions on stack usage. Our solution to this problem is
to provide dedicated syntax for sequential looping that is semantically just a
local tail-recursive function:

```Futhark
loop acc = 1 for i < n do acc * (i+1)
```

This construct defines a *loop parameter* `acc` that is initially set to `1`,
then `n` times evaluates the body `acc * (i+1)`, each time re-binding the `acc`
parameter to the value returned by the previous iteration of the `body`. This is
equivalent to the tail-recursive function

```Futhark
def loop acc i = if i < n
                 then loop (acc * (i+1)) (i+1)
                 else acc
```

initially called as `loop 1 0`. A variation also exists for `while` loops. The
advantage of this design is that it obviously allows for looping without using
any stack space.

However, there are plenty of functional languages where recursion is the primary
or *only* way to loop. These languages usually perform [tail call
elimination](https://en.wikipedia.org/wiki/Tail_call), in which the stack space
of the caller is reused by the callee. Why don't we just do that in Futhark as
well?

One reason is that most Futhark backends do not produce machine code directly,
but rather C, which is then processed by some downstream C compiler. The pros
and cons of this design choice perhaps merits a dedicated blog post in the
future, but the main downside is that we can only express things that are easily
(and preferably *portably*) expressible in C, and tail calls are not such a
thing. There are well known techniques for implementing them, such as
[trampolining](https://en.wikipedia.org/wiki/Tail_call#Through_trampolining),
but I am worried that they will confuse the kernel compilers provided by GPU
drivers. Futhark generally benefits from generating C code that looks like what
these compilers expect, and when we deviate, we often
[suffer](https://github.com/ROCm/ROCm/issues/1182). As I have mentioned at
various times, the main goal of Futhark is not to run *at all*, but to run fast,
and [we are not interested in features that we cannot run
fast](2016-09-03-language-design.html). If your main goal is just to write a
nice functional program, then Futhark is not necessary.

Alright, so general tail call elimination is out, but what about merely tail
*recursion*? That can always be rewritten to a C `for` or `while` loop (which is
also what inspired `loop` in Futhark). Here the problem is that for Futhark,
optimising these tail calls is not just a nice optimisation, but would have to
be an ironclad guarantee - in those cases where the recursion is not in tail
position (and so can be eliminated), the program must be rejected, because
consuming stack space proportional to the recursion depth is *not* acceptable.

It turns out that most functional languages do not *promise* that tail recursion
is optimised, although they unsurprisingly tend to do a pretty good job of it in
practice. I think one of the first languages to make a *promise* in this area,
and to precisely specify the bounds of this promise, was Scheme. In the last
specification for Scheme that I understand, namely the [fifth
one](https://docs.racket-lang.org/r5rs/r5rs-std/r5rs.html), tail calls are
defined in [Section
3.5](https://docs.racket-lang.org/r5rs/r5rs-std/r5rs-Z-H-6.html#%_sec_3.5). The
rules therein are a detour from the rest of the otherwise famously elegant
definition, in that it is basically a list of syntax-driven rules for when an
application is in tail position. I think the rules are supposed to be applied
after macro expansion, but I do not see it explicitly mentioned.

Although these rules are simple and predictable, I dislike that they impose
constraints on superficial elements of your programming style. Consider a
tail-recursive factorial function written in Haskell:

```Haskell
fact acc i = if i == 0 then acc
             else fact (acc*i) (i-1)
```

We can easily imagine some syntax-driven rules for which the recursive call to
`fact` is classified as a tail call. But Haskell programmers *hate* parentheses,
and might instead write it like this:

```Haskell
fact acc i = if i == 0 then acc
             else fact (acc*i) $ i-1
```

The `$` operator is a library definition (not a language builtin) that is just
function application with a low operator priority. It serves no semantic
purpose, but allows long chains of applications such as `f (g (h x))` to instead
be written `f $ g $ h x`. It serves the same purpose as the pipeline operator
`|>` popularised by F#. The problem is that in the function above, the function
call in tail position is no longer to `fact`, but to `$`. This is thus no longer
an obviously tail-recursive function - only if we know what `$` does, perhaps by
inlining it, do we see that `fact` is still tail-recursive. This is fine when
tail call optimisation is considered a nice-to-have optimisation, but for
Futhark, where we want to reject non-tail recursion, this is not acceptable. Any
program that passes the type checker must compile ([although we fall
short](2018-12-08-why-futhark-sometimes-goes-wrong.html)), and the rules that
govern well-typedness must be simple and compositional. I like [keeping my
promises](2022-01-27-cost-models-are-contracts.html) and also making promises
that are comprehensible.

It appears a language designer is left with a choice: either allow tail
recursion under some inflexible syntax-driven rules that constrain the coding
style of the programmer, or have complicated rules or analysis in the type
checker that might sometimes reject your program for difficult-to-understand
reasons. (Or a third choice: make your type system a lot more complicated so the
behaviour of `$` and similar functions can be expressed in its type.)

Since [Futhark is a desert
language](2018-06-18-designing-a-programming-language-for-the-desert.html), we
always pick the option that adds the least complexity to the language, which is
the first option. But we further restrict it by requiring dedicated `loop`
syntax whenever tail recursion is desired. In particular, the recursive call is
implied, so there is no way you can put it in a non-tail position. This means
that there are simply fewer rules to learn and to keep in mind while programming
in Futhark. The programmer may well shake their head at the artificial lameness
of the language's refusal to accept their recursive vision, but ultimately the
challenge becomes about how to write one's algorithm to eliminate the need for
recursion, rather than to trying to work out the precise confines of the
language in an attempt to sneak in something that is never going to work.
