---
title: The Futhark Formatter
description: Now your Futhark code can look good even if it is not good.
---

Programmers are a famously acrimonious bunch, with disagreements on
matters such as naming, coding style, or which invisible character is
best, often turning into ferocious flame wars. In recent years, it has
become common to impose order on this fractious rabble through a
classic imperialistic measure, namely the application of overwhelming
centralised force, by means of automated code formatters (which also
exploits laziness, one of the [three
virtues](https://thethreevirtues.com/)).

As I can easily imagine is the case for most programmers, I was
initially loath to surrender control of my carefully formatted source
code to an unthinking machine - surely it would diverge from my own
preference. And indeed, the first formatter I used pervasively -
[Ormolu](https://github.com/tweag/ormolu) for Haskell - did not format
things exactly as I would have. But it wasn't *that* bad, mostly just
somewhat more verbose, and the convenience of automation meant that I
quite quickly preferred this to my previous habit of artisinal
formatting. In retrospect, I may have been primed for this by spending
my teenage years programming in Lisp, where I made heavy use of the
automatic indentation facilities in GNU Emacs. Due to the particular
syntactical properties of Lisp, automatic indentation is quite close
to full automatic formatting.

Ormolu is a non-configurable formatter, in the same vein as
[gofmt](https://pkg.go.dev/cmd/gofmt),
[Black](https://github.com/psf/black), and
[smlfmt](https://github.com/shwestrick/smlfmt), which I consider to be
the overall best approach. However, Ormolu has one interesting feature
that I truly appreciate and miss in other formatters, which is that it
is partially sensitive to the pre-existing formatting of the code.
Simplifying a bit, Ormolu will not turn a single-line expression into
a multi-line expression. That is, if a subexpression does not contain
any linebreaks, then the formatted subexpression will also not contain
linebreaks - but if expression is already multi-line, then Ormolu has
full freedom to rearrange it. Ormolu will also not try to enforce any
particular line width. This is not because I think line widths are
unimportant - more than 80 characters is downright rude - but because
automatic formatting of expression-oriented languages is inherently
difficult due to how splitting across multiple lines can drastically
affect how the logical structure of an expression is perceived. For
example, if we have an long chain of additions such as

```Futhark
a + b + c + d + e + f + g
```

then a formatter might plausibly decide to reflow it as

```Futhark
a + b + c
  + d + e
  + f + g
```

but perhaps this obscure the fact that algorithmically, the terms are
actually pairwise connected. A better formatting might be this:

```Futhark
  (a + b)
+ (c + d)
+ (e + f)
+ g
```

This would be accomplished by the programmer inserting the redundant
parentheses (to indicate structure to the formatter), as well as a
single line break to make the full expression multi-line, and let the
formatter do the rest. Ormolu has a notion of what an "expression" is
that goes beyond what it looks like in an AST - a sequence of repeated
applications of the same operator is considered one "expression".
Essentially, the code itself becomes a kind of configuration for the
formatter. In this case, the parenthesized expressions would be
considered single-line, and never split across multiple lines.

In many other cases, properly formatting a long expression is best
done by adding local `let` bindings to name subexpressions. This is
beyond what can be expected of an automatic formatter. In practice,
after configuring Emacs to run Ormolu whenever the file is saved, it
feels more like an automatic assistant that demonstrates to you the
formatting consequences of your code, and encourages you to rewrite
your code (in a semantically identical way, hopefully) such that it
formats in a way you find pleasant. I think this strikes a nice
balance between control and consistency. It also helps that Haskell
provides a rich vocabulary of redundant ways to write the same thing,
e.g. `f . g . h $ x` or `f $ g $ h $ x`, which influences Ormolu's
notion of how to interpret the subexpressions of the program, and thus
affects the formatting.

I've long wanted a similar automatic formatter for Futhark, one that
behaves much like Ormolu, and fortunately two students at
[DIKU](https://diku.dk) - Therese Lyngby and William Due -
[volunteered](2024-02-03-quantifying-student-projects.html) to spend a
half-semester implementing one. The result is [`futhark
fmt`](https://futhark.readthedocs.io/en/latest/man/futhark-fmt.html),
and while there are still some sharp edges to file down, I already
find it quite pleasant to use. It is of course already supported in
[futhark-mode](https://github.com/diku-dk/futhark-mode).

One thing I have been experimenting with is taking the notion of
"whitespace-sensitivity" or "code as configuration" further. For
example, the usual formatting rule is that top level definitions are
separated by a single blank line. However, many Futhark programs have
sequence of fairly trivial one-line definitions, such as the
following:

```Futhark
def vecadd = map2 (f64.+)
def vecmul = map2 (f64.*)
```

I don't think adding blank lines here makes the code any more
readable, so `futhark fmt` will detect the (special) case of
already-adjacent single-line definitions and keep them adjacent. Is
this actually a good idea for a formatter? That remains to be seen. It
departs from the principle of Python's Black formatter that any given
AST can only be formatted in a single way, but that is already the
case when you have Ormolu-style whitespace sensitivity.

I look forward to using `futhark fmt` for all Futhark code that I
write in the future, and I also look forward to people telling me that
the way it formats code is ugly. In some cases they may be right, and
then we will fix it.
