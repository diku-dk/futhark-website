---
title: Which tokens are the most frequently used in Futhark programs?
description: Digging real deep into the sack of ideas now, but the content must flow.
---

Have you ever wondered which tokens are the most frequently used in
Futhark programs? By the end of this post you will wonder no more. One
of the more obscure commands provided by the `futhark` binary is
[`futhark
tokens`](https://futhark.readthedocs.io/en/latest/man/futhark.html#futhark-tokens-file),
which takes a program, runs it through the lexer, and prints the
constituent tokens. For example, given a program `foo.fut`

```Futhark
def main x = x + 2
```

we may obtain the following:

```
$ futhark tokens foo.fut
DEF
ID (Name "main")
ID (Name "x")
EQU
ID (Name "x")
SYMBOL Plus [] (Name "+")
NATLIT (Name "2") 2
```

The original purpose of this command was to help in developing [the
formatter](https://futhark-lang.org/blog/2024-12-01-futhark-fmt.html),
although it's also mildly handy for debugging the lexer. But how often
do you really modify a lexer? Another thing it can be used for is
performing crude statistical analysis on Futhark programs, answering
interesting questions such as *are all parens matched?* or *what is
the most common variable name?*. You could certainly write proper
programs to answer these questions on Futhark syntax trees, but it's
rather more convenient to feed `futhark tokens` into a ~~shell
script~~ sophisticated data analysis pipeline.

As the subject of my investigation, I use [the Futhark benchmark
suite](https://github.com/diku-dk/futhark-benchmarks) - a collection
of about 30k source lines of Futhark code (albeit with some
duplication), written by various programmers.

First we obtain the raw data:

```
$ find . -name \*.fut -exec futhark tokens {} \; > tokens.txt
```

This produces a file containing 327469 lines, which I will not
reproduce in its entirety. The Futhark lexer is a bit unusual in that
comments are not disregarded, but actually turned into tokens. While
these tokens are disregarded by the parser, they are useful to the
formatter, which must reinsert them in appropriate locations while
formatting the program. I count the number of comments thus:

```
$ grep ^COMMENT tokens.txt | wc -l
9450
```

Actually this is not entirely correct, as documentation comments are a
different token, so I try again:

```
$ grep -E '^(DOC|COMMENT)' tokens.txt | wc -l
11116
```

Out of 327469 tokens, 11116 are comments.

Now let us count things that are by definition more meaningful than
comments. Let us compute the number of occurences of each token. Since
the name of a variable (or the contents of a comment) is part of the
token, this also lets us find the most frequently used variable names:


```
$ sort tokens.txt |uniq -c | sort -n
...
   4425 ID (Name "x")
   5194 ID (Name "n")
   5573 ID (Name "i32")
   5652 DEF
   6495 LBRACKET
   7218 LET
  10273 RBRACKET
  10395 DOT
  12028 COMMA
  14180 COLON
  18411 EQU
  24604 LPAR
  24604 RPAR
```

The two most frequently used tokens (evenly tied) are `(` and `)`.
This is not such a great surprise. We find `=` in a distant third.
Interestingly, `let` is used only a bit more than `def`. This suggests
that many Futhark definitions do not have very many local bindings.

One surprising result is that the token `RBRACKET`, corresponding to
`]`, is used 10273 times, while the token `LBRACKET` (`[`) is only
used 6495 times. Doesn't Futhark require matching use of square
brackets? It does, but the lexer plays games with `[` depending on
what comes after. In Futhark, we want to distinguish `x [i]` (applying
a function to an array literal) from `x[i]` (indexing an array). [The
solution we decided
upon](https://futhark-lang.org/blog/2016-12-09-two-syntax-design-problems.html#array-indexing)
is for the lexer to produce a different token, `INDEXING` when a `[`
follos an identifier. And indeed, we find 3626 occurrences of
`INDEXING` in the token list. This still doesn't quite add up, but we
also use a similar trick to detect `#[` (for
[attributes](https://futhark-lang.org/blog/2020-06-28-attributes-in-futhark.html)),
which makes up the difference.

As shown in the output excerpt above, the single most used identifier
in Futhark programs is `i32`. I strongly suspect this is not because
of its use in variable names, but rather as a reference to the type in
explicit type ascriptions. The next most used name is `n`, which is
almost certainly due to its common use in [size
parameters](https://futhark-lang.org/blog/2019-08-03-towards-size-types.html).
The name `x` is of course also a popular choice when we don't know
what else to call something. The most popular symbolic name is `+`,
where I am again supposing that this is not because of its use as a
variable name.

We can also answer more exotic questions. For example, the longest
variable name at 49 letters is
[`flux_contribution_nb_density_energy_z`](https://github.com/diku-dk/futhark-benchmarks/blob/7d97653fae46657406b686092545240d6bf58910/rodinia/cfd/cfd.fut#L162).
The average length of a variable name is 16, and the median is 15. A
total of 13151 number literals are used, but only 963 distinct ones
(ignoring syntactic differences), with the five most common being `0`,
`1`, `2`, `1.0`, and `0.0`. The constant `255` is also pretty common,
with 148 occurences. Generally the most common numbers are the
constants you'd expect (such as powers of two), with
`15241094284759029579` being a weird outlier at 32 occurrences - it
turns out this is originally a hexadecimal constant used in a random
number generator. The sum of all number literals is
712554951466320527360, but the median is only 1. As usual, those with
the largest numbers exhibit a disproportional influence.
