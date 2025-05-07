---
title: Implement your language twice
description: Advice for all past, present, and future language implementers.
---

One of the challenges you face when programming is figuring out what your
program will do when you run it. To most programmers, the obvious way of solving
this conundrum is to run the program and see what it does, then modify the
program until it does what they would like it to do. Essentially you treat the
language implementation (whether a compiler or interpreter) as an oracle: an
unambiguous source of truth on the meaning of programs.

Things are more challenging when you are the language implementer. How do you
know whether the implementation of the language is correct? Again, there is a
simple but perhaps not very useful solution: the language implementation is
*always correct* by definition if you simply specify the language as being
whatever the language implementation does. However, even so-called
"implementation-defined languages" (such as Python) are not truly this
extremist. Typically there are at least implicit correctness criteria (e.g. the
interpreter ought not crash), and quite commonly there is some language
description that specifies the semantics of the language to some degree of
precision, and against which an implementation can be compared.

In most cases such language specifications are only partially formal. It was a
significant achievement when [The Definition of Standard
ML](https://smlfamily.github.io/sml97-defn.pdf) was published in 1997, and it
remains one of very few general-purpose real-world languages that are defined by
a formal specification. Most (as far as I know, *all*) of the major industrial
languages come with largely informal specifications. While you *can* find formal
specifications for languages such as C, these specifications are not
*normative*. They do not *define* C, but merely *describe it*, and usually only
a subset. Even academic darlings such as Haskell are not defined by a formal
specification, although various subparts of the language have been formally
specified (and verified) in isolation. The reason is that formal specifications
are difficult to write and to maintain, and are usually judged not worth the
effort compared to semi-formal specifications, like the one for C.

For an independent (or hobbyist) language developer, even such an informal (but
still precise) specification is difficult to write and maintain as the language
changes, and most fledgling languages will change rather often (that's the fun
part of language design). However, the absence of a specification does raise
questions about correct behaviour in edge cases, and whether the particular
behaviour of the current implementation is *intentional* or *accidental*. Even
independent implementers of niche languages ought to care about that.

This problem afflicted us particularly badly in Futhark, which from the start
was mainly characterised by its aggressively optimising compiler. Especially
during the early days, many edge size-related edge cases (e.g. [empty
arrays](https://github.com/diku-dk/futhark/issues/398)) were handled by inlining
and simplifying until some particular behaviour was convenient to implement. One
of the important correctness criteria for an optimising compiler is that it
should not change the observable behaviour of a program. (Life for the compiler
writer would be easier if programmers were less particular about what their
programs do.) One way to verify this is to simply turn off optimisations and
mandate that programs must still do the same thing. But for a language like
Futhark, not all program transformations are optional the way that optimisations
are. Things like
[defunctionalisation](../publications/tfp18.pdf),
monomorphisation, and
[defunctorisation](../publications/icfp18.pdf) are
*mandatory* for our compilation scheme to work at all.

Further, on an aesthetic level I dislike specifications of program behaviour
that involve first performing a nontrivial rewrite of the program. That is
certainly not what the Definition of Standard ML does, and even if I do not have
the resources to produce a complete formal specification, I still want the
semantics to be clean and simple enough that such a specification is *possible*.
I think that also leads to semantics that are easier to understand by humans.

The solution we reached in Futhark is to have a completely distinct
implementation of our evaluation semantics. Most users of Futhark will have used
`futhark repl`, but apart from being a convenient way to run code, it also
serves an important purpose as the *reference implementation* of the Futhark
language. The
[interpreter](https://github.com/diku-dk/futhark/blob/master/src/Language/Futhark/Interpreter.hs)
underlying `futhark repl` *is* the implementation that defines what Futhark
*is*. To this end, the interpreter is written in a very direct style: it is an
AST walker that does no pre-processing of the program beyond type checking. Any
semantic ugliness in Futhark manifests as code ugliness in this implementation.
(And there are some things I mean to write about in the future.) One downside is
that the interpreter is *slow* - and not just compared to the compiler. (Most
things are slow compared to the compiler.) The Futhark interpreter is only
suited for debugging and experimentation on small inputs.

There is of course no guarantee that the behaviour of the compiler and the
interpreter coincide, but [whenever differences
occur](https://github.com/diku-dk/futhark/issues/2239), one of them can be
modified accordingly. In most cases it is the interpreter behaviour that is
correct, [except when it
crashes](https://github.com/diku-dk/futhark/issues/2258) (although a purist
might perhaps insist that the compiler should be modified to crash likewise).
There are also some cases where the compiler is allowed to diverge from the
interpreter. One somewhat uninteresting case is when the program does blatantly
incorrect things, such as [reductions](../examples/scan-reduce.html) with a
non-associative operator. Other cases are more tricky - in particular, the
compiler [does not promise to preserve nontermination or similar
errors](https://futhark-lang.org/blog/2023-03-17-on-nontermination.html). These
can be removed through optimisations such as dead code removal, which the
interpreter will not do.

In the absence of formal specification (and importantly, a way of verifying an
implementation against said specification), merely producing two implementations
is a good way of ensuring that the semantics are *intentional* and not merely
*implementation-accidental*. In principle, these implementations could be
completely independent. For Futhark, the interpreter and compiler share the same
frontend (parsing and type checking). My estimate is that the risk of accidental
behaviour in those parts is not as great, and it is difficult to conceive of a
reference type checker that is implemented in "the most simple and
straightforward way", the way we can do with an unoptimised interpreter.

In conclusion, I encourage all language implementers to write (and maintain!) a
reference interpreter for their language. It is likely that you already
construct such an interpreter during the early stages of language development,
so why not keep it around for testing and verification? If the burden of such an
interpreter is too great, then perhaps that is a sign that the language
semantics have grown too complicated.
