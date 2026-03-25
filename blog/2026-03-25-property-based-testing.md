---
title: How should property-based tests be defined in Futhark?
author: Troels Henriksen
description: Property-based testing is challenging to fit into a restricted language such as Futhark, but it turns out that the biggest question is a fairly superficial design issue about how to specify the tests - because many approaches are viable.
---

Property-based testing, popularised by
[QuickCheck](https://dl.acm.org/doi/10.1145/351240.351266), is a style of
testing where instead of providing input/output pairs for some function, you
define a property that is expected to hold, and a library then generates random
inputs and checks whether the property holds. The classic example of a property
is that reversing a list twice should result in the original list.
Property-based testing is essentially a search problem where we try to find
counterexamples to the stated properties.

This post is related to ongoing efforts towards implementing a tool for doing
property-based testing for Futhark, with all of the work carried out by Matin
Nafar and Simon August Mørk, who are students here at [DIKU](https://diku.dk).
They do good work and I have confidence that they will ultimately produce a
working tool ([draft PR for the
curious](https://github.com/diku-dk/futhark/pull/2405)). There are many
technical challenges to overcome due to the
[peculiarities](2018-06-18-designing-a-programming-language-for-the-desert.html)
and [limitations](2016-09-03-language-design.html) of Futhark, but those are not
actually going to be the main topic of discussion here, as they have clear
technical solutions. No, the largest question has for me turned out to be about
how the user is supposed to even indicate which properties exist, and what
implications the possible solutions carry for the overall usage experience.

In most languages, a property-based testing library is just another library,
with input generation, error handling, properties, and printing of output being
orchestrated by normal code. In Haskell's QuickCheck this is exposed through
various monadic interfaces because random input generation is of course
stateful, but it still is just ultimately *normal Haskell*. Ultimately, the way
you run a property is that you pass it to the function `quickCheck`. If it finds
a counterexample to the property, `quickCheck` will print an appropriate error
message.

Unfortunately, Futhark is not a general purpose language, and it is not well
suited for expressing a property-based testing library. Among other things,
Futhark does not let you handle errors, and since it is pure, you *cannot print
anything* - that certainly makes it hard to explain the counterexamples found,
leading to a rather unfortunate user experience where the tool will claim that
something is wrong, but provide no further information. Not all users will enjoy
this exciting mystery.

Instead, [to use Futhark in
practice](https://futhark.readthedocs.io/en/latest/usage.html), you compile the
program to a library and invoke its declared *entry point functions*. This can
be done either through the [C
API](2017-09-26-calling-futhark-from-c-and-haskell.html), or through a [RPC-like
"server protocol" over Unix
pipes](2021-01-18-futharkscript.html#futhark-server-mode) defined on top of the
C API. The latter is much easier, as it does not require any linking. We can
then define a property as a function that takes a single input and returns a
boolean, like so:

```Futhark
-- Reversing twice is identity.
entry prop_rev_rev (xs: []i32) = and (map2 (==) (reverse (reverse xs)) xs)
```

It is not too difficult to conceive of a *test driver* program that communicates
with the compiled Futhark program. The test driver randomly generates arrays of
type `[]i32` of various sizes and contents, passes them to `prop_rev_rev`, and
checks whether the result is `false`. But a program can have many entry points,
and not all of them may be properties. How are we to know which of them we
should treat as such? Some options:

1. Every entry point of type `t -> bool`.

2. A list of property names passed in on the command line.

3. Every entry point whose name begins with `prop_`.

4. Marking each property with an
   [attribute](2020-06-28-attributes-in-futhark.html), e.g:

   ```Futhark
   #[prop]
   entry prop_rev_rev = ...
   ```

5. A magical comment somewhere in the file indicating properties.

I dislike option 1 because it is too implicit, and option 2 because I like
Futhark program files to be self-containing and do the right thing by default.
Options 3 and 4 are vulnerable to typos - if we misspell as `pro_` the property
will just be silently ignored.

Further, options 3 and 4 also have the problem that it is tricky to determine
which properties are available in an "offline" manner. By this I mean that we
actually have to parse the program and dig through the AST to find out which
entry points they provide, and which attributes they might have. This requires
the test driver to know about compiler internals. The information can be
extracted easily from the compiled program over the server protocol, but this
requires compiling the program before we know which properties it provides. I
want the ability to quickly tally how many properties are to be tested, so we
can print a progress bar or similar. This is impractical if we have to compile
all programs first.

Option 5 is similar to how the `futhark test` tool currently works. This is how
you currently define a unit test in Futhark:

```Futhark
-- ==
-- entry: test_rev
-- input { [1,2,3] } output { [3,2,1] }

entry test_rev (xs: []i32) = reverse xs
```

The magical comment beginning with `==` is called a "test spec" in the internal
lingo, and specifies which entry point(s) to test, which input to provide, and
which output to expect. The test specs can be parsed much faster than the full
Futhark programs, and is completely disentangled from how programs are
internally represented. The compiler as such does not understand these test
specs. The approach is still somewhat vulnerable to human error, as I can easily
imagine defining an entry point for a property, but forgetting to put in the
magic comment. This does not happen so much with unit tests, as manually writing
down the test cases is a major part of the process.

So we have three viable options - 3, 4, and 5, each with pros and cons. However,
there is an additional wrinkle. In practice, writing properties is not enough -
we also must make it possible for the user to specify *input generators*. For
example, some property may only be defined for sorted lists, or maybe it accepts
an abstract type that the test driver cannot generate automatically. Therefore,
each property must optionally be coupled with a generator, and perhaps also a
*shrinker* (for making failing inputs smaller), and a *prettyprinter* (for
printing the input in a human-readable manner).

Option 3 becomes awkward under these extended constraints. We can perhaps say
that a property `prop_foo` has a generator named `gen_foo`, a shrinker named
`shrink_foo`, etc. But in many cases we may want to use the same generator (say,
one for sorted arrays of integers) for multiple properties. we can of course do
that with simple function definitions (`entry gen_foo = generate_sorted_array`),
but I must admit I am uncomfortable with the notion of magic names.

Option 4 handles this easily enough, simply by adding the information to the
property. For example:

```Futhark
#[prop(gen_foo, shrink_foo, pp_foo)]
entry some_property = ...
```

Similarly, since option 5 also comes with some bespoke notation, it is also easy
to just jam in whatever information we wish. For example:

```Futhark
-- ==
-- property some_property { generator=gen_foo, shrinker=shrink_foo, printer=pp_foo }

entry some_property = ...
```

(I did not put much thought into this specific syntax, it is intended solely as
demonstration.)

The largest problem with option 5 is that it is essentially *another language*
(albeit a tiny one) that users have to learn, written inside comments. While it
is *fine*, I don't think anyone particularly likes it, and users are sometimes
confused by its syntax and behaviour.

We are left with three viable options. All of these will work technically, but
have implications for the user experience. Which option is preferable may be up
to personal judgement. There is also the possibility of mixing them - for
example, you could assume all entry points named `prop_*` are properties, but
use attributes for connecting them to generators, shrinkers, and prettyprinters.
The current prototype implements option 4, but I am personally leaning towards
option 5.

Of course, I should note that I am not writing this post about surface syntax
because it is the most interesting part of property based testing. There are
other questions still to answer, such as how to actually write generators or
shrinkers (do we need combinator libraries?), but these have much clearer
technical properties that make it easier to decide whether a solution is good or
not.
