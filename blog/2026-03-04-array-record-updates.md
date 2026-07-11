---
title: Addressing a type system limitation with syntactic sugar
description: A discussion of mixed record/array updates, which turns out to address a problem that would otherwise require a rather complicated extension of the type system.
---

Despite Futhark being a data parallel language and explicitly [only trying to
innovate in that area](2016-09-03-language-design.html), the most complicated
and unusual language feature in Futhark is actually related to making it more
efficient to write sequential code.

Specifically, in Futhark you can write `a with [i] = x` to produce the array `a`
where the element at index `i` has been replaced by `x`. *Semantically* this is
the same as a copy, but *operationally* Futhark will reuse the memory used to
store `a`, and perform a destructive update, like in an imperative language.
This means that the cost of the operation is constant, no matter how big `a`
might be. In order for this to be safe in a pure language, the compiler will
mark `a` as *consumed*, and flag an error if you try to use it again. Most of
the complexity is related to tracking consumption across function calls and
aliases, but the description above suffices for this post ([more details here
for the curious](2022-06-13-uniqueness-types.html). We call this construct an
*array update*, although semantically it returns a copy, rather than updating
anything.

*Record updates* are syntactically a very similar feature: we write `r with f =
x` to return a copy of the [record](2017-03-06-futhark-record-system.html) `r`
where the value of the field `f` has been replaced with `x`. In contrast to
array updates, record updates have no funny business going on: an update is
exactly the same as constructing a new record where one of the fields differs
from the original (although the [value
representation](https://futhark.readthedocs.io/en/latest/performance.html#value-representation)
means that the performance is still pretty good).

Both of these features are pretty simple (well, as simple as substructural type
systems go), but they interact in an unfortunate way. Specifically, suppose
we have the following record type:

```Futhark
type record = {a: [100]i32, b: [100]i32}
```

Assume now that we have a variable `x` of type `record` and we want to perform
an array update on the array in the `a` field. We can write this just fine:

```Futhark
x.a with [0] = 1337
```

This gives us back an array though, and we actually want a record with a changed
field. We try to write this, where we use both an array update and a record
update expression (parentheses added for clarity):

```Futhark
x with a = (x.a with [0] = 1337)
```

But now we get an error: the type checker complains that `x` has been consumed
and yet we are trying to use it. The problem is that the array update `x.a with
[0] = 1337` must be fully evaluated before we can do the record update, and by
our type rules, the array update consumes *all of `x`*. The reason is that
consumption is tracked at the granularity of *variable names*: we mark an entire
variable (and it aliases) as consumed, but we cannot mark only *part* of a
variable consumed. Specifically, the type checker cannot represent that the
field `a` of `x` is consumed, but that the other fields are fine. Instead, it
marks all of `x` as consumed, which is then a problem when we try get to the
record update `x with a = ...`.

This is not a quirk of writing it out as a single compound expression - the
following explicit form has the same problem, and is perhaps even clearer:

```Futhark
let tmp = x.a with [0] = 1337 -- Now 'x' is consumed.
in x with a = tmp             -- May not use 'x'!
```

[The workaround](https://github.com/diku-dk/futhark/issues/1560) is to
explicitly decompose the record into its component fields and then reconstruct
it:

```Futhark
let a = x.a
let b = x.b
in { a = a with [0] = 1337,
     b = b }
```

However, this is syntactically very awkward when the record is large. This is
not merely a theoretical issue - one of my colleagues was supervising a project
about porting an existing HPC application to Futhark, which involved working
with a record with dozens of fields. The explicit construction was incredibly
verbose and painful, but extending our semantic model for tracking consumption
was also not very palatable. This was already one of the weirdest parts of the
Futhark language, and many users are confused by consumption-related errors -
making the rules even more complicated seemed like a bad use of our [novelty
budget](http://shimweasel.com/2018/08/25/novelty-budgets).

Fortunately, as an academic I have access to a [supply of free
labour](2024-02-03-quantifying-student-projects.html). Over the past weeks, the
student [Aziz Rmadi](https://azizrmadi.com/) has added a new language construct
to Futhark that is essentially just syntactic sugar, but turns out to nicely
address the specific pain point described above.

Concretely, array updates and record updates have been unified into a *single*
construct that allows chaining updates to record fields and array elements. The
operation above can now be written simply as follows:

```Futhark
x with a[0] = 1337
```

From a semantic point of view, this is no different from using multiple updates
chained together. But from the perspective of consumption checking, it is a
great advantage that the update is now a *single* expression, rather than
multiple compound ones. It means that reading the old field and "writing" the
new field is now a single atomic operation. All of `x` is still marked as
consumed, as it must be, but *after* we construct the new record, with the
updated field.

In most cases, syntactic sugar really is just *syntactic*: you can rewrite it
yourself, and the program will still work. Sometimes that is also how it is
implemented. For this particular bit of sugar however, the rewrite must wait
until after type checking. This means we need to track this new construct
throughout the compiler frontend, but as it replaces two previous constructs
(record and array updates), that is still a net win as far as I am concerned.
I'm quite pleased at how well this feature came together, and that we have [for
now](2026-02-23-accumulators.html) been able to put off adding further
complications to the type system.
