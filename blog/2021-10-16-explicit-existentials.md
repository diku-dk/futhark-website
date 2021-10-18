---
title: Explicit quantification of existential sizes
author: Troels Henriksen
description: The type system is growing again.
---

When we first designed Futhark, array sizes were not part of the type
system.  A function such as ``map`` had this type:

```
val map 'a 'b : (a -> b) -> []a -> []b
```

(In the very earliest days, `map` was syntax and not a function, but
let's skip past those truly primordial times.)

As array programming often requires maintaining various size
invariants, we eventually added support for [size
types](2019-08-03-towards-size-types.html), which lets functions
express how the size of their result depends on the size of their
parameters:

```
val map 'a 'b [n] : (a -> b) -> [n]a -> [n]b

val transpose 'a [n] [m] : [n][m]a -> [m][n]a
```

This is useful, but it does raise the question of how to express
functions that return arrays whose sizes cannot be straightforwardly
known from the sizes of the arguments.  An example of such a function
is `filter`:

```
val filter 'a [n] : (a -> bool) -> [n]a -> []a
```

We don't know what to put inside the brackets in the return type,
since we have no way of knowing how many elements will satisfy the
property.  Our solution is to leave the size blank as above, which we
call an *anonymous size*.  Whenever `filter` is fully applied, as
`filter p xs`, the type checker will invent a new size, say `k`, and
assign the result the type `[k]a`.  We have no idea what this `k` will
be, but we know that it must eventually exist at runtime.  Inspired by
terms from predicate logic, `filter` has an *existential size*:

```
(α → bool) → [n]α → ∃k.[k]α
```

(Note the use of Unicode to indicate that this is not a Futhark type.)
Under this interpretation, there *exists some `k`* that is the size of
the array that `filter` returns.  In a dependently typed language,
`filter` would return a [dependent
pair](https://en.wikipedia.org/wiki/Dependent_type#%CE%A0_type).  For
example, this is the type of the `Data.Vect.filter` function in the
dependently typed language [Idris](https://idris-lang.org):

```
filter : (elem -> Bool) -> Vect len elem -> (p : Nat ** Vect p elem)
```

The return value is a pair of a number `p` and some `Vect` with `p`
elements.  So why did we not implement existential sizes in Futhark in
terms of dependent pairs?  The main reason is to keep the language
simple.  Consider an expression `length (filter p xs)`.  The `length`
function expects an array, *not* a dependent pair.  If `filter`
returned such a pair, the programmer would need to unpack it in some
manner, which is awkward.  Dependent pairs are a powerful programming
technique, but for Futhark's domain, we don't need that power.
Simplicity is more important.

That said, I am a PL academic, so of course Futhark's existential
sizes have an explanation in terms of dependent types.  Specifically,
the expression

```
length (filter p xs)
```

is really just syntactic sugar for first binding the result of
`filter`, extracting the size and the array, and then applying
`length`:

```
let [k] (ys: [k]a) = filter p xs
in length ys
```

In fact, the type checker will reject any program where this
transformation is not possible.  (This can happen for programs that
abuse type inference sufficiently, but the details are well outside
the scope of this post, and it probably will never occur in code
written by a sane person.)

For a while, this was the scheme: programmers would put in anonymous
sizes, which the type checker would interpret as existential
quantifiers, and compilation would happen using a strategy inspired by
dependent products.  In some cases this proved too restrictive.  For
example, since each anonymous size would be replaced by a distinct
existentially quantified size, there was no way to specify a function
that returns a matrix of unknown size, but which is known to be
square:

```
val square : f32 -> [][]f32
```

The above is interpreted as `f32 → ∃nm.[n][m]f32`.

We are also unable to express fancy higher-order types with complex
constraints, such as a function that returns both an array of
existential size, and a function that accepts arrays of precisely that
size:

```
val f : f32 -> ([]f32, []f32 -> bool)
```

Contrived?  Maybe, maybe not.  Later we'll see a function that
actually needs this.  The root problem is that the existential
quantifier is not directly accessible to the programmer, but inferred
by the type checker based on inflexible rules.

On a philosophical note, while I do (obviously) believe that type
inference is good, I think it is crucial that programmers can always
annotate their programs with the full and explicit types.  This is not
because I think fully elaborated programs are easy to read, but
because it means that type inference can always be *explained in terms
of the language itself*, rather than by reduction to some hidden
"core" language or logic.  Futhark's anonymous sizes violated this
principle, as they elaborated to some more expressive representation
private to the type checker.  Clearly we had erred.

So, as of recently, Futhark now allows explicit existential
quantification of sizes in function return types.  Because I prefer
ASCII syntax, the notation is with `?` instead of `∃`, and the
quantified names must be enclosed in brackets:

```
val filter [n] 'a : (a -> bool) -> [n]a -> ?[k].[k]a
```

Anonymous sizes are still allowed, and can now be explained as
syntactic sugar for inserting an existential quantifier at the
"nearest" enclosing function arrow.

Now we can give a type for the `square` function:

```
val square : f32 -> ?[k].[k][k]f32
```

And we can express complex relationships:

```
val f : f32 -> ?[k]([k]f32, [k]f32 -> bool)
```

Explicit programming with existential quantifiers will likely be rare.
My own motivation for supporting this was mostly the violation of my
principles for type inference.  I am however looking forward to seeing
which interesting new things we can express that we could not before.
I have already written one somewhat interesting usage of explicit
existential quantification.

## One somewhat interesting usage of explicit existential quantification

While Futhark is most obviously useful for number crunching with
matrices, part of the ambition is to be flexible enough to also
support more general data-parallel programming.  Let's consider
writing a Futhark function for splitting a string into words - in
parallel!

First, let us consider what the type the function should be.  In
Haskell, we might specify a function that takes a `String` and returns
a list of `String`s:

```
words :: String -> [String]
```

Unfortunately, Futhark does not support irregular arrays - each
element must have the same size.  So in Futhark, the straightforward
solution is to return an array of `(index,length)` pairs for
representing each word.  Also, Futhark does not have a character type,
so our strings will actually be byte strings:

```
type char = u8
val words [n] : [n]char -> ?[k].[k](i64,i64)
```

This is fine, but... doesn't seem quite type-safe, does it?  There is
no connection between those pairs and the string, and we have to
manually slice the string to actually extract the words.  We could
accidentally slice the wrong string, perhaps resulting in
out-of-bounds accesses!  So let us add a bit more type safety:

```
type word [n] = ([n](), i64, i64)
val words [n] : [n]char -> ?[k].[k](word [n])
val get [n] : [n]char -> word [n] -> ?[m].[m]char
```

The parametric type `word [n]` represents a word in a size-`n` string.
In order to "remember" the size `n`, we are forced to carry around an
array of size `n` of unit values.  At runtime, such an array will take
no space beyond its size, but it's admittedly a bit clumsy.  The `get`
function actually extracts a `word [n]` from a string of size `n`.  As
long as we keep the definition of the `word` type abstract (which can
be done with the module system), this lets us avoid out-of-bounds
accesses.

But we can *still* make mistakes!  These types do not prevent us from
extracting "words" from a *different* string of the same size - and
that string might have word breaks in different positions!  To avoid
this, we can make `words` not just return an array, but also the `get` function:

```
type word [p] -- abstract, but similar definition to above
val words [n] : [n]char -> ?[p].(word [p] -> ?[m].[m]char,
                                 ?[k].[k](word [p]))
```

We are returning an *existential phantom size* `p` that does not
correspond to the size of anything in particular, but forces us to use
the returned function `word [p] -> ?[m].[m]char` to do anything with
the array of `word [p]` values.  That function will in practice be a
closure that uses the original string.

Finally we have achieved type safety!  It is somewhat intricate, but
not particularly subtle.  The existential quantification is easy to
follow.

Oh, and while it feels a bit gauche after all this talk of types, the
actual definition of the function can be seen
[here](../static/words.fut).
