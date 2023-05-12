---
title: Static and dynamic challenges of size types
description: We're growing the Futhark type system yet again, and this is why it's difficult.
---

One of Futhark's interesting features is its size type system, by
which we can express pre- and post-conditions on the shapes of arrays
accepted and returned by functions.  For example, a function for
matrix multiplication might have the following type:

```Futhark
val matmul [n][m][p] : [n][m]f32 -> [m][p]f32 -> [n][p]f32
```

The `[n][m][p]` before the colon are *size parameters*, similar to
type parameters.  They indicate that the function is polymorphic in
these sizes.  When the function is invoked, the programmer does not
directly provide arguments for these.  Instead the type checker
instantiates the size parameters based on the shapes of the explicit
value arguments, and issues a type error if this leads to an ill-typed
application - which it will if the matrices are not of compatible
shapes.  This is a rather handy feature for avoiding shape mismatches
at runtime, which is often quite prevalent in array programs.

Size types were part of the [early
plans](2021-12-19-past-and-present.html) for Futhark, but did not
become part of the language [until
2020](2020-03-15-futhark-0.15.1-released.html) - turns out a pandemic
leaves you with a lot of time to hack on type systems.  The system as
first implemented had one major restriction: sizes had to be constants
or variables, rather than arbitrary expressions.  This meant that the
system could not represent the "natural" type scheme of `concat`:

```Futhark
val concat [n][m] 't : [n]t -> [m]t -> [n+m]t
```

Instead, we were forced to assign it a less precise *existential
size*:


```Futhark
val concat [n][m] 't : [n]t -> [m]t -> ?[k].[k]t
```

As the name implies, this means that the function will return an array
of *some size*, but that size cannot be known until the function
returns, whereupon you can inspect the actual array value it produces.
Such existential sizes are a necessary feature for functions such as
`filter`, where the size of the result is truly unpredictable, but for
`concat` it is unfortunate.  As an example of why this is not ideal,
consider a function such as `zip`, that expects two arrays of the same
size:

```Futhark
val zip [n] 'a 'b : [n]a -> [n]b -> [n](a,b)
```

Now suppose we have arrays `A: [n]t`, `B: [n]t`, `C: [n]t`.  An
expression `zip (concat A B) (concat A C)` will be ill-typed, because
each invocation of `concat` will return an array with a distinct size.

As another example, consider the function `iota`:

```Futhark
val iota : (n: i64) -> [n]i64
```

This has a *size-dependent type*, because the parameter `n` is used in
the return type.  Thus, an expression `iota x` has type `[x]i64`.  But
since we can put only variables or constants in sizes, what do we do
about an expression `iota (x+y)`?  Our solution has been to treat the
return size as existential, as in `concat` above, whenever it would
otherwise be inexpressible.

Fortunately, this state of affairs is about to change.  [Lubin
Bailly](https://github.com/catvayor) of
[ENS](https://www.ens.psl.eu/en) is interning at [our
department](https://diku.dk) and has extended the Futhark type system
to support (almost) arbitrary expressions in sizes.  This seems like a
good opportunity to write a blog post about some of the more subtle
details of the type system - which is what you're reading right now.

## Unknown sizes

Futhark's size types can be seen as a restricted subset of a proper
dependent type system found in languages such as
[Idris](https://www.idris-lang.org/) or
[Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php). As Futhark is
intended to be accessible to programmers who are not type system
experts, it contains restrictions - and some conveniences - that make
its use and implementation more tractable.  I will be making
comparisons with Idris, as it is useful to contrast Futhark's somewhat
idiosyncratic and specialised approach to more traditional and general
designs.

One of the key concepts is the notion of an *unknown size*, which is
used to handle existential types, which occur in the type scheme for
`filter`:

```Futhark
val filter [n] 't : (t -> bool) -> [n]t -> ?[k].[k]t
```

An expression `filter p xs` will have type `[m]t` where `m` is a fresh
unknown size made up by the type checker for the occasion, and
replacing with the existential `k` in the type scheme.  We know
nothing about this `m`, and we treat it as statically distinct from
all other sizes.  In comparison, the similar function for vectors in
Idris returns a *dependent pair*: a value that contains a size `p`
and a vector of just `p` elements:

```Idris
filter : (elem -> Bool)
      -> Vect len elem
      -> (p : Nat ** Vect p elem)
```

In Idris, you cannot pass a ``(p : Nat ** Vect p elem)`` to a function
that expects a `Vect p elem`, as the types are different.  This is
ill-typed in Idris:

```Idris
length (filter p xs)
```

Instead, in Idris you have to unpack the pair, bringing the size and
the vector separately into scope:

```Idris
let p ** v = filter p xs
in length v
```

Futhark's approach can be seen as doing this automatically - packing
and unpacking dependent pairs as necessary.  The well-typed Futhark
expression `length (filter p xs)` can be seen as syntactic sugar for

```Futhark
let [k] (v: [k]t) = filter p xs
in length v
```

which shows Futhark's notation for explicit size binding.  We'll
return to that in a bit.

But first, I want to return to the type of `filter`.  I mentioned that
we cannot know the size of the array produced by `filter`.  Strictly
speaking, that is not true.  Suppose we have a function that counts
how many elements of an array satisfy some property:

```Futhark
val count [n] 'a :
  (p: a->bool) -> (as: [n]a) -> i64
  ```

Exploiting the new ability to put arbitrary expressions in sizes, we
can assign `filter` this non-existential type scheme:

```Futhark
val filter [n] 'a :
  (p: a->bool) -> (as: [n]a) -> [count p as]a
```

And indeed, there is nothing that prevents you from doing so in
Futhark.  In general, for *any* function that returns an existential
size, you can *slice* that function into one that returns just the
sizes (as `count` does for `filter`), and use that in the type of the
original function.  This can be used to completely eliminate
existential quantification in function return sizes.  In fact, this
approach was used by Barry Jay in his [FISh array programming
language](https://link.springer.com/article/10.1007/s100090050037).
The downside is that such a *size slice* can be just as expensive as
the original function.  Much of the research on FISh focused on
reasoning about when such slices are cheap, but this would exceed
Futhark's novelty budget.  In our example here, the `count` function
is almost as expensive to compute as `filter`, and assigning `filter`
the existential-free type listed above will likely require us to
evaluate `count` every time we apply `filter`.  This is not
appropriate for a language whose main purpose is high performance
execution.  Still, Futhark is flexible enough that one can define
existential-free functions, if you prefer that style.

Two remarks before we continue:

1. `filter p xs` will of course have to compute something equivalent
   to `count p xs` internally, but it will be done in a different way
   (with a `scan` instead of a `reduce`), and it would take an
   impractical level of compiler smarts to realise that `count p xs`
   is equivalent to *part of an intermediate result in the definition*
   of `filter`.

2. It's a bit of an open question whether putting a fancy expression
   in a type requires us to actually *evaluate* that expression.  In
   our current compilation strategy we always do that, but we don't
   have a clear idea of exactly when it is strictly necessary.  For
   now we encourage a programming style where size expressions should
   not be very costly - in fact, try to keep them constant time.

## Dynamic challenges

Futhark's size type system is not just a mechanism for imposing
constraints on function types - it is also a mechanism for *accessing*
the dynamic size of arrays.  When we bind a size parameter, we also
bring that size into scope as a variable [of type
`i64`](2023-03-20-why-are-sizes-signed.html).  In fact, this is the
canonical definition of `length`:

```Futhark
def length [n] 't (_: [n]t) = n
```

The `length` function ignores the actual argument value and just
returns its size.

This is of course quite ordinary by dependent type standards - since
types are values, we can extract values from them at runtime.  It is
also one of the great challenges of implementing dependently typed
languages efficiently.  If types can influence computation, does that
then mean we have to carry around arbitrarily complex types at
runtime?  [Idris uses quantitative type
theory](https://idris2.readthedocs.io/en/latest/tutorial/multiplicities.html)
to allow direct reasoning about which types may be accessed at
runtime, and what the compiler is allowed to erase.

In Futhark, the type system ensures that the only values you can
extract from types are those that correspond directly to sizes of
arrays, which you need to store at runtime in any case.  Intuitively,
Futhark's size types are meant to have a simple operational
interpretation: it can be seen as syntactic sugar for extracting
information from values that use conventional value representations.

This *functional Fortran* line of thinking means that functions with
types such as `t -> ?[n].([n]t -> t)` are not meaningful.  Consider
what this function is trying to say: it takes an argument of some type
`t`, then returns a function of type `[n]t -> t` for some fresh
unknown size `n`.  But how are you supposed to find the actual runtime
value for `n` so you can construct an argument of appropriate size?
The only thing that knows about `n` is the function of type `[n]t ->
t`, and functions are represented as *black boxes* - the only way to
get information of them is to apply them.  In particular, size
bindings such as

```Futhark
let [n] (f: [n]t -> t) = ...
```

where we try to bind a size only known from a function value are not
allowed.

This is a true loss in expressive power.  In Idris, you could explicitly
return a dependent pair containing the size and the function, but
Futhark does not support dependent pairs directly.  Instead, the
workaround is to also return a *witness* for the unknown size, in the
form of an array that has just that size along one of its dimensions:

```
t -> ?[n].([n](), [n]t -> t)
```

An array `[n]()` of unit elements requires no space at runtime, but
does carry around its size, and so allows us to extract the dynamic
value of the `n`.  I wrote a blog post about an [interesting use of
this
technique](2021-10-16-explicit-existentials.html#one-somewhat-interesting-usage-of-explicit-existential-quantification).

So that is the guiding principle of size parameters: they must
actually be used as an array size, outside of function parameter or
return types.  We call this a *constructive* use, because the size is
actually used as the size of a real concrete array value, from which
its dynamic value can be extracted at runtime.  However, at some point
we realised that this is actually a bit more conservative than
necessary.  It is in fact sound for size polymorphic functions to have
size parameters that are *not* used constructively, or even in any
parameters at all.  Consider a definition

```Futhark
def iiota [n]: [n]i64 = 0..1..<n
```

which is an "implicit `iota`" taking no arguments, where the context
in which it is used is used to instantiate the size parameter `[n]`.
In some sense, it is the responsibility of the caller to disambiguate
what the size should be.  For example, this is well-typed:

```Futhark
zip (replicate x z) iiota : [x](i32,i64)
```

We can imagine that the type checker rewrites size parameters to be
explicit parameters, and then figures out an appropriate argument for
each distinct use, through expression unification.  That is actually
just how it is implemented in the compiler.  When a unique
instantiation cannot be determined, the type checker will complain
that the size is ambiguous.

### Empty arrays

Most of the rules and semantics of size types are completely
straightforward, and that is by design.  The dynamic semantics of size
parameters correspond to calling `length` on arrays in scope.  Well,
almost.  There is a significant wrinkle in that we can also use size
parameters to extract the size of an *inner* dimension:

```Futhark
def cols [n][m] (xss: [n][m]i64) = m
```

When we apply `cols` to some array that is empty (`n=0`), we should
still retrieve the correct inner size, which might still be nonzero:

```Futhark
cols (replicate 0 (replicate x 0)) == x
```

This means that although Futhark exposes a programming model based on
"arrays of arrays", the *actual* representation must be more like APL,
where an array stores its full dynamic shape at runtime, independent
of its elements, as a "shape vector".  APL has done this since the 60s
and so its implementation is hardly science fiction.  Unfortunately,
this approach has a thorny interaction with parametric polymorphism.
Consider `map`:

```Futhark
def map [n] 'a 'b (f: a -> b) (as: [n]a) : [n]b = ...
```

Now suppose we use `map` as follows:

```Futhark
map (\x -> replicate m x) xs
```

If `xs` has type `[n]t` for some `t`, then the entire expression has
type `[n][m]t`.  But consider the case where `n=0`, meaning we are
`map`ing on an empty array `xs`.  How is `map` supposed to construct
the shape vector `(n,m)`?  The `map` function itself has no idea what
the provided function `f` does.  In particular, `map` does not know that
it is constructing a multi-dimensional array.  Further, since the
input array is empty, we cannot even apply `f` once to see what kind
of value it is returning.  And since functions are black box, we
cannot query it and extract its type!

To handle this, we took another bit of inspiration from dependent type
theory.  At runtime, the type parameter `a` is bound to the *actual
type* it has been instantiated with, *including evaluating all sizes*,
which in the above example means a binding `b=[x]t`, where `x` is the
actual integer value that `m` is bound to.

This is quite unusual for a language that otherwise tries to stick
close to Standard ML ideals such as type erasure, but at least this
quirk is not visible in the surface semantics.  Also, the way the
compiler actually implements polymorphism - with
[monomorphisation](https://en.wikipedia.org/wiki/Monomorphization) -
simply turns the necessary sizes into explicit parameters.  Only the
interpreter needs to perform actual dynamic type passing and
evaluation.

## Supporting arbitrary size expressions

The idea is simple: instead of a size being a variable or constant, it
can be any expression of type `i64`.  This has required a lot of
implementation work in the type checker and compilation pipeline,
impressively carried out by Lubin Bailly despite his initial
unfamiliarity with the compiler code.  However, the things I want to
discuss are language level aspects.

As we hoped, the new type scheme for `concat` is:

```Futhark
val concat [n][m] 't : [n]t -> [m]t -> [n+m]t
```

When we type check an expression

```Futhark
concat A B
```

where `A: [e1]t`, `B: [e2]t`, we simply substitute `n⟼e1`, `m⟼e2` in
the return type, giving `[e1+e2]t`, no matter how complex `e1` and
`e2` might be.  I always have a good feeling about language mechanics
that ultimately boil down to substitution.

Similarly, an expression `iota (x+y)` is assigned the type `[x+y]i64`
by simply substituting the argument into the return type.

Some questions arise.  First, are all expressions truly valid as
sizes?  As a starting point, all of them as long as they are
well-typed and of type `i64`.  However, regular readers of this
column, or members of that prestigious elite who actually use Futhark,
may recall that the language has a somewhat exotic feature we usually
call [uniqueness types](2022-06-13-uniqueness-types.html).  I will not
go through their entire design here, but simply note that they allow
expressions to "consume" variables in scope, rendering them
inaccessible afterwards.  Effectively, such expressions can be
evaluated at most once.  It is not clear what it should mean for such
an expression to appear in a type, so they are banned when evaluating
type expressions.  Should they occur due to substitution, for example
in an expression like `iota (consume A)`, the expression will be
replaced with an unknown size and the compiler issue a warning.  We
don't expect this to be very common in real code.

### Variables going out of scope

Another interesting question is what to do when a variable used in a
type goes out of scope:

```Futhark
let n = e
in iota (n+1)
```

We cannot assign this expression the type `[n+1]i64`, because `n` is
no longer in scope.  There are a few options.  One is substitution:
just replace `n` with `e`, yielding an overall type of `[e+1]i64`.
That is fine here, but does not work when name is bound by a `case`
pattern or is a function parameter, because there will be no
corresponding expression.  (Real dependently typed languages can
produce impressively incomprehensible type error in these cases.)

Another option is to create a fresh unknown size `k` and substitute it
for the name going out of scope.  The expression would then have type
`[k+1]i64` for some unknown size `k`.  Unfortunately this does not
work when the variable going out of scope is not of type `i64`.  For
example, it might be a function:

```Futhark
let f = ...
in iota (f x)
```

We allow *unknown sizes* but not arbitrary *unknown values*, because
sizes are all we can actually extract from the array value
representation.  This is a core part of guaranteeing efficient
compilation, as it means we never have to carry around additional
metadata at runtime, beyond the actual sizes of the arrays - which is
a single integer per dimension.

The final option is to turn the entire size expression into an unknown
size, and thus assign the expression the type `[k]i64`.  This is
always viable, but loses information about the structure of the size.

Which of these options to use, and when, is something we are still
working on.  We will probably end up with a strategy where we take the
option that loses as little information as possible, but it's an open
question whether this will feel unpredictable and magical to the
programmer.

### Unification

Finally, let us consider unification of sizes.  Currently when type
checking, we use a purely syntactical form of unification, meaning
that if we have expressions of types

```Futhark
concat A B : [n+m]t
concat B a : [m+n]t
```

then they are considered to have different sizes, as `n+m` and `m+n`
are not syntactically identical.  To put it bluntly, the Futhark type
checker cannot do arithmetic, and does not understand that addition
commutes.  This is certainly something we need to address, but we must
be careful, as we can write a function such as this:

```Futhark
def tricky [n][m] 't (A: [n+m]t) = (n,m)
```

When applying this function and instantiating `n` and `m`, the order
of operands to the addition really does matter to the result!
Suddenly addition does not look so commutative anymore, because we can
directly observe the *structure of the computation*, not just the
result.  If we allow the type checker to do arithmetic, what should
this return?

```Futhark
tricky (concat (iota n) (iota m)) (concat (iota m) (iota n))
```

Truly the [full employment
theorem](https://en.wikipedia.org/wiki/Full-employment_theorem) can
also be justified by the endless edge cases that arise whenever you
think you've come up with a helpful new type system feature.
