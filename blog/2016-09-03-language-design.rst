---
title: Designing a Functional Language for GPU Execution
author: Troels Henriksen
description: Why Futhark has so few features.
---

The Futhark programming language is high-level, data-parallel and
hardware-agnostic.  It is intended to be used both for
hand-programming and as a target for code generation.  To demonstrate
that functional programming can deliver high parallel performance, we
have implemented an optimising compiler that generates code for GPU
execution.  We spend most of our time working on how such a compiler
should be constructed, and which functional invariants we can exploit
to perform optimisation.  Less time is spent designing the Futhark
source language itself.  This stands to reason: there are not many
compilers for functional language that can generate efficient GPU
code, but there has been a lot of work on designing convenient and
elegant functional languages.  Ideally, we want to innovate on the
former, and copy from the latter.

Unfortunately, we cannot merely copy-paste a `Standard ML`_ parser
into our compiler frontend and be done with it.  GPUs are a rather
hostile environment, and such sophisticated facilities as memory
allocation, function pointers, recursion, and branching are
unavailable or inefficient.  It might be possible to compile a
conventional functional language to GPUs, but performance would likely
not be good (`Harlan`_ makes a brave effort, however).  The market for
nice expressive moderately-fast functional languages is already pretty
crowded, so Futhark targets a different market: a smaller language
with fewer features, but where most language constructs are (or *can
be*) executed efficiently.  Put simply, *Futhark is not worth using
unless it is fast*.  This inspires two language design constraints:

+ **Include only features that can be executed efficiently in
  parallel**.  GPUs serve as our yardstick for uncooperative hardware,
  but they are not the only parallel systems that we wish to support.
  The main rule of thumb is that we must compile most abstraction
  away, because we cannot just represent everything (or anything!) as
  a function pointer on the GPU.  At least not if we want the code to
  run fast enough to be worthwhile.

+ **The language must be small**.  Not just to keep compiler
  complexity in hand, but also because *Futhark cannot be used to
  write full applications*.  To anyone writing code in Futhark, it is
  likely a secondary language that is used only for the
  performance-intensive parts of a larger application (`how this works
  </blog/2016-04-15-futhark-and-pyopencl.html>`_).  The language
  should not have complicated features that are found nowhere else.
  It also means Futhark cannot make use of sophisticated type
  machinery, although there are places where `dependent types`_ would
  fit real well...

For reasons of time, we are mostly hacking on compiler optimisations,
not doing language design.  This means that Futhark has grown rather
slowly, but it *has* changed, at least superficially.  As an example,
here's how we would once have written a matrix-matrix multiplication::

  let [[i32]] main([[i32]] x, [[i32]] y) =
    map(\[i32] ([i32] xr) ->
          map(\i32 ([i32] yc) ->
                reduce(+, 0, zipWith(*, xr, yc)),
              transpose(y)),
        x)

There is a Haskell-like notation for array types, and C-style type
indications, where the parameter type precedes the parameter name.
The parallel constructs (``map``, ``reduce``, ``transpose``) require
parentheses around their comma-separated arguments, just like any
other function call - again, like C.

Here is what it looks like now::

  let main(x: [n][m]i32, y: [m][p]i32): [n][p]i32 =
    map (\xr ->
           map (\yc -> reduce (+) 0 (zipWith (*) xr yc))
               (transpose y))
         x

Type annotations are now optional except for top-level functions,
function application is by juxtaposition, and the array type syntax is
different.  We also support shape declarations that express how the
types of inputs must relate to each other and to the result.  This is
not proper dependent typing - again, we keep it simple - but is
asserted through run-time checks, and used by the compiler to perform
optimisations.

We also used to not have pattern matching for function parameters, or
type aliases.  To wit, observe the initial version of a vector
addition function from an `N-body benchmark`_::

  let {f32, f32, f32} vec_add({f32, f32, f32} v1, {f32, f32, f32} v2) =
    let {x1, y1, z1} = v1
    let {x2, y2, z2} = v2
    in {x1 + x2, y1 + y2, z1 + z2}

We also used curly braces for tuples in those days.  Now the function
looks like this::

  let vec_add((x1, y1, z1): vec3) ((x2, y2, z2): vec3): vec3 =
    (x1 + x2, y1 + y2, z1 + z2)

.. _`N-body benchmark`: https://github.com/HIPERFIT/futhark-benchmarks/blob/master/accelerate/nbody/nbody.fut

Essentially, we are moving towards an SML/Haskell/F# *feel* for
Futhark, but with the restrictions that result from our demand for
performance and our uncooperative target platform.  Concretely, as
time permits, we plan to extend Futhark with:

1. **An SML-style module system**.  We already have a simple
   implementation of structures and the beginning of signature
   support.  All that is needed is to finish signatures and add
   functors.

2. **Some form of statically resolvable ad-hoc polymorphism**.  I'm
   thinking modular type classes, which are also being `considered
   for Successor ML
   <https://github.com/SMLFamily/Successor-ML/issues/18>`_.

3. **Proper type inference, even for top-level functions**.  This is
   hopefully easy to implement, although our `uniqueness types
   <http://futhark.readthedocs.io/en/latest/language-overview.html#uniqueness-types>`_
   may make things interesting.

These features would enable a style of modular programming that can
still be compiled efficiently, and would not be too hard to implement.
We can then move on to more advanced features, like:

4. **True higher-order functions (or a convincing imitation)**.  These are
   typically implemented with function pointers, which are tricky
   (slow/impossible) on GPUs.  Perhaps a solution could be a type
   system feature that ensures that higher-order values correspond
   to a (statically known) lambda term, but which might have any
   lexical closure.  For example, this would be permitted::

     let makeAdder x = \y -> y + x

   because the caller of ``makeAdder x`` would always know the
   "form" of the function being returned.  Meanwhile, this would
   not::

     let adderOrSubber b x = if b then (\y -> y - x) else (\z -> y + x)

   because the form of the returned function depends on a dynamic
   decision.  For this particular function, a workaround could be::

     let adderOrSubber b x = \y -> y + if b then -x else x

   Note that the function composition operator obeys this rule::

     let compose f g = \x -> f (g x)

   Except that's a function, not an operator, which reminds me...

5. **User-defined operators**.  I like how Haskell has made infix
   operators lexically distinct, and I also like how F# defines
   operator priority and associativity based on its constituent
   characters.

Of course, it's also worth discussing smaller changes, such as whether
our notation for anonymous functions is too verbose (it is), and
whether we should have syntax for common cases like ranges and array
comprehensions (probably).  If you like working on language design or
making things go fast, why not `contribute`_?  The `Futhark compiler
frontend`_, which processes source programs and converts them into the
core language, is not big and fairly easy to understand.

.. _`Standard ML`: https://en.wikipedia.org/wiki/Standard_ML
.. _`Harlan`: https://github.com/eholk/harlan
.. _`dependent types`: https://en.wikipedia.org/wiki/Dependent_type
.. _`contribute`: /getinvolved.html
.. _`Futhark compiler frontend`: https://github.com/HIPERFIT/futhark/tree/master/src/Language
