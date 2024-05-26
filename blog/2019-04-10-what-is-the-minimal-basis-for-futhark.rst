---
title: What is the minimal basis for Futhark?
author: Troels Henriksen
description: An investigation into how much we can reduce the compiler intrinsics while still maintaining asymptotic guarantees.
---
**Updated 2020-10-09 to incorporate language changes.**

Futhark exposes data-parallel operations in the form of *functions*,
both higher- and first-order.  In some cases, these are simply
wrappers around magical intrinsic functions that get mapped to special
representations in the intermediate language.  For example, this is
the definition of ``reduce`` in the Futhark basis library:

.. code-block:: Futhark

   let reduce 'a (op: a -> a -> a) (ne: a) (as: []a): a =
     intrinsics.reduce (op, ne, as)

The reason is that the compiler has baked-in knowledge about the
semantics of ``intrinsics.reduce``: not just how to generate efficient
code for the target platform, but also various properties that help
optimisations such as `loop fusion
<https://en.wikipedia.org/wiki/Loop_fission_and_fusion#Fusion>`_.

Futhark has a bit more than a dozen such intrinsics, not counting
primitive scalar functions like ``sqrt``: ``flatten``, ``unflatten``,
``concat``, ``rotate``, ``transpose``, ``scatter``, ``zip``,
``unzip``, ``reduce_by_index``, ``map``, ``reduce``, ``scan``,
``partition``, ``stream_map``, and ``stream_red``. Intrinsics are
never added lightly, and we try to keep their number small.  However,
I have been thinking about which of these intrinsics are actually
*critical* for maintaining the asymptotic (big-O) time guarantees of
Futhark, and which merely help the compiler perform constant-order
optimisations.  To clarify, note that parallel languages have *two*
big-O measures for time, namely `work and span
<https://en.wikipedia.org/wiki/Analysis_of_parallel_algorithms>`_.
Intuitively, *work* is the conventional measure for the total amount
of operations performed, while *span* is the longest chain of
sequential dependencies.  On a machine with an infinite number of
processors, the span indicates how long it would take to run the
program.  The higher the span, the more sequential the program.  For
example, consider this non-intrinsic replacement for ``reduce``:

.. code-block:: Futhark

   let reduce 'a (op: a -> a -> a) (ne: a) (as: []a): a =
     loop x = ne for a in as do x `op` a

This function is completely sequential and therefore has work *O(n)*
and span *O(n)*.  In contrast, ``intrinsics.reduce`` has span
*O(log(n))*.  Therefore, while this replacement is semantically
correct, it is asymptotically less efficient.  In this post, I will
investigate how to rewrite most of Futhark's built-in array functions
using just language constructs (like ``loop`` and ``if``) and a small
set of primitives, while maintaining the asymptotic performance as
much as possible, and also trying to keep the actual run-time
performance good.

Starting Out
------------

``map`` itself is one of the foundational primitives that we will
build on.  It cannot be written in terms of any available simpler
function.  But let's see if we can write ``zip``, which turns two
arrays into a single array of pairs:

.. code-block:: Futhark

   let zip [n] 'a 'b (as: [n]a) (bs: [n]b): [n](a,b) =
      map (\i -> (as[i], bs[i])) (iota n)

Okay, this works, but what is that ``iota``?  This is the function for
producing index spaces, as ``iota n`` produces an array from 0 to
``n-1``.  Can `iota` be written without intrinsics?  In Futhark, yes,
but for an interesting reason:

.. code-block:: Futhark

   let iota (n: i64): [n]i64 =
     0..1..<n

Futhark has special syntax for ranges, but this feels a bit like
cheating - ``iota`` is the primitive, and this syntax is just sugar.
Can we express ``iota`` in any other way? Sort of. It is possible to
express ``iota`` as a `prefix sum
<https://en.wikipedia.org/wiki/Prefix_sum>`_ on a replicated array
(with a ``map`` to adjust it so the array begins at zero):

.. code-block:: Futhark

   let iota (n: i64): [n]i64 =
     map (\x -> x - 1) (scan (+) 0 (replicate n 1))

In some parallel languages (`NESL
<http://www.cs.cmu.edu/~scandal/nesl.html>`_), the prefix sum ``scan
(+) 0`` is a builtin primitive.  However, for our purposes I'd prefer
to express it in terms of primitives, and as we will see later, this
requires ``iota``.  Catch-22.  Further, ``replicate`` would now have
to be a primitive, and it is not all that much simpler than ``iota``.
So, we add ``iota`` to our minimal base of assumed intrinsics and move
on.

Now that we have ``map`` and ``zip``, we can express all the other
`zip variants <https://futhark-lang.org/docs/doc/futlib/zip.html>`_ in
terms of them.  We can also express most of the `array utility
functions <https://futhark-lang.org/docs/doc/futlib/array.html>`_,
like ``replicate``:

.. code-block:: Futhark

   let replicate 'a (n: i64) (x: a): [n]a =
     map (\_ -> x) (iota n)

Or ``concat``:

.. code-block:: Futhark

   let concat 't (xs: []t) (ys: []t): []t =
     map (\i -> if i < length xs
                then xs[i]
                else ys[i - length xs])
         (iota (length xs + length ys))

Or ``rotate``:

.. code-block:: Futhark

   let rotate 't (r: i64) (xs: []t): []t =
     map (\i -> xs[(i+r) % length xs])
         (iota (length xs))

Here we are bending the rules a bit.  The builtin ``rotate`` is an
index transformation that does not have to copy its input - hence, it
is in principle *O(1)* work, while ours is *O(n)*.  However, any
non-pathological use will eventually require the rotated array to be
stored directly in memory (which is *O(n)*), or fused with a
subsequent *O(n)* operation.

The remaining functions ``transpose``, ``flatten``, and ``unflatten``
are similarly straightforward to define, so we now have a basic
parallel vocabulary.  Time to move on to more interesting functions.

Parallel reduction
------------------

The parallelism of a reduction is often demonstrated through *tree
reduction*, where pairs of neighbouring elements are successively
combined using the reduction operator.  This gives rise to a tree,
where each level halves the remaining number of elements, until only a
single one is left.  We can easily express this in Futhark, taking
some care to handle inputs whose size is not a power of two by
substituting the provided neutral element:

.. code-block:: Futhark

   let reduce_tree 'a (op: a -> a -> a) (ne: a) (as: []a): a =
     let as' = loop as while length as > 1 do
                 map (\i ->
                        let x = if i*2 >= length as
                                then ne
                                else as[i*2]
                        let y = if i*2+1 >= length as
                                then ne
                                else as[i*2+1]
                        in x `op` y)
                     (iota (length as `div_rounding_up` 2))
     in if length as' == 0 then ne else as'[0]

This works, but is it efficient?  It does *O(n)* work and has
*O(log(n))* span, so asymptotically it is fine.  Let us try
benchmarking it versus the default ``reduce`` for summing *n* integers
on an NVIDIA RTX 2080 Ti:

=====  ==========  =============== ==========
*n*    Builtin     ``reduce_tree`` Difference
-----  ----------  --------------- ----------
10³          22μs            119μs       5.4x
10⁴          22μs            386μs      17.5x
10⁵          26μs            151μs       5.8x
10⁶          31μs            649μs      20.9x
10⁷         123μs           1014μs       8.2x
10⁸         768μs           2491μs       3.4x
=====  ==========  =============== ==========

Not too bad - we're about a factor of three away from the builtin
``reduce`` for the largest *n*.  I can't really explain the strange
slowdowns for 10⁴ and 10⁶, but they showed up for every run.

The tree reduction does worst for small and large *n*.  For the
smaller *n*, we end up executing a lot of ``map``s on rather small
arrays, which will not fully saturate the GPU.  On the largest *n*,
the problem is that ``reduce_by_tree`` exploits *too much
parallelism*.  We don't need a hundred million threads to saturate
this GPU (a hundred thousand would be more than enough), but we still
pay the cost in the form of storing the intermediate arrays in memory.
Futhark's built-in ``reduce`` uses a fixed number of threads and
splits the input array between them, such that each thread
sequentially reduces an interval of the input.  Each thread then
contributes a single partial result, which are reduced in parallel.
We `wrote a paper
<https://futhark-lang.org/docs.html#design-and-gpgpu-performance-of-futharks-redomap-construct>`_
on the idea, but we can also try to express it in Futhark:

.. code-block:: Futhark

   let num_threads : i64 = 128 * 256

   let reduce [n] 'a (op: a -> a -> a) (ne: a) (as: []a): a =
     let chunk_size = n `div_rounding_up` num_threads
     let partial_results =
       map (\t -> loop x = ne for i < chunk_size do
                  let j = t + i * num_threads
                  in if j < n then x `op` as[j]
                     else x)
           (iota num_threads)
     in reduce_tree op ne partial_results

We fix the number of threads to some constant, and re-use the tree
reduction to handle the partial results.  The unusual index
calculation for ``j`` ensures a GPU-friendly memory access pattern
(specifically, `coalesced
<https://devblogs.nvidia.com/how-access-global-memory-efficiently-cuda-c-kernels/>`_).
If we do a naive slice for each thread instead, the function will
easily run four to five times slower.  In most cases, the Futhark
compiler is pretty good at rearranging array dimensions to ensure
efficient access, but here we are treating a one-dimensional array as
an irregular multi-dimensional array using complex index arithmetic,
and the compiler will not be able to understand what is going on.
Writing code like the above is deep hardware-specific voodoo, and not
something we expect Futhark programmers to have to do.

Our performance is quite decent:

=====  ==========  =============== ==========
*n*    Builtin          ``reduce`` Difference
-----  ----------  --------------- ----------
10³          22μs            135μs       6.1x
10⁴          22μs            137μs       6.2x
10⁵          26μs            144μs       5.5x
10⁶          31μs            142μs       4.5x
10⁷         123μs            235μs       1.9x
10⁸         768μs            875μs       1.1x
=====  ==========  =============== ==========

Note how our ``reduce`` is much slower than the builtin for small *n*,
but almost as fast for the largest workloads.  This is because we
always launch the full number of threads, even when there are not
enough elements in the input array to actually give every thread
something to do.  We can also see that run-time remains (almost)
constant until we get to *n=10⁷*; before that the run-time is almost
exclusively due to GPU overhead.  The builtin reduction is a little
smarter about dynamically picking the right number of threads based on
the hardware and workload, whereas our ``reduce`` uses a hard-coded
number.  Furthermore, the compiler uses low-level tricks to
efficiently combine the partial results, while we use the fairly naive
``reduce_tree``.

It is also worth mentioning that our ``reduce`` only works correctly
for operators that are `commutative
<https://en.wikipedia.org/wiki/Commutative_property>`_, while the
builtin requires only `associativity
<https://en.wikipedia.org/wiki/Associative_property>`_.  Ensuring
efficient memory access patterns for a non-commutative operator
requires a very different implementation strategy that I'm not sure
can be expressed nicely in a high-level way.  Our ``reduce_tree``
works fine for non-commutative operators, however.

Parallel scan
-------------

While reductions permit implementations that are both easily
understood and fairly efficient, scans are a different matter.  Of
course, Futhark's builtin ``scan`` is not particularly efficient as it
is, so maybe we stand a chance.  The algorithm we'll be using is a
simple but work-inefficient one first presented by Danny Hillis and
Guy Steele:

.. code-block:: Futhark

   let scan [n] 'a (op: a -> a -> a) (_ne: a) (as: [n]a): [n]a =
     let iters = i64.f32 (f32.ceil (f32.log2 (f32.i64 n)))
     in loop as for i < iters do
          map (\j -> if j < 2**i
                     then as[j]
                     else as[j] `op` as[j-2**i])
              (iota n)

(Note that this algorithm does not use the neutral element at all!)

This implementation is work-inefficient in that it requires *O(n
log(n))* operations, while a sequential ``scan`` only requires *O(n)*.
However, the span is *O(log(n))*, which is the best we can hope for.
Work-efficient parallel scan algorithms do exist, but they are more
complicated, and I'm not sure they can be expressed with the parallel
vocabulary we have developed so far (they need either recursion or
``scatter``).  Further, they might not even be faster in practice.
Most GPU scans (including the builtin one generated by the Futhark
compiler) use a work-inefficient method for certain sub-computations,
because it makes better use of hardware resources.  Anyway, let's see
how fast our ``scan`` is.

=====  ==========  =============== ==========
*n*    Builtin            ``scan`` Difference
-----  ----------  --------------- ----------
10³          27μs             86μs       3.2x
10⁴          26μs            116μs       4.5x
10⁵          33μs            138μs       4.2x
10⁶          66μs            506μs       7.6x
10⁷         489μs           4093μs       9.9x
10⁸        4428μs          45149μs      10.1x
=====  ==========  =============== ==========

Ouch.  Scan is one of those algorithms that require quite careful
implementation, and ours is just too simple.  Let's move on.

Finishing up
------------

So far we depend on intrinsics for ``map`` and ``iota``, and have
``scatter``, ``reduce_by_index``, ``partition``, ``stream_map``, and
``stream_red`` left to handle.

For ``scatter``, which is a kind of parallel in-place update of an
array, there's an easy answer: it must be an intrinsic as well.  There
is no efficient way to express it using a ``map``.  It may be possible
to come up with some elaborate scheme where each element performs a
search for the value it's supposed to be replaced with, but it would
be extremely inefficient.

``partition`` is a kind of generalised ``filter``, which can be
expressed with a combination of ``scan`` and ``scatter``:

.. code-block:: Futhark

   let filter 'a (p: a -> bool) (as: []a): []a =
     let keep = map (\a -> if p a then 1 else 0) as
     let offsets = scan (+) 0 keep
     let num_to_keep = reduce (+) 0 keep
     in if num_to_keep == 0
        then []
        else scatter (replicate num_to_keep as[0])
                     (map (\(i, k) -> if k == 1 then i-1 else -1)
                          (zip offsets keep))
                     as

I won't bother benchmarking this one, since it builds on ``scan``,
which performs atrociously.  Similarly, ``reduce_by_index`` is
implemented in the compiler with a sophisticated multi-versioned
approach that leverages primitive atomics when possible, but it can
also be implemented by `sorting
<https://futhark-lang.org/pkgs/github.com/diku-dk/sorts/0.3.3/>`_
followed by a `segmented reduction
<https://futhark-lang.org/pkgs/github.com/diku-dk/segmented/0.2.4/doc/lib/github.com/diku-dk/segmented/segmented.html#3500>`_.
Both of these operations are non-intrinsic library functions that are
implemented in terms of ``map``, ``scan``, and ``scatter``.

Last up are ``stream_red`` and ``stream_map``.  These are fairly
subtle constructs that are used to `expose optimisation opportunities
to the compiler <2017-06-25-futhark-at-pldi.html>`_.  However, their
semantics are quite simple:

.. code-block:: Futhark

   let stream_map 'a 'b [n] (f: (c: i64) -> [c]a -> [c]b) (as: [n]a): [n]b =
     f n as

   let stream_red 'a 'b [n] (op: b -> b -> b) (f: (c: i64) -> [c]a -> b) (as: [n]a): b =
     f n as

But this is too simple - the point of these combinators is permitting
the per-chunk function (``f``) to be sequential (but more
work-efficient), and exploiting parallelism by dividing the input into
parts, each of which is then processed by a thread.  Thus, by merely
applying ``f`` to the whole array, as above, we may end up with a
fully sequential program.  A more reasonable approach is to reorganise
the input arrays into size-1 chunks, and apply ``f`` to each of these:

.. code-block:: Futhark

   let stream_map 'a 'b (f: (c: i64) -> [c]a -> [c]b) (as: []a): []b =
     as |> unflatten (length as) 1 |> map (f 1) |> flatten

   let stream_red 'a 'b (op: b -> b -> b) (f: (c: i64) -> [c]a -> b) (as: []a): b =
     as |> unflatten (length as) 1 |> map (f 1) |> reduce op (f 0 [])

A *good* implementation, and what the compiler does, is more like our
``reduce``: split the input into as many chunks as necessary to
saturate the hardware, and assign each chunk to a thread.

Trying it out
-------------

As a larger example, let's try writing a simple dot product
using these constructs:

.. code-block:: Futhark

   let dotprod [n] (xs: [n]i32) (ys: [n]i32): i32 =
     reduce (+) 0 (map (\(x, y) -> x*y) (zip xs ys))

And running it on the RTX 2080 Ti for various values of *n*:

=====  ============= =============== ==========
*n*    With builtins       With ours Difference
-----  ------------- --------------- ----------
10³          24μs             152μs       6.3x
10⁴          25μs             149μs       5.7x
10⁵          26μs             158μs       6.0x
10⁶          41μs             162μs       4.0x
10⁷         202μs             653μs       3.2x
10⁸        1602μs            3023μs       1.9x
=====  ============  =============== ==========

For *n=10⁸*, the ``dotprod`` using intrinsic ``reduce`` is almost
twice as fast, but we saw earlier that our ``reduce`` is only 10%
slower.  What's going on?  The explanation is that the compiler deeply
understands the intrinsic ``reduce``, and is able to *fuse* the
``map`` with it, such that the array produced by ``map`` is never
actually manifested in memory.  In this program, and many others, the
bottleneck is how fast we can move bytes in and out of memory, so
avoiding unnecessary intermediate arrays has a major impact on
performance.  This fusion does not take place with our home-made
``reduce``.

Conclusions
-----------

Like many languages, Futhark has a good number of intrinsic functions
that are specially known to the compiler.  However, as we have seen
above, most of these can be expressed in fairly simple Futhark code
using only three core primitives (``map``, ``iota``, and
``scatter``).  Performance does suffer for nontrivial programs,
because the compiler will not understand the algebraic structure of
the custom functions, and so will not perform important structural
optimisations.

In summary: use the builtin functions whenever possible; don't try to
outrun ``reduce`` (unless you are really clever, and if you do, please
tell me how!)

If you wish to look at the full code, it is here: `miniprelude.fut
<../static/miniprelude.fut>`_, `miniprelude-benchmark.fut
<../static/miniprelude-benchmark.fut>`_.
