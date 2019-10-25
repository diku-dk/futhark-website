---
title: Beating C with Futhark running on GPU
author: Troels Henriksen
description: With Futhark, you don't have to be smart to make your code run fast, you just need to be able to afford an expensive GPU.
---

Chris Penner recently wrote a blog post titled `Beating C with 80
lines of Haskell <https://chrispenner.ca/posts/wc>`_, where he showed
how to optimise and parallelise a Haskell implementation of ``wc`` to
outperform the implementation bundled with macOS.  This made a lot of
people on the Internet angry, as they felt that ``wc`` is already fast
enough (it is), is often applied on streams whereas Chris Penner's
implementation requires an input file (probably), and that the
optimised Haskell code is unreasonably complicated (arguably).
However, I really enjoyed the explanation of how to parallelise a
seemingly-sequential problem via a clever monoid.

Not long after, Matthew Maycock wrote `Beating C with Dyalog APL
<https://ummaycoc.github.io/wc.apl/>`_, where he implemented ``wc`` in
APL.  In a degenerate way, Futhark can be seen as the bastard
offspring of Haskell and APL, so I obviously have to get involved as
well.  In particular, I want to show that it is straightforward to
write highly efficient parallel code using the same high-level
principles as we would in Haskell.  `The full source code is available
on GitHub <https://github.com/athas/futhark-wc>`_.

Now, to pre-appease angry Internet people, let me make things clear
from the start: Futhark is a decent language for counting words, but a
bad language for implementing ``wc`` specifically.  In particular,
Futhark is a *pure* language, and not in the way that Haskell is
"pure" but still seems to find room for highly sophisticated and
efficient IO mechanisms.  No, in Futhark, you *cannot read a file or
print to the screen*, which makes it rather tricky to implement a
program like ``wc`` that is *entirely* about reading from a file and
printing to the screen!  What I will show is how to implement the core
word-counting logic in Futhark (using the same approach as for
Haskell), and how to call it from a wrapper program written in C that
takes care of the IO.

``libwc.fut``
-------------

The core of the word counting logic is going to be taken directly from
Chris Penner's Haskell implementation.  I won't go through it in
detail, but highly recommend reading `his original article
<https://chrispenner.ca/posts/wc>`_.  Like Haskell, Futhark is a
functional language, so the translation is mostly straightforward.

To start out, we define two sum types that encode a kind of state
machine for the word counting:

.. code-block:: Futhark

   -- Value constructors are #-prefixed in Futhark.

   type char_type = #is_space | #not_space
   type flux = #flux char_type i32 char_type | #unknown

We then define an associative function for combining two ``flux``
values, along with a neutral element.  This means that ``flux`` is a
`monoid <https://en.wikipedia.org/wiki/Monoid>`_.  In Haskell, this is
done by implementing the ``Monoid`` type class.  Futhark does not have
type classes, so we just write the definitions like any other:

.. code-block:: Futhark

   let flux_mappend (x: flux) (y: flux): flux =
     match (x,y)
     case (#unknown, _) -> y
     case (_, #unknown) -> x
     case (#flux l n #not_space,
           #flux #not_space n' r) ->
       #flux l (n + n' - 1) r
     case (#flux l n _ ,
           #flux _ n' r) ->
       #flux l (n + n') r

   let flux_mempty : flux = #unknown

Now we need a function for mapping characters to ``flux`` values.
Futhark is generally not a good language for string processing (in
fact, it has neither a string nor a character type), so we are going
to restrict ourselves to ASCII, and represent characters as bytes.
First, a function for determining whether something is a whitespace
character:

.. code-block:: Futhark

   let is_space (c: u8) =
     c == 9 || c == 10 || c == 32

This function recognises newlines, tabs, and spaces as whitespace.  We
can now define the ``flux`` function:

.. code-block:: Futhark

   let flux (c: u8) : flux =
     if is_space c
     then #flux #is_space 0 #is_space
     else #flux #not_space 1 #not_space

Next we define the ``counts`` type, which is a record tracking the
number of characters, words, and lines we have seen so far.

.. code-block:: Futhark

   type counts = { chars: i32
                 , words: flux
                 , lines: i32 }

And we also define a combining function and neutral element for ``counts``:

.. code-block:: Futhark

   let counts_mappend (x: counts) (y: counts) =
     { chars = x.chars + y.chars,
       words = x.words `flux_mappend` y.words,
       lines = x.lines + y.lines }

   let counts_mempty : counts =
     { chars = 0, words = flux_mempty, lines = 0 }

And given a single character on its own, its ``count``:

.. code-block:: Futhark

   let count_char (c: u8) : counts =
     { chars = 1, words = flux c, lines = if c == 10 then 1 else 0 }

Finally we can put together the pieces and create a function for
counting the number of characters, words, and lines in a "string"
(modelled as an array of bytes), with a ``map``-``reduce`` composition
just as in Haskell:

.. code-block:: Futhark

   entry wc (cs: []u8) : (i32, i32, i32) =
     cs
     |> map count_char
     |> reduce counts_mappend counts_mempty
     |> \counts ->
          (counts.chars,

           match counts.words
           case #unknown -> 0
           case #flux _ words _ -> words,

           counts.lines)

Performance depends crucially on the compiler performing fusion to
avoid constructing a large intermediate array as the result of the
``map``.  Fortunately, the Futhark compiler is very good at fusion.

The ``wc`` function is defined with ``entry`` rather than ``let``
because we want it to be callable from the outside world.  When we
compile this program, only ``entry`` functions will be visible in the
generated API.  The final lambda simply transforms the ``counts``
record, which for technical reasons would be opaque to the outside
world, into a simple triple of integers.

``wc.c``
--------

We compile ``libwc.fut`` into a C library containing (for now)
ordinary sequential C code::

  $ futhark c --library libwc.fut

This produces two files: ``libwc.h`` and ``libwc.c``, with the former
defining the interface to the latter.  Futhark's C API is a bit
verbose, but fundamentally simple.  First, we initialise our generated
library by creating a context:

.. code-block:: C

  struct futhark_context_config *cfg =
    futhark_context_config_new();

  struct futhark_context *ctx =
    futhark_context_new(cfg);

``libwc.h`` declares a function ``futhark_entry_wc()`` that
corresponds to our ``wc`` function.  It has the following type:

.. code-block:: C

   int futhark_entry_wc
     (struct futhark_context *ctx,
      int32_t *out0,
      int32_t *out1,
      int32_t *out2,
      const struct futhark_u8_1d *in0);

So, the argument cannot just be any old C array, it has to be a
specific ``futhark_u8_1d``.  There is a function
``futhark_new_u8_1d()`` for creating these:

.. code-block:: C

   struct futhark_u8_1d *futhark_new_u8_1d
     (struct futhark_context *ctx,
      uint8_t *data, int64_t dim0);

Creating such an array always involves a copy, because Futhark wants
to manage its own memory.  To avoid copying the input file contents
more than once, we use ``mmap()`` on the open file and pass the
resulting pointer to Futhark.  The entire procedure looks like this:

.. code-block:: C

   FILE *fp = fopen(filename, "r");

   fseek(fp, 0, SEEK_END);
   size_t n = ftell(fp);
   rewind(fp);

   void *data =
     mmap(NULL, n, PROT_READ, MAP_SHARED, fileno(fp), 0);

   struct futhark_u8_1d *arr =
     futhark_new_u8_1d(ctx, data, n);

We can then call the Futhark entry point:

.. code-block:: C

   int chars, words, lines;
   futhark_entry_wc(ctx, &chars, &words, &lines, arr);

Print the result:

.. code-block:: C

   printf(" %d %d %d %s\n", lines, words, chars, filename);

Putting all of this (`plus some boilerplate and cleanup
<https://github.com/athas/futhark-wc/blob/master/wc.c>`_) in ``wc.c``,
we can compile with::

  $ gcc wc.c libwc.c -o wc -O3 -lm

And that's it!  So, how fast is it?  I'll be testing on a 100MiB file
``huge.txt`` that is merely ``big.txt`` from the original post
repeated some times.  First, let us check out GNU ``wc`` 8.22 (using
the C locale so ``wc`` can also assume ASCII):

.. code-block:: C

   $ time wc huge.txt
     2055312  17531120 103818656 huge.txt

   real  0m0.586s
   user  0m0.568s
   sys   0m0.016s

Now for our Futhark ``wc``:

.. code-block:: C

   $ time ./wc-c huge.txt
     2055312 17531120 103818656 huge.txt

   real   0m0.516s
   user   0m0.435s
   sys    0m0.079s

Not bad!  It actually runs faster than GNU ``wc``.  I ran both
programs a few times and took the fastest runtime for each.  And note
that this is *without any parallelism* or low-level optimisations!  I
am of course hugely biased, but I think the Futhark program is an
easier read than the optimised Haskell program.

But really, Futhark is a parallel language, and generating sequential
C code is not what it's for.  So how do we make this program run in
parallel on my employers' $1000 RTX 2080 Ti GPU?  We simply recompile
using ``futhark opencl`` instead of ``futhark c``::

  $ futhark opencl --library libwc.fut
  $ gcc wc.c libwc.c -o wc -O3 -lm -lOpenCL

Alright, let's check out the performance:

.. code-block:: C

   $ time ./wc-opencl huge.txt
     2055312 17531120 103818656 huge.txt

   real   0m0.309s
   user   0m0.083s
   sys    0m0.157s

Well, it's better, but not really by that much.  There are two
possible reasons:

  (1) Word counting is primarily IO-bound, and it is much too
      expensive to ferry the file contents all the way to the GPU over
      the (relatively) slow PCI Express bus just to do a relatively
      meagre amount of computation.

  (2) GPU initialisation (hidden inside the ``futhark_context_new()``
      call) takes a nontrivial amount of time, as it may involve JIT
      compilation of GPU kernels and other bookkeeping operations by
      the GPU driver.

On this machine, for this problem, reason (2) is the most significant.
If we augment ``wc.c`` with a ``-t`` option that makes it perform its
own internal timing, excluding the context initialisation (but
including copying the entire file to the GPU), we get this::

  ./wc-opencl -t huge.txt
    2055312 17531120 103818656 huge.txt
  runtime: 0.070s

Much faster!  Apparently context initialisation has a fixed cost of
about 230 milliseconds on this machine.  This is relatively fast - I
have seen multiple seconds on other systems.  This is the main reason
why Futhark is a bad choice for ``wc``, or other kinds of very
short-running processes - you really do not want to pay this startup
cost unless it can be amortised by a significant amount of subsequent
computation.

Is the Futhark code as efficiently written as possible?  I think it's
close, but I know that the ``char_type`` values will be stored as an
entire byte each, despite only encoding a single bit of information.
This does not matter on the CPU, but on the GPU, this storage comes
out of the fairly sparse on-chip scratchpad memory.  I have not
measured the impact, but a more compact encoding might improve
performance slighly.  However, I generally believe that such
representation-level optimisations are the job of the compiler.

In conclusion, I'm actually surprised that Futhark manages to
out-compete GNU ``wc`` at all - I would have thought that the overhead
of copying the file to the GPU would offset the faster computation.
Most likely, GNU ``wc`` does not have any special optimisations for
the case of word-counting large ASCII files, as it is already more
than fast enough.

Since I still don't believe Futhark is a good choice for implementing
``wc``, I think the main takeaway here is that data-parallelisation
techniques developed for other languages (e.g. Haskell) can be
transfered to Futhark with good results.
