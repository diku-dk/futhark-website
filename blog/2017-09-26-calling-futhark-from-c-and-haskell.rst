---
title: Calling Futhark from C and Haskell
author: Troels Henriksen
description: We recently taught the Futhark native code generator how to generate reusable library code.  This post shows how to call the generated code directly from C, or through Haskell's FFI.  Any language that has a C FFI can use the same technique.
---

One of the core goals of Futhark has always been to make it usable in
practice.  However, as a purely functional high-performance language
incapable of interacting with the outside world except through
function parameters, Futhark is not suited for writing full
applications.  This post describes recent developments in making
Futhark programming not merely fun, but perhaps even useful.

The Futhark compiler possesses two code generators: one that generates
C code that uses the OpenCL library to orchestrate parallel execution,
and another that generates Python code that does the same though
`PyOpenCL <https://mathema.tician.de/software/pyopencl/>`_.  Since
most of the execution time will be spent inside OpenCL kernels that
are identical anyway, the overhead of using Python is often negligible
(see `a previous blog post on the topic
</blog/2016-04-15-futhark-and-pyopencl.html>`_).  Furthermore, we
generate Python code that is easy to invoke as a library from other
Python programs.  This gives us the best of both worlds: we can write
the performance-critical bits in Futhark, and use Python for
high-level application logic and talking to the outside world (`more
on this here </blog/2016-04-25-futhark-and-pygame.html>`_).  This has
allowed us to write a bunch of `fancy demos
</blog/2016-12-04-diving-beet.html>`_ to show off Futhark.

Unfortunately, Python code is not all that easy to call from programs
not themselves written in Python.  Our C code generator was until
recently limited to generating executable programs that read data from
standard input and write results on standard output.  While this is
fine for benchmarking, testing, and development, it is not exactly a
low-overhead way to invoke Futhark code.  To address this, we recently
spent some time extending the C code generator with support for
compiling a Futhark program to *library code* - C code that is meant
to be linked (dynamically or statically) with other programs.  While C
may no longer be the preferred language for application development,
most languages come with `foreign function interfaces (FFIs)
<https://en.wikipedia.org/wiki/Foreign_function_interface>`_ that
allow C libraries to be called more or less conveniently.  When we
make Futhark code callable via a C interface, we thus enable
interoperability with a wealth of languages.

First we will look at how to write a simple Futhark program (`dot
product <https://en.wikipedia.org/wiki/Dot_product>`_) and how to
compile it to a shared library (``.so``).  We will look at the
generated header file and will see how easy it is to call the
generated code from an ordinary C program (if a little heavy on
boilerplate).  Then we will see how to use the FFI of Haskell to call
that same generated code.  Haskell merely serves as a stand-in for any
modern language with an FFI, so the same principles should apply
elsewhere.  Finally, we will discuss how values of more complicated
Futhark types, which have no direct analogue in C (like arrays of
tuples), are mapped to C.

Compiling a Futhark Program to a C Library
------------------------------------------

**Warning: some of the details of library compilation may have changed
 or been updated since this blog post was written in 2017.  Refer to
 the `library guide in the User's Manual
 <https://futhark.readthedocs.io/en/latest/usage.html#compiling-to-library>`_
 for updated details.**

The following Futhark program computes the dot product of two integer
vectors::

  entry dotprod (xs: []i32) (ys: []i32): i32 =
    reduce (+) 0 (map2 (*) xs ys)

We define the ``dotprod`` function with ``entry`` to indicate that it
should be externally visible.  After saving this program as
``dotprod.fut``, we can compile it with the ``futhark opencl``
compiler::

  $ futhark opencl --library dotprod.fut

This produces two files in the current directory: ``dotprod.c`` and
``dotprod.h``.  We can compile ``dotprod.c`` to a shared library like
this::

  $ gcc dotprod.c -o libdotprod.so -fPIC -shared

We can now link to ``libdotprod.so`` the same way we link with any
other shared library.  But before we get that far, let's take a look
at (parts of) the generated ``dotprod.h`` file.  We have written the
code generator to produce as simple header files as possible, with no
superfluous crud, in order to make them human-readable.  This is
particularly useful at the moment, since few explanatory comments are
inserted in the header file.

The first declarations are related to initialisation, which is based
on first constructing a *configuration* object, which can then be used
to obtain a *context*.  The context is used in all subsequent calls,
and contains GPU state and the like.  We elide most of the functions
for setting configuration properties, as they are not very
interesting::

  /*
   * Initialisation
  */

  struct futhark_context_config ;

  struct futhark_context_config *futhark_context_config_new();

  void futhark_context_config_free(struct futhark_context_config *cfg);

  void futhark_context_config_set_device(struct futhark_context_config *cfg,
                                         const char *s);

  ...

  struct futhark_context ;

  struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);

  void futhark_context_free(struct futhark_context *ctx);

  int futhark_context_sync(struct futhark_context *ctx);

The above demonstrates a pervasive design decision in the API: the use
of pointers to *opaque structs*.  The struct ``futhark_context`` is
not given a definition, and the only way to construct it is via the
function ``futhark_context_new()``.  This means that we cannot
allocate it statically, which is contrary to how one would normally
design a C library.  The motivation behind this design is twofold:

  1. It keeps the header file readable, as it elides implementation
     details like struct members.

  2. It is easier to use from FFIs.  Most FFIs make it very easy to
     work with functions that only accept and produce pointers (and
     primitive types), but accessing and allocating structs is a little
     more involved.

The disadvantage is a little more boilerplate, and a little more
dynamic allocation.  However, relatively few objects of this kind are
used, so the performance impact should be nil.

The next part of the header file concerns itself with arrays - how
they are created and accessed::

  /*
   * Arrays
  */

  struct futhark_i32_1d ;

  struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                            int32_t *data,
                                            int dim0);

  int futhark_free_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr);

  int futhark_values_i32_1d(struct futhark_context *ctx,
                            struct futhark_i32_1d *arr,
                            int32_t *data);

  int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                                struct futhark_i32_1d *arr);

Again we see the use of pointers to opaque structs.  We can use
``futhark_new_i32_1d`` to construct a Futhark array from a C array,
and we can use ``futhark_values_i32_1d`` to read all elements from a
Futhark array.  The representation used by the Futhark array is
intentionally hidden from us - we do not even know (or care) whether
it is resident in CPU or GPU memory.  The code generator automatically
generates a struct and accessor functions for every distinct array
type used in the entry points of the Futhark program.

The single entry point is declared like this::

  int futhark_entry_dotprod(struct futhark_context *ctx,
                            int32_t *out0,
                            const struct futhark_i32_1d *in0,
                            const struct futhark_i32_1d *in1);

As the original Futhark program accepted two parameters and returned
one value, the corresponding C function takes one *out* parameter and
two *in* parameters (as well as a context parameter).

We have now seen enough to write a small C program (with no error
handling) that calls our generated library::

  #include <stdio.h>

  #include "dotprod.h"

  int main() {
    int x[] = { 1, 2, 3, 4 };
    int y[] = { 2, 3, 4, 1 };

    struct futhark_context_config *cfg = futhark_context_config_new();
    struct futhark_context *ctx = futhark_context_new(cfg);

    struct futhark_i32_1d *x_arr = futhark_new_i32_1d(ctx, x, 4);
    struct futhark_i32_1d *y_arr = futhark_new_i32_1d(ctx, y, 4);

    int res;
    futhark_entry_dotprod(ctx, &res, x_arr, y_arr);
    futhark_context_sync(ctx);

    printf("Result: %d\n", res);

    futhark_free_i32_1d(ctx, x_arr);
    futhark_free_i32_1d(ctx, y_arr);

    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
  }

We hard-code the input data here, but we could just as well have read
it from somewhere.  The call to ``futhark_context_new()`` is where the
GPU is initialised (is applicable) and OpenCL kernel code is compiled
and uploaded to the device.  This call might be relatively slow.
However, subsequent calls to entry point functions
(``futhark_dotprod()``) will be efficient, as they re-use the already
initialised context.

Note the use of ``futhark_context_sync()`` after calling the entry
point: Futhark does not guarantee that the final results have been
written until we synchronise explicitly.  Note also that we free the
two arrays ``x_arr`` and ``y_arr`` once we are done with them - memory
management is entirely manual.

If we save this program as ``luser.c``, we can compile and run it like
this::

  $ gcc luser.c -o luser -lOpenCL -lm -ldotprod
  $ ./luser
  Result: 24

You may need to set ``LD_LIBRARY_PATH=.`` before the dynamic linker
can find ``libdotprod.so``.  Also, this program will only work if the
default OpenCL device is usable on your system, since we did not
request any specific device.  For testing on a system that does not
support OpenCL, simply use ``futhark c`` instead of
``futhark opencl``.  The generated API will be the same.

Calling the Futhark Library from Haskell
----------------------------------------

While C is no longer the favourite language of application
programmers, surely Haskell is.  Therefore, let's look at how to call
our Futhark library from Haskell.  Haskell has a relatively
lightweight FFI for calling C code, but it's still rather verbose.
First, some necessary imports::

  import Data.Int
  import Foreign.Ptr
  import Foreign.Marshal.Alloc
  import Foreign.Marshal.Array
  import Foreign.Storable

Then we can define the foreign functions.  For brevity, we omit the
functions for freeing context and data::

  data Futhark_Context_Config
  foreign import ccall "futhark_context_config_new"
    futhark_context_config_new :: IO (Ptr Futhark_Context_Config)

  data Futhark_Context
  foreign import ccall "futhark_context_new"
    futhark_context_new :: Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)

  data Futhark_i32_1d
  foreign import ccall "futhark_new_i32_1d"
    futhark_new_i32_1d :: Ptr Futhark_Context -> Ptr Int32
                       -> Int32 -> IO (Ptr Futhark_i32_1d)

  foreign import ccall "futhark_entry_dotprod"
    futhark_entry_dotprod :: Ptr Futhark_Context -> Ptr Int32
                          -> Ptr Futhark_i32_1d -> Ptr Futhark_i32_1d -> IO ()

We use empty data declarations to declare Haskell types corresponding
to the C types.  This is a nice trick for getting type-safe pointers,
but ultimately just a convenience.  Note how easily we are able to
express the pointer-based C functions as Haskell functions.  As the
operations we perform are inherently effectful, we put them in the IO
monad.  This makes the interface somewhat awkward to use from most
Haskell code, but a nicer interface can be built on top of this if
desired.  We can call  the imported functions like this::

  main :: IO ()
  main = do
    cfg <- futhark_context_config_new
    ctx <- futhark_context_new cfg

    x <- newArray [1,2,3,4]
    y <- newArray [2,3,4,1]

    x_arr <- futhark_new_i32_1d ctx x 4
    y_arr <- futhark_new_i32_1d ctx y 4

    res <- alloca $ \res -> do futhark_entry_dotprod ctx res x_arr y_arr
                               peek res
    putStrLn $ "Result: " ++ show res

The Haskell function ``newArray`` produces a C-level heap-allocated
array, which we can pass to ``futhark_new_i32_1d``.  Memory management
is still entirely manual (and since we skip the freeing, this program
leaks memory), but we could easily wrap this in smart pointers with
finalisers to automate it, if we wished.

Compiling and running this program is as straightforward as with C::

  $ ghc luser.hs -ldotprod -lOpenCL
  $ ./luser
  Result: 24

Handling Awkward Futhark Types
------------------------------

Our dot product function uses only types that map easily to C:
primitives and arrays of primitives.  But what happens if we have an
entry point that involves abstract types with hidden definitions, or
types with no clear analogue in C, such as records or arrays of
tuples?  In this case, the generated API defines structs for *opaque
types* that support very few operations.

(Some may argue that records are easily mapped to C structs, and
arrays of tuples to arrays of structs.  This is correct, but we don't
do that yet - it's complicated by the fact that Futhark does not
always represent values in the way indicated by their source language
types, and for example stores an array of pairs by two separate
arrays.  We will probably improve the capabilities of the code
generator in the future, but for now we'll stick with these for our
examples.)

Consider the following contrived program, ``pack.fut``, which contains
two entry points::

  entry pack (xs: []i32) (ys: []i32): [](i32,i32) = zip xs ys

  entry unpack (zs: [](i32,i32)): ([]i32,[]i32) = unzip zs

The ``pack`` function turns two arrays into one array of pairs, and
the ``unpack`` function reverses the operation.  The generated header
file contains the following definitions::

  struct futhark_opaque_z31U814583239044437263 ;

  int futhark_free_opaque_z31U814583239044437263(struct futhark_context *ctx,
                                                 struct futhark_opaque_z31U814583239044437263 *obj);

  int futhark_pack(struct futhark_context *ctx,
                   struct futhark_opaque_z31U814583239044437263 **out0,
                   struct futhark_i32_1d *in0,
                   struct futhark_i32_1d *in1);

  int futhark_unpack(struct futhark_context *ctx,
                     struct futhark_i32_1d **out0,
                     struct futhark_i32_1d **out1,
                     struct futhark_opaque_z31U814583239044437263 *in0);

The unfortunately named struct,
``futhark_opaque_z31U814583239044437263``, represents an array of
tuples.  There is nothing we can do with it except for freeing it, or
passing it back to an entry point.  Clearly we need to improve the
rules by which we generate names for opaque Futhark types (currently
it's a hash of the internal representation), but the basic idea is
sound.

Opaque values typically occur when you are writing a Futhark program
that keeps some kind of state that you don't want the user modifying
or reading directly, but you need access to for each call to an entry
point.  Since Futhark programs are purely functional (and therefore
stateless), having the user to manually pass back the state returned
by the previous call is the only way to accomplish this.

What Remains to be Done
-----------------------

The main missing piece in the generated code is proper error handling.
Python has it easy: just throw an exception and let the automatic
memory management deal with avoiding leaks.  In C, we have to be
careful to avoid leaking memory when we exit early from a function,
and communicating what went wrong to the caller is not easy.  While
our generated code does make an attempt to return proper errors (most
functions return zero on success), most errors will cause a message to
be printed on standard error and the process to be aborted.  This was
fine when we were generating executables, but clearly not acceptable
in library code.

It is also not a given that the current design of the API is
convenient for all users.  We are very interested in figuring out what
kinds of things people may want to use Futhark to, and get some
experience with the current limitations of the design, so we can
improve it.

Finally, it will likely be useful to make the API more flexible.  When
compiling with ``futhark opencl``, it would be useful if you could
pass in an already existing OpenCL context and command queue when
creating the ``futhark_context``.  And certainly it would be nice if
the values of a Futhark array could be written not just to a CPU
array, but also directly to an OpenCL buffer or texture object, thus
saving a round trip.
