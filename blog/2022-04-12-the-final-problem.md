---
title: The final problem
author: Troels Henriksen
description: Finally we also have to worry about stale caches.
---

Most of Futhark's core developers are academics, meaning that it is
our job to get in trouble and then figure out how to get out again.
Of course, it is well known that computer science only has two
hard problems:

1. Cache invalidation.

2. Naming things.

3. Off-by-one errors.

Having solved the [second](2021-01-11-no-regrets.html#short-names) and
[third](2020-01-24-futhark-0.14.1-released.html) ones, only cache
invalidation is left.  Unfortunately, our efforts towards tackling
this problem were hampered somewhat by Futhark not really having any
caches to invalidate.  This is not conductive to scientific work, so
we set about adding a caching mechanism somewhere in the system.

## Why caching is useful to Futhark

To understand why caching might be useful, and not just as a source of
delightful bugs, we need to understand how GPU APIs such as OpenCL and
CUDA work.  Current GPUs don't like exposing their concrete
instruction sets, probably so the vendors can easily change them
between generations.  Instead, the userspace components of GPU drivers
contain compilers that translate one or more high-level languages
(typically C dialects) to whatever instruction set is appropriate for
the GPU in the system.  This also allows the generated code to be
tuned to the specific hardware in question.  It's really a form of JIT
compilation, although I rarely see that term used in this context.

Compiling code at runtime is of course costly.  While the amount of
code that runs on the GPU (the "GPU program", which Futhark embeds in
the executable as a string) is not large in absolute terms, it is
performance critical, and so the GPU runtime compiler will put in a
lot of effort (meaning time) into doing a good job.  The result is
slow startup times, as every time the program is launched, it has to
hand off its embedded GPU program for compilation.  For example, the
Futhark benchmark
[heston32](https://github.com/diku-dk/futhark-benchmarks/tree/master/misc/heston),
which is sizeable but by no means very large, takes 6.3s from launch
until it is ready to execute application code.  Most of this time is
spent on runtime compilation.

Since the GPU program likely does not change from one run to the next,
an obvious way to speed this up is to cache the compiled GPU program
in a file.  As long as we are careful to recompile the GPU program
whenever the source changes, this should speed up startup
significantly.  Some GPU API implementations (such as NVIDIAs OpenCL)
do this automatically, but in my experience it is somewhat flaky.  Others
(NVIDIAs NVRTC) don't seem to have any automatic caching mechanism at
all, or perhaps only one that operates at a lower layer, after most of
the expensive work has been re-done.

So now Futhark has its own caching mechanism - perhaps no less flaky
than the one in the GPU API implementations, but at least it will be
*our* flakiness to fix.  The basic facility we built upon is that both
OpenCL and CUDA allows one to conveniently retrieve the compiled GPU
code as a byte blob, and to later load such a blob as a program.
Storing such a blob on disk is straightforward.

## How caching is implemented in Futhark

So where on disk should a Futhark program store its cache file?  Here
is where one of my own cantankerous idiosyncrasies enters the picture,
because I would strongly dislike using a programming language where
compiled programs silently litter files in obscure
locations. (`$XDG_CACHE_HOME` counts as *obscure* for the purpose of
this discussion.)  Further, in practice [Futhark will be a
guest](2018-06-18-designing-a-programming-language-for-the-desert.html)
in a larger system, and good guests must not make a mess except where
directed.  Therefore, a Futhark program will by default not touch the
file system, and will not maintain a cache.  When caching is desired,
[the C API](https://futhark.readthedocs.io/en/latest/c-api.html)
exposes [a
function](https://futhark.readthedocs.io/en/latest/c-api.html#c.futhark_context_config_set_cache_file)
for specifying the name of a cache file.  The idea is that Futhark
then automatically manages this file: creating, deleting, updating, or
invalidating as needed, and storing whatever is appropriate for the
backend in use.  This might be nothing for the `c` backend, or a
compiled GPU program for the `opencl` and `cuda` backends.

Suppose a Futhark program is initialising itself and has been told to
use some designated cache file.  If the file does not exist, then the
cache is empty, and the embedded GPU program is runtime-compiled as
usual.  But if the file *does* exist, then our troubles start.  What
if the file has been corrupted?  What if the Futhark program has
changed?  What if the GPU hardware, drivers, or tuning parameters have
changed since the cache file was last updated?  As any properly
trained computer scientist ought to be, I am terrified of stale cache
contents, and so I tried to come up with a cache file design that will
hopefully minimise future misery.

A cache file starts with 8 bytes corresponding to the ASCII string
`"FUTHARK"` followed by a zero byte.  This can be interpreted as a
version number, should it ever become relevant.  After this comes an 8
byte integer encoding the expected size of the cache file.  This is to
detect truncation.  After this is a 32 byte cache key, followed by the
payload.  The key is some sort of hash of the data that went into
constructing the payload.  When loading from the cache, we first
compute the expected key, and only use the payload if the cache key
matches the expected key.  For the GPU backends, the payload will be a
compiled GPU program, but the basic caching mechanism is intentionally
agnostic to what the payload bytes represent.

Computing the cache key is clearly the most interesting part.  Hashing
the GPU program source code is an obvious first start, but we also
include the compiler options (which include tuning parameters and such).
If the GPU program has changed, which happens when the original
Futhark program is modified, the hash will change, and so any old
cache file will be considered invalid.

The specific hash algorithm is not terribly important, as there are no
security aspects to consider.  Using a cryptographic hash would be
safe, but also overkill in terms of code complexity.  Instead we use
an adaptation of [djb2](http://www.cse.yorku.ca/~oz/hash.html), which
is not any more complicated than a basic random number generator.
Since djb2 produces only a single 32-bit word as output, and we need 8
of them to form our 32-byte cache key, I [modified it to also store
some of its intermediate
states](https://github.com/diku-dk/futhark/blob/f34cc97c1971466ad71cfd794101a869b2608fe2/rts/c/cache.h#L36-L44).
I don't know for certain that this actually reduces the risk of
collisions, but it should certainly not increase it.

### Finer points

There are some subtle parts of this design that are worth elaborating.
First, one common way to implement a cache as above is to use
content-addressable storage, where the hash *itself* is used to
construct a file name for the cache.  This means that different users
that happen to cache the same data can use the same cache entry.  For
Futhark, such reuse is exceedingly unlikely.  Also, it has the
downside that it is non-obvious when the cache entries have no users
left and can be deleted.  Futhark's caching mechanism is supposed to
connect each program with *one* cache file, and if the program
changes, then the "old" cache is automatically replaced with the new
one.  This should hopefully avoid an eternally growing cache of
compiled GPU code artefacts.

Another interesting detail is that even should all the safeguards fail
and Futhark load a compiled GPU program from a stale cache, there is a
final bulwark: if the GPU driver fails to load the program (perhaps
because it's from an older version of the driver), Futhark will behave
just as if the cache was invalid, and recompile the GPU program from
the embedded source code.  The only real failure scenario (which is
hopefully rare) is if we load an invalid compiled program *that the
GPU driver still accepts*.

One downside of the automation is that it is not easy for the user to
see whether caching is successful.  This is tragic when caching fails
for some silly reason that is easy to fix, such as having forgotten to
create the directory containing the cache file.  The only way to
observe such failures is to [enable Futhark's logging
mode](https://futhark.readthedocs.io/en/latest/c-api.html#c.futhark_context_config_set_logging)
and inspect the log output.  Not ideal, but we will see whether it
becomes a problem in practice.

## Worth it?

So was this worth the effort?  Yes.  The `heston32` program starts in
0.35s with a valid cache file, as opposed to 6.3s without one.  The
remaining startup time is mostly down to driver initialisation, which
cannot be avoided, and is independent of the size of the Futhark
program.  This means Futhark is [still not a good choice for
short-running command line
programs](https://futhark-lang.org/blog/2019-10-25-beating-c-with-futhark-on-gpu.html),
or at least not when using the GPU backends.

In conclusion, while the use of caching does not strictly speaking
make Futhark code *run* any faster, it certainly does make it *launch*
much faster.  Some users might appreciate this.
