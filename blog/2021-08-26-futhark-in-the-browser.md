---
title: Futhark in the browser
description: It works, but you probably don't want to use it just yet.
---

If something exists, then it will eventually also exist in the
browser.  That's just how the world works these days.  Thanks to the
work of [Philip Lassen](http://philiplassen.com/), you can now run
Futhark in your browser.  In this post I will discuss how we made this
work, why it's not yet as useful as one might hope (tldr: no GPU
support yet), and how we hope to eventually make it truly useful.  The
full details on the current design are in [Philip's MSc
thesis](../student-projects/philip-msc-thesis.pdf).

Originally, languages that wanted to run in the browser had to be
compiled to JavaScript. That is fortunately no longer the case.
Today, all major browsers support
[WebAssembly](https://webassembly.org/) (WASM), a fairly conventional
register-based bytecode, which can interoperate with JavaScript.
Generating WebAssembly is very similar to generating assembly code.
Of course, the Futhark compiler currently generates C ([or
Python](2016-04-25-futhark-and-pygame.html)), not assembly.  Writing a
WASM backend would not be particularly hard, but it would have to be
maintained indefinitely.  Further, Futhark has a (tiny) runtime system
written in C that includes facilities for memory management,
profiling, and reporting errors.  Not the end of the world, but it
would be nice not to have to maintain an entirely new backend.

Fortunately, [Emscripten](https://emscripten.org/) exists.  Emscripten
is a compiler toolchain that can compile C programs to WASM.  In
practice, it uses [Clang](https://clang.llvm.org/) as the C compiler
frontend, and then uses LLVM's WASM backend.  Emscripten's
contribution is to glue all these parts together in a way to make it
all *just work*, and also generate low-level JavaScript wrapper code
to make the result available.  I was uncertain about how well it would
work, but it turns out you can mostly just take the output of
Futhark's C code generator, pass it to Emscripten, and get runnable
WASM code that can be accessed from JavaScript!  There are a few
wrinkles (e.g. JavaScript does not directly support 64-bit integers)
that need to be papered over in various way ([see Philip's
thesis](../student-projects/philip-msc-thesis.pdf)), but nothing
particularly onerous.

Of course, this approach will result in a JavaScript module that
exposes the equivalent of Futhark's [C
API](https://futhark.readthedocs.io/en/latest/c-api.html), including
objects that are essentially pointers into the WASM heap.  While I can
imagine JavaScript programmers must have *incredible* pain tolerance
given the field they are in, even for them this is perhaps a bridge
too far.  So in truth, the most complicated part of Futhark's WASM
backend is that we automatically generate a [relatively *idiomatic*
JavaScript API](https://futhark.readthedocs.io/en/latest/js-api.html)
for the compiled Futhark program.

For example, a Futhark program `futlib.fut` can be compiled with

    $ futhark wasm --lib futlib.fut

which produces the files `futlib.wasm` and `futlib.mjs`.  The latter
can be imported from JavaScript with:

```JavaScript
import { newFutharkContext } from './futlib.mjs';
newFutharkContext().then(ctx => ...);
```

The `ctx` parameter is a `FutharkContext` instance.  For each entry
point in the original program, this class will have a corresponding
method.  Futhark-side arrays are represented with a special
`FutharkArray` class, which can be converted to a typed JavaScript
array with a method.  This is to avoid having to copy Futhark values
from the WASM heap and into JavaScript unless absolutely necessary -
values that are simply passed from one Futhark entry point to the next
will not need to be copied.  All of this is very similar to the API we
expose in the Python code generator.  [This test
program](https://github.com/diku-dk/futhark/blob/master/tests_lib/javascript/test_array.js)
provides a good glimpse of what using it looks like in practice.  And
if you actually want to see Futhark running in your browser, then [go
here](http://philiplassen.com/wasm/).  Fiddle with the sliders and a
fractal should appear, rendered by Futhark.

## Parallel WASM

The `futhark wasm` command will generate *sequential* WASM code.
Futhark's sequential code generator is *fine*, but it is obviously not
satisfying for a parallel language.  Futhark's `multicore` backend
generates C that uses [POSIX
threads](https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html)
(pthreads) as the underlying thread API.  Can we just pass that to
Emscripten?  Amazingly, [yes we
can](https://emscripten.org/docs/porting/pthreads.html)!  Emscripten
recognises the pthreads API and implements it using a combination of
WebWorkers and
[SharedArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer).
So our `futhark wasm-multicore` backend will essentially just run the
`multicore` codegen and pass the result to Emscripten.  The generated
JavaScript API is the same as for `futhark wasm`.

Of course, this is web programming, so it doesn't actually work as
well as that.  The SharedArrayBuffer API is heavily restricted as it
is a potential [SPECTRE attack
vector](https://blog.mozilla.org/security/2018/01/03/mitigations-landing-new-class-timing-attack/),
so you need to take [special measures](https://web.dev/coop-coep/) to
use it on a website.

Still, if you expend enough suffering, then browsers will deign you
worthy of running Futhark in your browser on multiple cores.
Preliminary benchmarking done in [Philip's
thesis](../student-projects/philip-msc-thesis.pdf) suggests that our
thread scheduler works as well in the browser as when running
natively, but perhaps we'll get a nasty surprise down the line.

Unfortunately, nobody has yet suffered enough to make Futhark run on
the GPU in your browser.

## GPU WASM

First up, browsers *do* allow JavaScript code to run GPU programs
("shaders").  Given the quality of GPU drivers, this is perhaps mildly
terrifying, but let's leave that aside.  There are two low-level APIs
for GPU programming in the browser:

* [WebGL](https://www.khronos.org/webgl/) is basically OpenGL ES in a
  ~trenchcoat~ straightjacket.  It's quite widely supported, but the
  API (like OpenGL) is fairly old-fashioned, and not a great fit for
  modern GPUs.  Shaders are written in a variant of GLSL.

* [WebGPU](https://www.w3.org/TR/webgpu/) is the *new thing* meaning
  that it is [not yet widely
  supported](https://github.com/gpuweb/gpuweb/wiki/Implementation-Status).
  It's a more modern API that is not explicitly modeled after any
  native API, but is similar in spirit to Metal and Vulkan, although
  *far* simpler than the latter.  Shaders are written in the bespoke
  language [WGSL](https://www.w3.org/TR/WGSL/), a [not
  uncontroversial](https://github.com/gpuweb/gpuweb/issues/566)
  decision, but it's ultimately very similar to modern low-level
  shader languages such as [SPIR-V](https://www.khronos.org/spir/).

Both WebGL and WebGPU are primarily oriented towards graphics, but
WebGPU explicitly also aims at providing good support for *compute
shaders*, which is exactly what Futhark needs.  Since WebGPU seems to
be the future (if not exactly the *present*), it is what we ultimately
wish to target.

Futhark's current GPU backends generate either OpenCL or CUDA code.
Are we lucky enough that Emscripten can just translate these APIs for
us, like with pthreads? No.  But it can do something [seductively
similar](https://github.com/emscripten-core/emscripten/pull/10218).
See, WebGPU is not just a JavaScript API; enterprising persons have
also defined
[webgpu.h](https://github.com/webgpu-native/webgpu-headers), a C API
that is exposed by native WebGPU implementations such as
[wgpu-native](https://github.com/gfx-rs/wgpu-native).  Emscripten can
translate a C program making use of the WebGPU C API to a
WASM/JavaScript program calling the WebGPU JavaScript API.  This is
really cool.  It means if we construct a Futhark compiler backend that
generates C code with calls to the webgpu.h functions, we can compile
that with Emscripten and run it in the browser.  So how much work will
it take to create such a backend?

It's unfortunately not entirely trivial.  For maintenance reasons, it
would be best if the host-level code (that is, the code *not* running
on the GPU) we emit is reasonably similar across all our GPU backends.
Futhark does not actually generate very complicated host code, but it
does require the ability to do a full synchronisation, which is [not
yet supported by
WebGPU](https://github.com/webgpu-native/webgpu-headers/issues/91) -
presumably because you'd normally hook into the JavaScript event loop,
which is also what Emscripten does.  This is mostly an artifact of
WebGPU still being immature, and it can be hacked around for now.

A more *interesting* problem is how to generate the code that will
actually run on the GPU - the *shaders* (or *kernels* in OpenCL/CUDA
terms).  CUDA and OpenCL both support a subset of C for writing GPU
code, so it has been quite easy to maintain these two code generators.
WGSL looks like a completely different language, at least
syntactically, but the basic programming model is very similar.
Writing and maintaining a small WGSL code generator is in principle
not difficult - the [core parts of our C
generator](https://github.com/diku-dk/futhark/blob/51eb0c478b6059f1629fc461d79113745a357220/src/Futhark/CodeGen/Backends/GenericC.hs#L1823-L2109)
is less than 300 lines.  The main problem is that Futhark requires
features that WGSL does not support.  So what are these extremely
elaborate features that are expected by a pampered academic functional
language like Futhark, and which no pragmatic, real-world shader
language could possibly support?  Unreasonable stuff like:

* 8-bit, 16-bit, and 64-bit integers.
* Double precision floats.
* [Compare-and-swap](https://en.wikipedia.org/wiki/Compare-and-swap).
* Pointers.

Some of these can be worked around.  The need for pointers because we
have an optimisation for memory reuse, where some memory block might
be used for storing 16-bit integers early in the program, and
single-precision floats later.  This requires us to "cast" between
pointers in the generated code (or just issue arbitrary loads from
untyped addresses), but if necessary this optimisation can be
disabled.

Double precision floats are also not a hard requirement.  Since even
in OpenCL, some GPUs do not support them, we have code for
conditionally enabling them only if the program needs them - and it's
always up to the programmer whether to use them.

WGSL's limited integer types is a larger problem.  One issue is that
[Futhark uses 64-bit integers for all
sizes](2020-09-01-performance-regression.html).  I suppose we can
simulate 64-bit integers as pairs of 32-bit integers, and even store
them like that in memory.  For 8-bit and 16-bit integers, you can
easily simulate them with 32-bit integers in scalar code - in fact,
since real GPUs don't have 16-bit registers or ALUs, that's usually
how they end up being compiled.  The problem here is the *in-memory
representation*.  Futhark expects to be able to have arrays of 8-bit
integers, taking up one byte per element, and that *a write to an
element is atomic*.  We can't just write an entire 32-bit integer to
update an 8-bit element, since it will clobber the neighbouring
elements.

Futhark [uses an 8-bit integer for the constructor tag in a sum
type](2019-08-21-futhark-0.12.1-released.html#representation), so even
if *your* code does not use any 8-bit integers, the generated code
might still have them.  Further, Futhark's [flattening
transformation](2017-06-25-futhark-at-pldi.html#improving-available-parallelism-via-loop-distribution-and-interchange)
means that even if your original code does not ever explicitly
construct an array of 8-bit integers, the compiler might still produce
some.  This is *core stuff* in the compiler - not something we can
just turn off in an prospective WebGPU backend.

I'm not yet sure how to handle this.  For an initial prototype, we can
certainly just ignore the problem, since it does not show up for every
program.  Long-term, WGSL does actually reserve keywords corresponding
to the missing integer types, so maybe they'll get around to
supporting them.

Adding a WebGPU backend certainly has lots of potential for fun.  If
anyone is interested in helping out, we have a [tracking
issue](https://github.com/diku-dk/futhark/issues/1403).
