---
title: Benchmarking a real Futhark application
description: Futhark has good tools for benchmarking programs specifically written to be benchmarks, but what happens when you have a full program and you want to investigate how it works? This post tells you what ought to happen.
---

Since Futhark was [originally](2021-12-19-past-and-present.html) designed to
conduct research in compiler optimisations, it should come as no great surprise
that it has good built-in tools for benchmarking programs. However, these tools
were largely built with the assumption that you were benchmarking programs
specifically written to *be* benchmarks, with a single entry point functions and
cleanly defined inputs stored in a file on disk. Real uses of Futhark involve
multiple entry points, and the data may be derived from weird sources, or
generated on the fly by non-Futhark code. In this post I will talk about how
some of Futhark's crucial properties, particularly its
[value-orientation](2026-04-22-value-oriented-programming.html) makes it
reasonably easy to isolate the benchmarkable parts of larger applications, and
also how to extract useful profiling data even when such isolation is not
possible.

## The program

The program [`fuchat`](https://github.com/jeromew/fuchat) is an LLM chat bot
written by Jérôme Wagner, inspired by a [previous program by Borna
Ahmadzadeh](https://github.com/BobMcDear/llaf). The computational core is
written in Futhark, but it also has a nontrivial amount of logic written in
Python for downloading model weights, handling tokenization, interaction with
the user, and so on. The coolest part about `fuchat`, to me, is actually a
clever use of uniqueness types for managing a KV cache efficiently (although it
took fixing [one](https://github.com/diku-dk/futhark/issues/2461) or
[two](https://github.com/diku-dk/futhark/issues/2459) or
[three](https://github.com/diku-dk/futhark/issues/2456) type checker bugs to
make it work). I will probably discuss that in a future post; this one is about
measuring performance.

I don't actually expect that a program written in Futhark can compete
performance-wise with tools such as
[llama.cpp](https://github.com/ggml-org/llama.cpp), as the computationally
intensive parts of these models is mainly found in very well-understood kernels
such as SGEMM and Attention. While Futhark generates *decent* code, it cannot
compete with hand-tuned code optimised by experts, whose performance is the
foundation of essentially entire industries. Still, we *gotta go fast*, and to
make things fast, we must be able to conveniently measure the impact of changes
we make.

If we just run `fuchat`, it does actually tell us achieved performance, in
tokens per second, after every interaction:

```
(ctx: 0/8192)> Tell me, how fast can we actually make a hedgehog go if we use GPUs?

The speed at which a hedgehog can move depends on the context and the type of
movement. A hedgehog is a small, slow-moving animal, but if we consider the
speed of a **hedgehog** in terms of its **movement speed** (not its speed in
terms of a computer or GPU), then it's extremely slow. However, if we are
talking about the **speed of a computer or GPU** processing data, then the speed
would be much higher.

t/s: 8.586 s
```

The performance we obtain here is what we ultimately care about, but going
through the interactive chat interface is awkward, and it is hard to isolate the
impact of changes. To understand the performance of `fuchat`, we need to see how
it works.

## How `fuchat` works

The Futhark code in `fuchat` is (as of this writing) entirely contained in the
file
[qwen-f32.fut](https://github.com/jeromew/fuchat/blob/9314bba362b53c205bf0a3719bf3a6d6213c7c5e/qwen-f32.fut)
(there is also an [`f16`](2021-08-05-half-precision-floats.html) variant, but
let's ignore that). It exposes two entry points: `init`, which is called once to
construct an initial cache, and `gen` which is the actual token generator. It is
the `gen` function that is the computational core of the Futhark code, and hence
the one whose performance we care about. (There are also some entry points for
demoing tool calling, which is very cool, but let us ignore those as well.)

Instead of linking directly to compiled code, `fuchat` talks to compiled Futhark
code via [server mode](2021-01-18-futharkscript.html#futhark-server-mode), where
a compiled Futhark program talks to the world through a text-oriented RPC
mechanism, allowing loading of data, calling entry points, and so on. Using
server mode, we load data into server variables whose values then live inside
the running Futhark context (presumably stored on GPU), and we can then call
entry point functions on values stored in those variables, storing the result in
another server variable.

The main advantage of using server mode is that it is just a lot less hassle
than linking and [calling C
functions](2022-07-01-how-futhark-talks-to-its-friends.html). You compile the
Futhark program to a server-mode executable using your preferred backend, in my
case `hip`:

```
$ futhark hip --server qwen-f32.fut -o qwen-f32
```

And then you have a `qwen-f32` executable that the code in `chat.py` talks to.

Using the server mode of interacting with Futhark has a small amount of overhead
compared to using the C API,
but the main overhead is that exchanging data requires serialising to temporary
files. For `fuchat`, the big data, such as weights and cache, stay resident
inside the Futhark program, and the few bits that are exchanged - user input and
generated tokens - are tiny compared to the computational load. As such,
`fuchat` is very well suited for server-mode interaction. The protocol is pretty
simple, and `fuchat` uses a [convenience Python
library](https://github.com/diku-dk/futhark-server-python) to make it even
easier to use (also [available for Standard
ML](https://github.com/diku-dk/futhark-server-sml) should you wish it).

## Using `futhark bench`

Futhark comes with a benchmarking tool called [`futhark
bench`](https://futhark.readthedocs.io/en/latest/man/futhark-bench.html). You
use it by putting an appropriate comment in your source program, telling it
which entry point to benchmark, and which data to pass it:

```Futhark
-- ==
-- entry: sum_array
-- input @ input.data

entry sum_array (xs: []f32) = f32.sum xs
```

There's more to it: you can provide expected output, multiple inputs, generate
random inputs, etc. The coolest part is that it uses an [adaptive measurement
methodology](2022-04-15-futhark-0.21.9-released.html#adaptive-benchmarking)
where it keeps running until certain statistical properties are fulfilled. It's
all very convenient. However, it depends on having the arguments to the function
stuffed away in a file or similar. In the case of `fuchat`, those inputs are
produced at run-time by Python code that downloads stuff from cyberspace and
calls various functions from third party libraries.

But here is the trick: all Futhark values that can cross entry points can be
losslessly serialised to files. This is a core advantage of [value-oriented
programming](2026-04-22-value-oriented-programming.html). Since Futhark
functions are stateless, this completely captures the input to an entry point.
Therefore, we can instrument the Python program such that *just before* it calls
an entry point using the server `call` command, we ask the server to dump input
to a file. We can then use that file as input to the entry point for
benchmarking.

If we wish, we could dump data for *all* calls to `gen`. It wouldn't even be
that hard to automate it, since they all go through the method `cmd_call` in the
Python implementation of the server protocol. However, that would probably
result in a ruinous amount of disk usage and an angry email from the server
administrators at [my department](https://di.ku.dk), so instead I arbitrarily
decide to store the inputs for the 10th call, which will hopefully be
representative. We do this by simply adding a counter to the `LLM` class in
`chat.py`:

```Python
self.counter=0
```

And then we add some code just before the `gen` entry point is called to dump the
values:

```Python
self.counter += 1
if self.counter == 10:
    self.server.cmd_store('data.in', 'xsat', 'xs', 'params', 'cache', 'eos_token_id', 'max_new_tokens')
# Original call is unchanged.
self.server.cmd_call('gen', 'out', 'xsat', 'xs', 'params', 'cache', 'eos_token_id', 'max_new_tokens')
```

You can see the names of the server-mode variables containing the inputs (`xsat`
and so on), and the variable for the output (`out`). If we wanted, we could also
save the output, to ensure that our optimisations don't change the result.

This produces a handy little 4.9GiB file `data.in`. We then write a file
`benchmark.spec`, that contains benchmarking directives similar to the example
above:

```
==
entry: gen
input @ data.in
```

And then we ask `futhark bench` to benchmark our program, taking the directives
from the file:

```
$ futhark bench qwen-f32.fut --backend=hip --spec-file benchmark.spec
Compiling qwen-f32.fut...
Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
More runs automatically performed for up to 300s to ensure accurate measurement.

qwen-f32.fut:gen (no tuning file):
data.in:     187132μs (95% CI: [  186569.1,   189053.6])
```

That's it! Now we can benchmark `gen` in isolation. The use of an external file
is not critical, and we could simply have added benchmarking comments to
`qwen-f32.fut` itself. However, my principle is that benchmarking should be
possible without modifying the program being benchmarked in any way. The only
question is whether the tenth call to `gen` is really representative, but that
can be answered by just dumping more inputs and comparing them.

Note that although the data storage format is not stable across compiler
versions (although I don't remember it ever changing), it *is* stable across
compiler backends, so this also makes it easy to investigate the performance
impact of using a different backend, or tweaking other tuning parameters:

```
$ futhark bench qwen-f32.fut --backend=multicore --spec-file benchmark.spec
Compiling qwen-f32.fut...
Reporting arithmetic mean runtime of at least 10 runs for each dataset (min 0.5s).
More runs automatically performed for up to 300s to ensure accurate measurement.

qwen-f32.fut:gen (no tuning file):
data.in:    1943980μs (95% CI: [ 1903006.6,  1970727.9])
```

We can now also use the other convenient tooling built around `futhark bench`.
For example, we can ask for machine-readable profiling information in order to
understand what our code actually does:

```
$ futhark bench qwen-f32.fut --backend=hip --spec-file benchmark.spec --json results.json --profile
...
```

This produces a file `results.json` which we can pass to the program `futhark
profile`:

```
$ futhark profile results.json
Writing results to results.prof/
Stripping 'qwen-f32.fut:gen' from program paths.
```

I will not go over everything in the generated directory - [the
manual](https://futhark.readthedocs.io/en/latest/man/futhark-profile.html) does
a decent job of that. Nor will I claim that it has all the information you might
want, or that it is all as readable as we would want - [that is future
work](https://github.com/diku-dk/futhark/issues?q=state%3Aopen%20label%3A%22futhark-profile%22).
But the `data.in.summary` file does produce a table that tells us 50% of the
run-time is spent in a cost centre labeled `copy_host_to_dev`, executed 20
times, which is somewhat unexpected, as this is the name of the cost centre that
copies CPU arrays to the GPU. That probably merits further investigation.
(Update: [this turned out to be an error in the benchmarking
tool](https://github.com/diku-dk/futhark/issues/2464).)

## Not using `futhark bench`

Suppose now that our program was more complicated, using multiple entry points
with widely divergent inputs, and isolating the entry points such that we can
use `futhark bench` is not practical or would not lead to useful information. It
is still possible to obtain profiling information that describes the behaviour
of the running program.

To do this, we must start the server executable with the `--profile` option,
which makes it collect profiling information while it is running, and use the
server command `report` (and optionally `pause_profiling`/`unpause_profiling`).
In the case of `fuchat`, it is not so difficult. Simply change how the
executable is started:

```Python
self.server = futhark_server.Server('./qwen-%s' % type, '--cache=qwen-%s.cache' % type, '--profile')
```

And then we add a command to the user interface that requests the profiling
report and dumps it to a file:

```Python
if user_message == "report":
    open("report.json", "w").write('\n'.join(llm.server.cmd_report()))
    continue

```

This produces a file `report.json` that we can pass to `futhark profile`, just
as we did above. The only difference is that it does not contain results on a
per-entry-point basis, but instead a summary of the behaviour of entire
application lifetime.

## Conclusions

Although Futhark's benchmarking tooling is mostly designed for the somewhat
artificial way that academic benchmark programs are not written, the ability to
easily identify and store the arguments to an entry point makes it quite easy to
treat real programs as a collection of smaller and well-defined benchmark
problems.
