---
title: Using Futhark with PyOpenCL
author: Troels Henriksen
description: Compiling a Futhark program into an ordinary reusable Python module with calls to PyOpenCL.
---

**INCOMPLETE**

Python is a language with many qualities, but few would claim that
performance is among them.  While libraries such as Numpy can be used,
they are not as flexible as being able to write code directly in a
high-performance language.  Unfortunately, writing the
performance-critical parts of a Python program in (say) C is not
always a good experience, and the interfacing between the Python code
and the C code can be awkward and inelegant (although to be fair, it
is still nicer in Python than in many other languages).  It would be
more convenient if we could compile a high-performance language
directly to a Python module that we could then ``import`` like any
other piece of Python code.  Of course, this entire exercise is only
worthwhile if the code in the resulting Python module executes much
faster than manually written Python.  Fortunately, when most of the
computation can be offloaded to the GPU via OpenCL, the Futhark
compiler is capable of this feat.

OpenCL works by having an ordinary program running on the CPU that
transmits code and data to the GPU (or any other *accelerator*, but
we'll stick to GPUs).  In the ideal case, the CPU-code is mostly glue
that performs bookkeeping and making API calls - in other words, not
resource-intensive, and exactly what Python is good at.  No matter the
language the CPU code is written in, the GPU code will be written in
OpenCL C and translated at program initialisation to whatever machine
code is needed by the concrete GPU.

.. image:: /images/cpu_gpu_division.svg
   :alt: Division between CPU and GPU computation
   :class: centre

This is what is exploited by the `PyOpenCL
<https://mathema.tician.de/software/pyopencl/>`_ code generation
backend in the Futhark compiler.  Certainly, the CPU-level code is
written in pure Python and quite slow, but all it does is use the
PyOpenCL library to offload work to a GPU.  The fact that this
offloading takes place is hidden from the user of the generated code,
who is provided a module with functions that accept and produce
ordinary Numpy arrays.

To demonstrate this capability, we will implement a simple `Mandelbrot
set <https://en.wikipedia.org/wiki/Mandelbrot_set>`_ visualisation
program in Python.  We will use Futhark to compute the set and produce
the pixel data, and make use of Pythons `png
<https://pythonhosted.org/pypng/png.html>`_ module to write an image
file with the result.  Our program will consist of two files:
``mandelbrot.fut`` and ``mandelbrot-run.py``.

`mandelbrot.fut </static/mandelbrot.fut>`_
----------------------------------------------

(Do not worry too much about the specifics of how the Mandelbrot set
is defined - it is not the point of this post.)

For convenience, we ask that all decimal literals be considered single
precision (the type `f32` in Futhark)::

  default(f32)

Since Futhark does not have built-in support for complex numbers, we
have to define our own.  Futhark does not yet support proper
user-defined types, so we decide to simply represent complex numbers
as pairs of `f32`s.  We need three operations: dot product,
multiplication, and addition::

  fun f32 dot({f32,f32} c) =
  let {r, i} = c
  in r * r + i * i

  fun {f32,f32} multComplex({f32,f32} x, {f32,f32} y) =
    let {a, b} = x
    let {c, d} = y
    in {a*c - b * d,
        a*d + b * c}

  fun {f32,f32} addComplex({f32,f32} x, {f32,f32} y) =
    let {a, b} = x
    let {c, d} = y
    in {a + c,
        b + d}

We can now define the core function that determines whether a given
point on the complex plane is part of the Mandelbrot set.  We do this
by defining a function ``divergence`` that returns the iteration at
which the loop diverges (or the limit, ``depth``, if it does not)::

  fun int divergence(int depth, {f32,f32} c0) =
    loop ({c, i} = {c0, 0}) = while i < depth && dot(c) < 4.0 do
      {addComplex(c0, multComplex(c, c)),
      i + 1}
    in i

The ``mandelbrot`` function returns the divergence point for the
complex number corresponding to a pixel in a given view of the complex
plane::

  fun [[int,screenX],screenY] mandelbrot(int screenX, int screenY, int depth,
                                         f32 xmin, f32 ymin, f32 xmax, f32 ymax) =
    let sizex = xmax - xmin
    let sizey = ymax - ymin
    in map(fn [int,screenX] (int y) =>
             map (fn int (int x) =>
                    let c0 = {xmin + (f32(x) * sizex) / f32(screenX),
                              ymin + (f32(y) * sizey) / f32(screenY)}
                    in divergence(depth, c0),
                  iota(screenX)),
           iota(screenY))

Given the point of divergence for a pixel, we can decide on a colour,
which is encoded as RGB within a 32-bit integer (the alpha channel is
not used)::

  fun int escapeToColour(int depth, int divergence) =
    if depth == divergence-1
    then 0xFF0000
    else
      let r = 3 * divergence
      let g = 5 * divergence
      let b = 7 * divergence
      in 0xFFFFFFFF - (r<<16 | g<<8 | b)

Finally we tie it all together - the ``main`` function computes the
point of divergence for each pixel, then colours them::

  fun [[int,screenX],screenY] main(int screenX, int screenY, int depth,
                                   f32 xmin, f32 ymin, f32 xmax, f32 ymax) =
    let escapes = mandelbrot(screenX, screenY, depth, xmin, ymin, xmax, ymax)
    in map(fn [int,screenX] ([int] row) =>
             map(escapeToColour(depth), row),
           escapes)

We can test our code by compiling it to a standalone program::

  $ futhark-pyopencl mandelbrot.fut
  $ echo 3 2 255 -2.23 -1.15 0.83 1.15 | ./mandelbrot
  [[-1i32, -395791i32, -593686i32], [-1i32, -50200570i32, -50200570i32]]

Of course, it is not very satisfying to look at fractals as arrays of
numerically encoded pixel values.  Hence, we pass ``--module`` to
``futhark-pyopencl``::

  $ futhark-pyopencl --module mandelbrot.fut

This produces a file ``mandelbrot.py`` defining a single Python class
``mandelbrot``, which we can access from ordinary Python code, as
shown below.

`mandelbrot-visualise.py </static/mandelbrot-visualise.py>`_
------------------------------------------------------------

We will need to import a PNG encoder, Numpy, and of course the module
produced by ``futhark-pyopencl``::

  import png
  import numpy
  from mandelbrot import mandelbrot

Then we create an instance of the class ``mandelbrot``::

  m = mandelbrot()

The constructor may take additional arguments specifying which OpenCL
platform and device to use, as well as other configuration parameters.
The class defines a single method, ``main``, corresponding to the main
function of the Futhark program.  We define a handful of constants
which we pass to the method::

  filename='mandelbrot.png'
  width=800
  height=600
  limit=255
  minx=-2.23
  miny=-1.15
  maxx=0.83
  maxy=1.15
  fut_image=m.main(width, height, limit, minx, miny, maxx, maxy)

The result value, which we store in the variable ``fut_image``.  Since
we declared the return type of ``main`` to be
``[[int,screenX],screenY]``, the returned value will be a
two-dimensional Numpy array of shape ``(width,height)``.  We cannot
pass this directly to the ``png`` library, as it expects a
three-dimensional array explicitly encoding the different colour
channels.  Fortunately, this array transformation is easy to do with
Numpy::

  image=numpy.empty((height,width,3))
  image[:,:,0] = (fut_image & 0xFF0000) >> 16
  image[:,:,1] = (fut_image & 0xFF00) >> 8
  image[:,:,2] = (fut_image & 0xFF)

And now we can simply invoke the ``png`` library::

  w = png.Writer(width, height, greyscale=False, alpha=False, bitdepth=8)
  with open(filename, 'wb') as f:
    w.write(f, numpy.reshape(image, (height, width*3)))

The result is this moderately attractive fractal in the file
``mandelbrot.png``:

.. image:: /images/mandelbrot-opencl.png
   :alt: Mandelbrot fractal produced by PyOpencL
   :class: centre

A slightly more elaborate Python program, which supports command-line
parameters and reports timing, can be found `here
<https://github.com/HIPERFIT/futhark-benchmarks/tree/master/accelerate/mandelbrot>`_.
We also have an `implementation of Game of Life
<https://github.com/HIPERFIT/futhark-benchmarks/tree/master/misc/life>`_
that uses `Pygame <http://www.pygame.org/>`_ to render the ongoing
simulation.
