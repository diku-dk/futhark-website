---
title: Creating Interactive Futhark GUIs
author: Niels G. W. Serup
description: Using PyGame, NumPy, and PyOpenCL to interact with Futhark programs in simple graphical user interfaces.
---

Futhark is a functional language which can be compiled into efficient
GPU code.  See our `performance page </performance.html>`_ for
details.  Futhark is a pretty specialised language: You can express
complex programs using, among other things, Futhark's `second-order
array combinators
<https://futhark.readthedocs.org/en/latest/language-overview.html#soacs>`_,
but the programs cannot themselves be interactive in any way; in
Futhark you can *only* describe computations which take an input,
return an output, and do not have side effects.  However, since you
might want to add an interactive touch to your Futhark program (we
do!), we have developed a code generator for generating Python
modules.  A Futhark-generated Python module can be imported into any
Python program, provided you have NumPy and PyOpenCL installed, and
can then be used for the computing-intensive part of a GUI, while the
main Python file is used for everything else.

In `the previous blog post
</blog/2016-04-15-futhark-and-pyopencl.html>`_, Troels showed you how
to use Futhark's PyOpenCL backend to generate PNG files with Python
and NumPy.  There was also a short mention of *interactive* interfaces
to Futhark programs.  In this blog post I will demonstrate how to use
PyGame and Python to create those interactive visual interfaces.  The
concept is largely the same as in the previous post, but the
visualisations are prettier.

`PyGame <http://www.pygame.org/>`_ is a Python library for the SDL 1.2
graphics library.  Although a Python 3 port does exist, for
compatibility reasons we use the original Python 2 library.
Library-savvy readers might point out that a more modern library like
pyglet or PySDL2 might be more... modern, but we really only require
few features, so good old PyGame is sufficient for these basic
examples.

We currently have four GUI examples:

  + `Mandelbrot Explorer <https://github.com/diku-dk/futhark-benchmarks/tree/master/accelerate/mandelbrot>`_
  + `Fluid Simulation <https://github.com/diku-dk/futhark-benchmarks/tree/master/accelerate/fluid>`_
  + `Game of Life <https://github.com/diku-dk/futhark-benchmarks/tree/master/misc/life>`_
  + `HotSpot 2D Heat Equation <https://github.com/diku-dk/futhark-benchmarks/tree/master/rodinia/hotspot>`_

It is pretty easy to create an interactive visualisation with Futhark,
so expect this number to go up.  However, note that Futhark
development is not primarily oriented towards graphics programming --
we just want to show off what we have!


A Simple Start: Game of Life
----------------------------

Let us start with Futhark's own Game of Life visualisation.  This is
the simplest of the four visualisations, as it is actually not
interactive.  However, it shows how few lines of code are needed to
integrate Futhark, NumPy, and PyGame.

First, here is a video of me running the visualisation!

.. raw:: html

   <video src="/static/life-2016.04.20.webm"
          poster="/static/life-2016.04.20-poster.jpg"
          controls
          width="480" height="270">
     Sorry, your browser doesn't support embedded WebM videos, but
     don't worry, you can <a
     href="/static/life-2016.04.20.webm">download it</a> and watch it
     with a compatible video player!
   </video>
   <p style="font-style: italic; margin-top: 0;">Direct link: <a href="/static/life-2016.04.20.webm">life-2016.04.20.webm</a></p>

Secondly, here is how it works: `life-gui.py
<https://github.com/diku-dk/futhark-benchmarks/blob/master/misc/life/life-gui.py>`_
first imports our three Game of Life variants::

  import life
  import quadlife
  import quadlife_alt

These Python modules are generated from the Futhark programs `life.fut
<https://github.com/diku-dk/futhark-benchmarks/blob/master/misc/life/life.fut>`_,
`quadlife.fut
<https://github.com/diku-dk/futhark-benchmarks/blob/master/misc/life/quadlife.fut>`_,
and `quadlife_alt.fut
<https://github.com/diku-dk/futhark-benchmarks/blob/master/misc/life/quadlife_alt.fut>`_
by means of the ``futhark-pyopencl`` code generator.  ``life`` uses the
usual Game of Life rules, while ``quadlife`` and ``quadlife_alt`` use
rules invented by `Torben Mogensen <http://www.diku.dk/~torbenm/>`_.

It then sets up PyGame::

  screen = pygame.display.set_mode(size)

where ``size`` is a tuple of ``(width, height)``.  We then generate a
NumPy array of random initial states::

  initworld = numpy.random.choice([True,False], size=size)

A NumPy array can be used as an argument to an entry point function in
a Futhark-generated Python module.  In this case, we use the NumPy
array to get the initial simulation values by calling::

  world, history = l.init(initworld)

Here, the ``l`` variable is the imported simulation backend,
i.e. either ``life``, ``quadlife``, or ``quadlife_alt``.  You can
specify the backend with the ``--variant`` command-line flag.  The
``l.init`` function is a Futhark function that takes a two-dimensional
``bool`` array and returns a two-dimensional ``bool`` array and a
two-dimensional ``i32`` array.

We also need a temporary PyGame surface for transferring pixel data
from NumPy to PyGame, so we run this::

  surface = pygame.Surface(size)

To render a frame, we use the temporary ``surface`` surface to
transform the returned NumPy array into something PyGame can
understand, after which we blit it to the screen::

  def render():
      frame = l.render_frame(history)
      pygame.surfarray.blit_array(surface, frame.get())
      screen.blit(surface, (0, 0))
      pygame.display.flip()

Here, ``l.render_frame`` is a Futhark function that takes a
two-dimensional ``i32`` array and returns a three-dimensional pixel
array in the colour format expected by PyGame.

As the blit array we need to use ``frame.get()`` and not just
``frame``.  This is because the Futhark-compiled Python module gives
us a PyOpenCL array, which is mostly compatible with NumPy, but not
fully -- and Pygame really wants a proper NumPy array, so we use the
``get()`` method to obtain that.
  
Finally, since Game of Life is a state-based simulation, we need a way
to step through the simulation, using previous output as new input.
This is pretty simple in Python::

  while True:
      world, history = l.steps(world, history, steps)
      render()
      for event in pygame.event.get():
          if event.type == pygame.QUIT:
              sys.exit()

The ``steps`` argument is the number of simulation steps to perform
per frame, and defaults to 3.  You can set this to any positive 32-bit
i32.  To increase the work done per frame, we have set the default to
3 and not e.g. 1.  This choice reflects possible real-world use, where
we might not care about having a real-time visualisation of a
simulation, but just use the visualisation to track progress, and thus
ask the Futhark program to perform large chunks of work at a time, and
update the display fairly rarely.

We have also added a simple PyGame event check, so that you can close
the simulation window as expected.


The Three Other Ones
--------------------

In the fluid simulator you can add both particles and forces.  See for yourself:

.. raw:: html

   <video src="/static/fluid-2016.04.20.webm"
          poster="/static/fluid-2016.04.20-poster.jpg"
          controls
          width="480" height="270">
     Sorry, your browser doesn't support embedded WebM videos, but
     don't worry, you can <a
     href="/static/fluid-2016.04.20.webm">download it</a> and watch it
     with a compatible video player!
   </video>
   <p style="font-style: italic; margin-top: 0;">Direct link: <a href="/static/fluid-2016.04.20.webm">fluid-2016.04.20.webm</a></p>

My laptop's GPU (a nVidia GT 650 M) is not the newest one around, so I
am running this in a fairly small window to avoid too much lag.

The Mandelbrot Explorer is also pretty nifty.  Note that this
implementation re-renders the entire visible region from scratch for
every frame.  This would likely be too slow if it was not
GPU-accelerated.

.. raw:: html

   <video src="/static/mandelbrot-2016.04.20.webm"
          poster="/static/mandelbrot-2016.04.20-poster.jpg"
          controls
          width="480" height="270">
     Sorry, your browser doesn't support embedded WebM videos, but
     don't worry, you can <a
     href="/static/mandelbrot-2016.04.20.webm">download it</a> and watch it
     with a compatible video player!
   </video>
   <p style="font-style: italic; margin-top: 0;">Direct link: <a href="/static/mandelbrot-2016.04.20.webm">mandelbrot-2016.04.20.webm</a></p>

In the end of the video, I switch to a Mandelbrot implementation
written in pure NumPy (also included in the benchmarks repository).
You can also check out the `Mandelbrot performance numbers
</performance.html#mandelbrot-futhark-thrust-accelerate>`_.

Finally, there is the HotSpot 2D Heat Equation GUI.  You can see its
performance and a description of what it is computing `here
</performance.html#hotspot-futhark-rodinia>`_.  This visualisation is
pretty silly, since every marked pixel gets the same power output
level.  The initial heat levels are random and take a while to
dissipate, which is why the simulation spends quite some time before
the generated graphics resemble the originally drawn graphics.

.. raw:: html

   <video src="/static/hotspot-2016.04.20.webm"
          poster="/static/hotspot-2016.04.20-poster.jpg"
          controls
          width="480" height="270">
     Sorry, your browser doesn't support embedded WebM videos, but
     don't worry, you can <a
     href="/static/hotspot-2016.04.20.webm">download it</a> and watch it
     with a compatible video player!
   </video>
   <p style="font-style: italic; margin-top: 0;">Direct link: <a href="/static/hotspot-2016.04.20.webm">hotspot-2016.04.20.webm</a></p>


More on Arrays
--------------

Every array in a Futhark-compiled Python module is of type
`pyopencl.array.Array
<https://documen.tician.de/pyopencl/array.html#the-array-class>`_,
which means that all data is stored on the compute device, in our case
the GPU.  This enables GPU-stored arrays to be passed to and from a
Python program without copying between CPU and GPU.

If you pass an array which *is not* a PyOpenCL array, but *is*
NumPy-compatible, the generated Python code will automatically convert
it into a PyOpenCL array by transferring its data to the compute
device.  This means that you can use NumPy to construct e.g. the
initial arrays of a simulation, and not worry about the finer details.

         
Tips
----

Futhark is an optimising compiler which takes an *entire program* as
input.  As such, its optimisations are not directed at separate
functions, but rather the program as a whole.  This is in stark
contrast to how computing libraries, e.g. NumPy, usually work.  They
consist of many primitive functions, and expect the programmer to
structure them together using the host language, in this case Python.

Also, for every call to a function in a Futhark Python module, Python
causes some overhead, which is another reason to have few calls to
Futhark and much code in Futhark.


Try them for yourself!
----------------------

If you install the Futhark compiler (and PyOpenCL, NumPy, and PyGame),
you should be able to compile and run all of the four GUI examples.
First run::

  git clone https://github.com/diku-dk/futhark-benchmarks.git

This will download all of Futhark's benchmarks.  Then for each of the
four interactive examples, ``cd`` into its directory, run ``make``,
and then follow the local README to run the GUI.

However, if you do not have the patience required to install Futhark
(and GHC), we have manually pre-compiled the current versions of the
four programs into Python for you with the ``futhark-pyopencl`` code
generator.  Download `futhark-guis-v0.1.tar.gz
</static/futhark-guis-v0.1.tar.gz>`_.  This has only been tested on a
Debian, so run at your own risk.  You still need to have PyOpenCL,
NumPy, and PyGame installed.


Write your own!
---------------

Do you have an idea for a computing-intensive program well suited for
interactive use?  If you can think of something, or even want to try
your hand at implementing it, please `contribute
</getinvolved.html>`_!
