---
title: Creating Interactive Futhark GUIs
author: Niels G. W. Serup
description: Using PyGame and PyOpenCL to interact with Futhark programs in simple graphical user interfaces.
---

In the previous blog post, Troels showed you how to use Futhark's
PyOpenCL backend to generate PNG files with Python and NumPy.  There
was also a short mention of *interactive* examples built with PyGame.
In this blog post we will demonstrate how to add interactive visual
interfaces to simulations implemented in Futhark.

`PyGame <http://www.pygame.org/>`_ is a Python library for the SDL 1.2
graphics framework.  Although a Python 3 port does exist, for
compatibility reasons we use the original Python 2 library.
Library-savvy readers might point out that a more modern library like
pyglet or PySDL2 might be more... modern, but we really only require
few features, so good old PyGame is sufficient for our basic examples.

We currently have four GUI examples:

  + `Mandelbrot Explorer <https://github.com/HIPERFIT/futhark-benchmarks/tree/master/misc/mandelbrot-explorer>`_
  + `Fluid Simulation GUI <https://github.com/HIPERFIT/futhark-benchmarks/tree/master/accelerate/fluid/gui>`_
  + `Game of Life <https://github.com/HIPERFIT/futhark-benchmarks/tree/master/misc/life>`_
  + `HotSpot 2D Heat Equation GUI <https://github.com/HIPERFIT/futhark-benchmarks/tree/master/rodinia/hotspot>`_

It is pretty easy to create an interactive visualisation with Futhark,
so expect this number to go up.  However, note that Futhark
development does *not* target interactive visualisations -- we just
want to show off what we have!


A Simple Start: Game of Life
----------------------------

Let us start with Futhark's own Game of Life visualisation.  This is
arguably the simplest of the four visualisations, as it is actually
not interactive.  However, it shows how few lines of code are needed
to integrate Futhark, NumPy, and PyGame.

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

Secondly, here is how it works: ``life-visualise.py`` first sets up
PyGame::

  screen = pygame.display.set_mode(size)

where ``size`` is a tuple of ``(width, height)``.  We then generate a
NumPy array of random initial states::

  initworld = numpy.random.choice([True, False], size=size)

A NumPy array can be used as an argument to an entry point function in
a Futhark Python module.  In this case, we use the NumPy array to get
the initial simulation values by calling::

  world, history = l.init(initworld)

We also need a temporary PyGame surface for transferring pixel data
from NumPy to PyGame, so we run this::

  surface = pygame.Surface(size)

To render a frame, we use the temporary ``surface`` surface to
transform the returned NumPy array into something PyGame can
understand, after which we blit it to the screen::

  def render():
      frame = l.render_frame(history)
      pygame.surfarray.blit_array(surface, frame)
      screen.blit(surface, (0, 0))
      pygame.display.flip()

Here, the ``l`` variable is the imported simulation backend,
i.e. either the default ``life`` or one of the alternative variants
``quadlife`` or ``quadlife_alt``, whose rules are invented by `Torben
Mogensen <http://www.diku.dk/~torbenm/>`_.  When running the
visualisation, you can specify the backend with the ``--variant``
command-line flag.
  
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
int.  The default is 3 and not 1 to increase the work done per frame.

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

My GPU is not the best one around, so I am running this in a fairly
small window.

The Mandelbrot Explorer is also pretty nifty:

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

In the end of the video, I switch to NumPy's internal Mandelbrot generator.

Finally, there is the HotSpot 2D Heat Equation GUI.  You can see its
performance `here </performance.html#hotspot-futhark-rodinia>`_.  This
visualisation is pretty silly.

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


Use Cases
---------

Futhark is an optimising compiler which takes an *entire program* as
input.  As such, its optimisations are not directed at separate
functions, but rather the program as a whole.  This is important to
keep in mind when developing Futhark-PyGame programs, since it means
that we would like to have as few calls as possible to Futhark from
Python, and keep as much code as possible inside Futhark.

This is in stark contrast to how computing libraries, e.g. NumPy,
usually work.  They consist of many primitive functions, and expect
the programmer to structure them together using the host language, in
this case Python.

In conclusion, The Futhark-PyGame combo is best when every step of the
visualisation -- i.e. every call to Futhark -- is compute-intensive.


Try them for yourself!
----------------------

If you install the Futhark compiler (and PyOpenCL, NumPy, and PyGame),
you should be able to compile and run all of the four GUI examples.
First run::

  git clone https://github.com/HIPERFIT/futhark-benchmarks.git

This will download all of Futhark's benchmarks.  Then for each of the
four interactive examples, ``cd`` into its directory, run ``make``,
and then follow the local README to run the GUI.

However, if you do not have the patience required to install Futhark
(and GHC), we have manually pre-compiled the current versions of the
four programs into Python with PyOpenCL for you.  Download
`futhark-guis-v0.1.tar.gz </static/futhark-guis-v0.1.tar.gz>`_.  This
has only been tested on a Debian, so run at your own risk.  You still
need to have PyOpenCL, NumPy, and PyGame installed.


Write your own!
---------------

Do you have an idea for a computing-intensive program well suited for
interactive use?  If you can think of something, or even want to try
your hand at implementing it, please `contribute
</getinvolved.html>`_!
