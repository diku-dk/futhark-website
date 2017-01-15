---
title: A Port of Falling Turnip to Futhark
author: Troels Henriksen
description: Falling Turnip is a falling-sand style physics playground implemented using Haskell and the Repa library.  I ported this program to Futhark and gave it a Pygame-based frontend.
---

Some time ago, I came across a cool program written by Tran Ma:
`Falling Turnip`_.  It is a physics imitation that uses cellular
automata to implement facsimiles of gravity and (al)chemical
interactions between various elements.  For example, water plus fire
gives steam, lava melts sand and metal (the latter at a slower rate),
plants grow into water, etc.  I became smitten by the `cool demo
video`_, and resolved that *so ein Ding muss ich auch haben*.  Falling
Turnip is written in Haskell, and uses the `Repa`_ library to work
with efficient parallel arrays.  The algorithm used (which I will
describe shortly) is embarrassingly parallel - essentially a rank-1
stencil at its core - and seemed expressible in Futhark.  This program
isn't be a benchmark as such (there is no measurable reference), but
is just be something fun to play around with.  The result is on Github
under the name `Diving Beet`_.  In contrast to Falling Turnip, which
runs solely on CPU(s), Diving Beet runs on any platform supported by
OpenCL (although I've only run it on GPUs).  The frontend is written
in Python, using Pygame, via the technique described in an `earlier
blog post`_.

.. _`Falling Turnip`: https://github.com/tranma/falling-turnip
.. _`cool demo video`: https://www.youtube.com/watch?v=hlL9yi2hGx0&feature=youtu.be
.. _`Repa`: https://hackage.haskell.org/package/repa
.. _`Diving Beet`: https://github.com/Athas/diving-beet
.. _`earlier blog post`: /blog/2016-04-25-futhark-and-pygame.html

Algorithm
---------

The intuition behind the algorithm is pretty simple.  The world is a
discrete 2D grid of particles of different kinds.  Empty space is also
treated as a special "empty" particle.  Each particle can interact
with its neighbours, transforming the neighbour or itself.  For example,
if a heavy particle is above a lighter particle, we swap them, to
simulate gravity.  Or if water is above fire, the fire is extinguished
and the water turned into steam.  These interaction rules can be
elegantly expressed as a cellular automaton, as known from the `Game
of Life`_, although somewhat more complicated.  One problem is how to
implement the rules to preserve the physical property of conservation.
This is not an issue in Game of Life, because particles may appear or
disappear, but in a physical simulation, particles should not
disappear for no reason.

.. _`Game of Life`: https://bitstorm.org/gameoflife/

The solution is to use a *block cellular automaton*, where the 2D grid
is partitioned into non-overlapping 2x2 cells, called *Margolus
neighbourhoods* that shift along both axes for every time step.  Each
neighbourhood is then transformed based on the chemical and physical
rules that we have defined.  In principle, we could assign each
neighbourhood to a thread, but the approach chosen by Falling Turnip,
and which we replicate in Diving Beet, is to assign a thread for each
particle, which then redundantly computes an entire new neighbourhood
state, only to pick out just the single new element that the thread is
responsible for.

The block cellular automaton has some weaknesses - for example,
information propagates only locally (radius one), so a solid mass of
particles will fall from the bottom up, with the topmost particles
only beginning to drop when there is empty space beneath them.  The
phenomenon looks like this:

.. raw:: html

   <video src="/static/falling-sand-2016.12.04.webm"
          controls
          width="480" height="270">
     Sorry, your browser doesn't support embedded WebM videos, but
     don't worry, you can <a
     href="/static/falling-sand-2016.12.04.webm">download it</a> and watch it
     with a compatible video player!
   </video>
   <p style="font-style: italic; margin-top: 0;">Direct link: <a href="/static/falling-sand-2016.12.04.webm">falling-sand-2016.12.04.webm</a></p>

The port from Falling Turnip was quite simple.  One interesting
complication was the need to generate random numbers.  Some
interaction rules have a random chance of applying.  For example, lava
in contact with metal has a small chance of melting the metal, while
lava in contact with sand has a large chance of melting the sand.  In
Falling Turnip, this is implemented by generating a random Repa array.
Futhark is a pure language, and thus cannot produce true randomness.
However, we can get close enough by running a few iterations of a
simple hash function, feeding it a mixture of element index and step
count::

  -- From http://stackoverflow.com/a/12996028
  fun hash (x: i32): i32 =
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) in
    x

  fun rand_array (seed: i32) (n: i32) (lower: i32, upper: i32): [n]i32 =
    map (fn (i: i32): i32  =>
          -- We hash i+n to ensure that a random length-n array is not a
          -- prefix of a random length-(n+m) array.
          (hash (seed ^ i+n)) % (upper-lower+1) + lower) (iota n)

This is good enough for simulations, even if a cryptographer would be
horrified.

One other interesting part of the implementation is the `main chemical
interaction function`_, which consists of a long chain of simple
``if-then-else`` expressions.  The ability to have such free-form
control flow, even inside of parallel code, was crucial when it came
to phrasing the interaction rules in a clear and understandable
manner.  In contrast, to write this in APL (a very old array
language), you would have to express the control flow as data, usually
by generating all possible results, then multiplying with either ones
or zeroes based on whether each branch was supposed to be taken.
There was an attempt to port Falling Turnip to Accelerate, a Haskell
eDSL for GPU programming, but it `seems to have been aborted`_.  I
suspect the need for deeply nested control flow is related, although
Accelerate does have some `branching constructs`_.  The design space
of functional array languages is still fairly unexplored, so I am very
curious about exactly which challenges were encountered.

.. _`main chemical interaction function`: https://github.com/Athas/diving-beet/blob/master/alchemy.fut
.. _Accelerate: https://hackage.haskell.org/package/accelerate
.. _`seems to have been aborted`: https://github.com/tranma/falling-turnip/issues/3#issuecomment-144244558
.. _`branching constructs`: https://hackage.haskell.org/package/accelerate-0.15.1.0/docs/Data-Array-Accelerate.html#v:acond

Video
-----

The primary motivation behind this work was of course to have
something pretty to show off when I demo Futhark.  A video of me
playing around with Diving Beet running on an NVIDIA GTX 680 GPU is
available `here`_.  In the video, I create some spouts to generate
water, then create torches beneath them that create fire.  When fire
and water meets, steam arises, and eventually condenses and turns to
water again.  I then insert plants (the green stuff), which grows into
the water and eventually comes into contact with fire, upon which the
it too bursts into fire, sometimes leaving behind a remnant of sand.

.. _`here`: http://sigkill.dk/junk/diving-beet.webm

On the right-hand side, I create some wedges of metal onto which I
drop lava.  The lava pools and slowly eats into the metal, while I
drop oil on top to create some pretty flames.  Near the end, the video
starts getting choppy because my screen recorder was unable to keep up
and had to write to disk.  I create a line of (unreactive) wall, some
torches, and then some water to show off the condensation effects more
clearly.

Future
------

I am quite pleased with Diving Beet so far.  It was fairly simple to
port to Futhark, and the result runs quite well on GPUs.  I never knew
cellular automatons could be this fun!  Yet, the model I ported from
Falling Turnip is still pretty simple, and there are some things I
would like to improve:

  * Temperature.  Currently, fire is faked by having several different
    kinds of particles model fire in various stages of fading.  It
    would be nice if each particle had its own notion of temperature,
    which would spread through materials at different speeds, and have
    an effect once it reached some level.  For example, water could
    boil and freeze based on the temperature, and lava would
    eventually cool into stone (not just when it reaches water).

  * Some notion of momentum, which could also be used to represent
    wind.  This is slightly complicated by the fact that the block
    cellular model restricts particles to moving one cell for every
    time step.

  * Gravity-based pressure, such that a huge pile of sand on top of a
    thin metal line might cause the line to disintegrate.  I am
    primarily interested in this because the computational problem
    would be a segmented scan along the columns of the grid, which
    would show off Futhark's parallel capabilities better than just
    having a fairly simple block cellular automaton.

  * Bugfixing.  In the video, you may have noticed a few columns of
    sand dropping some some seemingly inexhaustible source.  I am
    pretty sure this is due to a bug in the gravity rules, although
    they seem identical to the ones in Falling Turnip.
