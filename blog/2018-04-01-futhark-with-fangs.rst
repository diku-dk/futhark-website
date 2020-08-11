---
title: Futhark with Fangs!
author: Troels Henriksen
description: The first web framework (sort of) for Futhark.  Also an attempt at bringing us into compliance with Wirth's Law.
---

Any fledgling language designer must consider how to make their
language useful to someone, and thus gain users.  `Last year`_, the
Futhark outreach efforts were focused on catering to an overlooked
niche - computational history - but this niche may have been just a
tad too narrow.  In fact, I am still not sure whether it exists at
all.  Therefore, this year, we will change our strategy to focus on
the most mainstream development field in the world: web programming.
This field is characterised by two key properties:

  * Web programmers are conditioned to have a very high pain threshold
    when it comes to complexity papered over by tools.

  * Sheer novelty seems to be a selling point by itself.

This analysis inspired me to implement support for calling Futhark
functions via HTTP.  Futhark already has a `pretty good Python
interface`_, so it was a quick hack to wire it up to the standard
`http.server` module.  The result: `Futhark with Fangs`_.

.. _`Last year`: /blog/2017-04-01-roman-numerals.html
.. _`pretty good Python interface`: /blog/2016-04-15-futhark-and-pyopencl.html
.. _`http.server`: https://docs.python.org/3/library/http.server.html
.. _`Futhark with Fangs`: https://github.com/diku-dk/futhark-with-fangs

Using Futhark with Fangs
------------------------

Putting Futhark on the web is quite easy.  First we define a couple of
interesting Futhark functions in a file ``futapp.fut``:

.. code-block:: Futhark

  entry dotprod (xs: []i32) (ys: []i32) = reduce (+) 0 (map2 (+) xs ys)

  entry sumrows (xss: [][]i32) = map (reduce (+) 0) xss

We then compile it to a Futhark library with embedded GPU code::

  $ futhark-pyopencl --library futapp.fut

And then we simply pass the module name to Futhark with Fangs::

  $ ./futhark_with_fangs.py futapp

We can now invoke the Futhark entry points using our favourite HTTP
client::

  $ echo '[1,2] [3,4]' | curl -X POST --data-binary @- localhost:8000/dotprod
  10i32
  $ echo '[[1,2], [3,4]]' | curl -X POST --data-binary @- localhost:8000/sumrows
  [3i32, 7i32]

The input and output values are represented in the soon-to-be industry
standard Futhark value notation.  However, we can also pass the input
in a somewhat more efficient `binary data format`_.  It is a little
tricky to write in a shell, so we will generate some random data with
`futhark-dataset`_::

  $ futhark-dataset --binary -g [2][2]i32 | curl -X POST --data-binary @- localhost:8001/sumrows
  [-807407905i32, 1953530750i32]

.. _`binary data format`: http://futhark.readthedocs.io/en/stable/binary-data-format.html
.. _`futhark-dataset`: http://futhark.readthedocs.io/en/stable/man/futhark-dataset.html

Transmitting large arrays over HTTP is of course rather slow.  A good
idea would therefore be to use compression to cut the transmission
times.  So, does Futhark with Fangs support `HTTP compression`_ with
something like gzip?  I don't know, as I did not read the
``http.server`` docs closely enough.  Maybe it does; we will just have
to wait and see.  This seems in the spirit of dynamically typed
programming languages.

.. `HTTP compression`: https://en.wikipedia.org/wiki/HTTP_compression

The legal perspective
---------------------

Functional programmers, of which I am one, care a lot about laws.  One
of these, `Wirth's Law`_, states roughly:

    "Software is getting slower more rapidly than hardware is getting
    faster."

    -- Niklaus Wirth

.. `Wirth's Law`: https://en.wikipedia.org/wiki/Wirth%27s_law

GPUs, with their tremendous computational performance, are already
terribly close to breaking this law, and I would rather not see
Futhark become accessory to a crime.  Compiling Futhark to Python is a
valiant effort to reign in the performance a bit, but it still runs
quite fast, as most of the work remains on the GPU.  Hobbling Futhark
with HTTP may be just what we need to obtain compliance with Wirth's
Law.

Is this a joke?
---------------

Check the date of the post.  However, do note that Futhark with Fangs
is fully operational and works quite well for what it is.  Perhaps
Stockholm Syndrome is setting in, but it even feels like it might
actually be useful in some cases.  For problems that contain a large
ratio of computation to I/O, which includes most Monte Carlo
simulations, the network overhead may be negligible.  So at this
point, I'm not even sure myself whether Futhark on Fangs is supposed
to be a prank.  I guess the joke is on me.
