---
title: Beginning a collection of Futhark examples
author: Troels Henriksen
description: All the cool languages are writing collections of examples, so I guess we do the same and see if cargo shows up.
---

Websites like `Go by Example <https://gobyexample.com/>`_ and `Rust by
Example <https://doc.rust-lang.org/rust-by-example/index.html>`_ seem
to be pretty popular.  They are collections of typically small
self-contained programs, with commentary about how they work and what
they do.  I can see the appeal: for the author, it takes very little
effort to add content to such a document, because there is no cohesive
narrative to maintain.  For the reader, it requires very little time
investment to read a single example.  Also, for the curious visitor,
who is not (yet?) trying to actively learn the language, it provides a
good way for getting the *feel* of the language.

By the standards of obscure academic programming languages, I think
that Futhark is decently documented.  The `reference manual
<https://futhark.readthedocs.io/en/stable>`_ is terse but fairly comprehensive,
and `the unfinished book <https://futhark-lang.org>`_ contains a good
conventional language introduction, as well as some information about
tooling.  However, while I never had much trouble updating the
reference manual, I always felt the book was harder to improve in
small increments.  Every paragraph in a book must fit in some context,
and sometimes I just want to show off some small programming trick, or
elaborate on some detail.  That's exactly what a catalogue of examples
can do.  So, for the past week or so, I have been assembling `Futhark
by Example <../examples.html>`_, a yet-small
collection of Futhark code for doing various things.

As of this writing, most of the examples are still about basic
language constructs (which is not too dissimilar to Go by Example).
However, while these are necessary, that's not really where I think
the potential lies: the `radix sort <../examples/radix-sort.html>`_
and `gaussian blur <../examples/gaussian-blur.html>`_ examples are
much better examples of what I want.  I write new examples whenever I
think of something interesting, but if there's something you would
like to see, don't hesitate to `get in touch <../getinvolved.html>`_.

In order to make it as low-friction as possible for me to write
examples, they are just ordinary Futhark programs processed by the
`website generator
<https://github.com/diku-dk/futhark-website/blob/master/site.hs>`_ to
turn them into Markdown files, which are then translated to HTML.  For
example, `this is what gaussian-blur.fut looks like
<https://github.com/diku-dk/futhark-website/blob/master/examples/gaussian-blur.fut>`_.
It's a crude technique, and for examples like `this one
<../examples/basic-parallelism.html>`_ I'd prefer some automated way
of showing the values produces by intermediate bindings.  Maybe some
examples should be programs, and others should be ``futhark repl``
sessions?  I'm sure I will find some way to over-engineer the
infrastructure eventually, but for now I am satisfied just writing
content.
