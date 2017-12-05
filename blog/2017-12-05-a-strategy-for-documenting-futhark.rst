---
title: A strategy for documenting Futhark
author: Troels Henriksen
description: A discussion of our plans for how to structure Futhark's current and future documentation.
---

We have the vain hope that, some day, Futhark will be more than just a
vehicle for writing research papers about compiler optimisations.
However, if we want our work to be useful in practice, it is not
enough to simply write an optimising compiler - we also need to write
comprehensive documentation so people know how to use it!  I have no
illusions about the Futhark team being intrinsically gifted
documentation writers, and our team is furthermore so small that we
need to maximise the return on our time investment.  One way to do so
is to look at how other programming languages manage their
documentation.  One language that we often look to for inspiration is
`Rust`_, as they tend to make good decisions.

.. _`Rust`: https://www.rust-lang.org/en-US/

However, merely cargo-culting Rust would not be very satisfying.  We
would also like to understand the *essential purposes* of
documentation, so that our efforts can be more focused.  Fortunately,
Daniele Procida already wrote an article on the subject, titled `What
nobody tells you about documentation`_, where he characterises
documentation by four different *functions*: tutorials, how-tos,
explanations, and references.  While there is plenty of room to
disagree with this model (personally, I'd say there is apparently a
need for some theory explaining why light grey text on a white
background is a terrible idea), it seems a useful starting point to
thinking about documentation in a structured way.

.. _`What nobody tells you about documentation`:
   https://www.divio.com/en/blog/documentation/

To expand a little, the four functions and their orientations, are:

**Tutorials**

  Learning-oriented and targeted at beginning users who do not yet
  even know *what* to learn, or which questions to ask.  A tutorial
  needs to hold the hand of the user and show them something they have
  not seen before.

**How-to guides**

  This is goal-oriented documentation sought out by users who already
  know *what* they want to accomplish, but not *how* to do it.

**References**

  Information-oriented documentation that describes machinery in
  detail, but without focusing on specific uses.  Accuracy and
  coverage is paramount.

**Explanations**

  Understanding-oriented, in-depth documentation that focuses on
  background, context, and theory.

For Futhark, the `user's guide`_ provides a decent reference on how
the language and compilers work, but it does not (and should not)
fulfil the role of tutorial, how-to guide, or explanation.  Instead,
some of these roles should be filled by the in-progress book,
`Parallel Programming in Futhark`_.  In particular, if we model its
structure on the `Rust book`_, it seems like it will essentially
consist of repeated switching between tutorial and explanation
segments.  Intuitively, this also seems like a good structure: first
describe something concrete ("here is how to use ``map``"), followed
by deeper reflection on what ``map`` *is* (such as an introduction to
parallel cost models, or the fusion optimisation).  In fact, such a
refactoring of the book would solve one of the problems with its
current form, which consists essentially of a verbose language
reference, followed by a handful of chapters on the theory of data
parallelism.  These chapters contain useful information, but they are
currently too detached from the discussion about the language itself.

.. _`user's guide`: http://futhark.readthedocs.io
.. _`Parallel Programming in Futhark`: http://futhark-book.readthedocs.io
.. _`Rust book`: https://doc.rust-lang.org/book/second-edition/

The structure outlined above still leaves one documentation function
unaddressed: how-to guides.  These can be written in the form of
appendices tacked onto either the book or the user's guide, or perhaps
even as an extended FAQ on this very website.
