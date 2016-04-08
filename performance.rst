---
title: Performance
---

On this page, lower is better.

.. image:: images/mss.svg
   :alt: MSS runtime (lower is better)

This graph shows performance of a *maximum segment sum* implementation
in Futhark and Thrust.  The sequential runtime is for Futhark code
compiled to sequential CPU code.  This example is interesting because
it involves a non-commutative reduction.  In Thrust, we have to
implement this via an inclusive scan followed by picking out the last
element.

It's fast.  I'll put some graphs here or whatever.
