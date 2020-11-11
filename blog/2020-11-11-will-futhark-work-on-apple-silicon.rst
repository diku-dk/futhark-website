---
title: Will Futhark work on Apple Silicon?
author: Troels Henriksen
description: People write these posts for all kinds of languages these days, so I guess we should too.
---

Apple is coming out with computers that are basically ARM64, `but with
a different ABI than existing ARM64 for some reason
<https://en.wikipedia.org/wiki/Think_different>`_.  They call the
architecture `Apple Silicon
<https://en.wikipedia.org/wiki/Mac_transition_to_Apple_Silicon>`_,
which is a wonderful term that will undoubtedly never become dated or
insufficiently precise.

Anyway, you see various posts such `Will R Work on Apple Silicon?
<https://developer.r-project.org/Blog/public/2020/11/02/will-r-work-on-apple-silicon/>`_
where language developers answer whether their language will work on
these new machines.  Since these machines will support transparent
emulation of x86, the simple answer is *yes*.  Apple's emulation was
quite good during the PPC-to-x86 transition, so this is trustworthy.
Of course, emulation is never going to be as fast as native
compilation.  For R, the problem is that they depend on some Fortran
code, and there is `not yet a Fortran compiler available for Apple
Silicon <https://developer.apple.com/forums/thread/651476>`_.

Well, I can confirm that Futhark depends on absolutely no Fortran.
Futhark compiles to C (or Python), and does not care about the
specific target architecture.  Therefore, Futhark programs should run
fine on Apple Silicon.  The bigger problem is that `Apple has
deprecated OpenCL
<https://www.extremetech.com/computing/270902-apple-defends-killing-opengl-opencl-as-developers-threaten-revolt>`_,
and `does not support CUDA at all
<https://www.provideocoalition.com/officially-official-nvidia-drops-cuda-support-for-macos/>`_
due to being grumpy with NVIDIA, so there may eventually be no way to
run Futhark *on a GPU* on macOS.  It is unlikely that we will find the
time to add a backend for Apple's `proprietary Metal API
<https://developer.apple.com/metal/>`_ that is supported *absolutely
nowhere else*, but it's possible that we'll finish Futhark's embryonic
Vulkan backend.  While macOS does not come bundled with Vulkan, `even
Apple probably cannot hold back this eruption
<https://github.com/KhronosGroup/MoltenVK>`_.

And of course, the Futhark multi-core backend should run well on any
Unix-like system.
