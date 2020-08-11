---
title: Python gotta go faster
author: Pepijn de Vos
description: A higher-performance way to call Futhark from Python.
---

`*Crossposted from pepijndevos.nl* <http://pepijndevos.nl/2018/07/05/futhark-python-gotta-go-faster.html>`_

While discussing the disappointing performance of my `Futhark DCT <http://pepijndevos.nl/2018/07/04/loefflers-discrete-cosine-transform-algorithm-in-futhark.html>`_ on my "retro GPU" (Nvidia NVS 4200M) with Troels Henriksen, it came up that the Python backend has quite some calling overhead.

Futhark can compile high-level functional code to very fast OpenCL, but Futhark is meant to be embedded in larger programs. So it provides a host library in C and Python that set up the GPU, transfer the memory, and run the code. It turns out the the Python backend based on PyOpenCL is quite a bit slower at this than the C backend.

I wondered why the Python backend did not use the C one via FFI, and Troels mentioned that someone had done this for a specific program and saw modest performance gains. However, this does require a working compiler and OpenCL installation, rather than just a ``pip install PyOpenCL``, so he argued that PyOpenCL is the easiest solution for the average data scientist.

I figured I might be able to write a generic wrapper for the generated C code by feeding the generated header directly to CFFI. That worked on the first try, so that was nice. The hard part was writing a generic, yet efficient and Pythonic wrapper around the CFFI module.

The first proof of concept required quite a few fragile hacks (pattern matching on function names and relying on the type and number of arguments to infer other things) But it worked! My DCT ran over twice as fast. Then, Troels, helpful as always, modified the generated code to reduce the number of required hacks. He then proceeded to port some of the demos and benchmarks, request some features, and contribute Python 2 support.

`futhark-pycffi <https://github.com/pepijndevos/futhark-pycffi>`_ now supports all Futhark types on both Python 2 and 3, resulting in speedups of anywhere between 20% and 100% compared to the PyOpenCL backend. Programs that make many short calls benefit a lot, while programs that call large, long-running code benefit very little. The OpenCL code that runs is the same, only the calling overhead is reduced.

One interesting change suggested by Troels is to not automatically convert Futhark to Python types. For my use case I just wanted to take a Numpy array, pass it to Futhark, and get a Numpy array back. But for a lot of other programs, the Futhark types are passed between functions unchanged, so not copying them between the GPU and CPU saves *a lot* of time. There is even a compatibility shim that lets you use futhark-ffi with existing PyOpenCL code by merely changing the imports. An example of this can be seen `here <https://github.com/diku-dk/futhark-benchmarks/blob/e36913a4b76477526abc214488b38fa8466bda05/accelerate/fluid/fluid-gui.py#L13-L23>`_.

After `installing Futhark <https://futhark.readthedocs.io/en/stable/installation.html>`_, you can simply get my library with ``pip``. (working OpenCL required)::


  pip install futhark-ffi


Usage is as follows. First generate a C library, and build a Python binding for it::

  futhark-opencl --library test.fut
  build_futhark_ffi test

From there you can import both the CFFI-generated module and the library to run your Futhark code even faster!

.. code-block::

   import numpy as np
   import _test
   from futhark_ffi import Futhark

   test = Futhark(_test)
   res = test.test3(np.arange(10))
   test.from_futhark(res)
