-- ---
-- title: Complex numbers
-- ---
--
-- Futhark does not have complex numbers, but they are fairly easy to
-- implement as a module.  First we [define a module
-- type](abstract-data-types.html) describing the interface of complex
-- numbers:

module type complex = {
  -- | The type of the components of the complex number.
  type real
  -- | The type of complex numbers.
  type complex

  -- | Construct a complex number from real and imaginary components.
  val mk: real -> real -> complex
  -- | Construct a complex number from just the real component.  The
  -- imaginary part will be zero.
  val mk_re: real -> complex
  -- | Construct a complex number from just the imaginary component.  The
  -- real part will be zero.
  val mk_im: real -> complex

  -- | Conjugate a complex number.
  val conj: complex -> complex
  -- | The real part of a complex number.
  val re: complex -> real
  -- | The imaginary part of a complex number.
  val im: complex -> real

  -- | The magnitude (or modulus, or absolute value) of a complex number.
  val mag: complex -> real
  -- | The argument (or phase) of a complex number.
  val arg: complex -> real

  val +: complex -> complex -> complex
  val -: complex -> complex -> complex
  val *: complex -> complex -> complex
  val /: complex -> complex -> complex

  val sqrt: complex -> complex
  val exp: complex -> complex
  val log: complex -> complex
}

-- Then we define a parametric module that given a `real` module (in
-- practice `f32` or `f64`) will give us back a complex number module.
-- We use *local opens* to perform operations using the operators from
-- the `T` module.  For examle, `T.(x + y)` means the same as `x T.+
-- y`, but is more readable.  The definitions here are straight out of
-- a textbook, so we won't elaborate them.

module mk_complex (T: real): (complex with real = T.t
                                      with complex = (T.t, T.t)) = {
  type real = T.t
  type complex = (T.t, T.t)

  let mk (a: real) (b: real) = (a,b)
  let mk_re (a: real) = (a, T.i32 0)
  let mk_im (b: real) = (T.i32 0, b)

  let conj ((a,b): complex) = T.((a, i32 0 - b))
  let re ((a,_b): complex) = a
  let im ((_a,b): complex) = b

  let ((a,b): complex) + ((c,d): complex) = T.((a + c, b + d))
  let ((a,b): complex) - ((c,d): complex) = T.((a - c, b - d))
  let ((a,b): complex) * ((c,d): complex) = T.((a * c - b * d,
                                                b * c + a * d))
  let ((a,b): complex) / ((c,d): complex) =
    T.(((a * c + b * d) / (c * c + d * d),
        (b * c - a * d) / (c * c + d * d)))

  let mag ((a,b): complex) =
    T.(sqrt (a * a + b * b))
  let arg ((a,b): complex) =
    T.atan2 b a

  let sqrt ((a,b): complex) =
    let gamma = T.(sqrt ((a + sqrt (a * a + b * b)) / i32 2))
    let delta = T.(sgn b *
                   sqrt (((i32 0 - a) + sqrt (a * a + b * b)) / i32 2))
    in (gamma, delta)

  let exp ((a,b): complex) =
    let expx = T.exp a
    in mk T.(expx * cos b) T.(expx * sin b)

  let log (z: complex) =
    mk (T.log (mag z)) (arg z)
}

-- We instantiate the complex number module using double-precision
-- floats for the components:

module c64 = mk_complex f64

-- And try it out in the REPL:
--
-- ```
-- > c64.exp (c64.mk 1 2)
-- (-1.1312043837568135f64, 2.4717266720048188f64)
-- ```

-- The
-- [github.com/diku-dk/complex](https://github.com/diku-dk/complex)
-- package (which you should use in real programs rather than rolling
-- your own) is implemented in exactly this way.
--
-- # See also
--
-- [Dex: Mandelbrot set](dex-mandelbrot.html).
