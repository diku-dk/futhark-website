-- ---
-- title: AD with dual numbers
-- ---
--
-- [AD](https://en.wikipedia.org/wiki/Automatic_differentiation) is
-- the term for a family of techniques that compute the derivatives of
-- computer programs.  One particularly simple technique is
-- [forward-mode AD with dual
-- numbers](https://blog.demofox.org/2014/12/30/dual-numbers-automatic-differentiation/),
-- which has the convenient property that it can be implemented as a
-- library in most programming languages.  This is in contrast to
-- other techniques that require nonlocal code transformations,
-- typically integrated into the compiler.  The basic idea of
-- forward-mode AD is that we write our functions such that "numbers"
-- carry not just their normal ("primal") value, but also their
-- differentiated value ("tangent").

-- Differentiation is defined on
-- [fields](https://en.wikipedia.org/wiki/Field_(mathematics)), so
-- first we [define a module type](abstract-data-types.html) that
-- describes the field interface.

module type field = {
  -- | The field element type.
  type t

  -- | Constructing an element from a float.
  val f64 : f64 -> t

  val + : t -> t -> t
  val - : t -> t -> t
  val * : t -> t -> t
  val / : t -> t -> t

  -- | Additive identity.
  val zero : t

  -- | Multiplicative identity.
  val one : t

  -- | Additive inverse.
  val negate : t -> t

  -- | Multiplicative inverse.
  val recip : t -> t
}

-- Plain fields are not enough to define many interesting functions.
-- An ordered field is a field that also admits the usual comparison
-- operators.

module type ordered_field = {
  include field

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool
  val <=: t -> t -> bool
  val >=: t -> t -> bool
  val !=: t -> t -> bool
}

-- We can define modules that implement `ordered_field` from scratch,
-- but it is more convenient to define a parametric module that can
-- take any module `F` that implements the `numeric` module type, and
-- construct a corresponding `ordered_field`.  The `numeric` module
-- type is implemented by the built-in `f32` and `f64` modules.  Most
-- of the definitions are just forwarding those from the module
-- parameter `F`.

module mk_field_from_numeric (F: numeric) : ordered_field with t = F.t = {
  type t = F.t
  let f64 = F.f64
  let (+) = (F.+)
  let (-) = (F.-)
  let (*) = (F.*)
  let (/) = (F./)
  let (==) = (F.==)
  let (<) = (F.<)
  let (>) = (F.>)
  let (<=) = (F.<=)
  let (>=) = (F.>=)
  let (!=) = (F.!=)

  let zero = F.i64 0
  let one = F.i64 1
  let negate x = zero - x
  let recip x = one / x
}

-- We create a field module where the elements are `f64`:

module f64_field = mk_field_from_numeric f64

-- Now we can define the module type of fields of dual numbers.  These
-- support the usual field operations, as well as extracting the
-- "primal" (normal) and "tangent" (differentiated) components of a
-- number.

module type dual_field = {
  -- | The element type of the underlying field (the components of the
  -- dual numbers).
  type underlying

  -- | We include all the operations required by ordered fields.  The
  -- 't' will be a dual number.
  include ordered_field

  -- | The primal of a dual number if the normal result.
  val primal : t -> underlying

  -- | The tangent is, well, the tangent..
  val tangent : t -> underlying

  -- | Construct a dual number with tangent zero.
  val dual0 : underlying -> t

  -- | Construct a dual number with tangent one.
  val dual1 : underlying -> t
}

-- We now define a parametric module that, given an ordered field,
-- construct a ordered field that uses dual numbers for the field
-- elements.  We keep the actual representation of the dual numbers
-- abstract.

module mk_dual (F: ordered_field) : (dual_field with underlying = F.t) = {
  type underlying = F.t

-- We represent a dual number as a pair of the "primal" and "tangent"
-- parts.

  type t = (underlying, underlying)
  let primal ((x, _) : t) = x
  let tangent ((_, x') : t) = x'
  let dual0 x : t = (x, F.f64 0)
  let dual1 x : t = (x, F.f64 1)

-- A constant has tangent zero.

  let f64 x = dual0 (F.f64 x)
  let zero = f64 0
  let one = f64 1

-- Negation is defined in the obvious way.

  let negate (x,x') = (F.negate x, F.negate x')

-- The reciprocal is a little more tricky, but you can look up the
-- reciprocal rule in a calculus textbook (or more realistically, on
-- Wikipedia).

  let recip (x,x') = (F.recip x, F.(negate (x'/(x*x))))

-- Then we get to the actual arithmetic operations.  These are also
-- as you'd expect to find in a textbook.  We define subtraction and
-- division via the inverse elements, so we have fewer things
-- written from scratch.

  let (x,x') + (y,y') = F.((x + y, x' + y'))
  let (x,x') * (y,y') = F.((x * y, x' * y + x * y'))
  let x - y = x + negate y
  let x / y = x * recip y

-- Comparisons are straightforward and use only the primal parts.
-- Since we produce booleans here, the result has no tangent.

  let (x,_) == (y,_) = F.(x == y)
  let (x,_) < (y,_) = F.(x < y)
  let (x,_) > (y,_) = F.(x > y)
  let (x,_) <= (y,_) = F.(x <= y)
  let (x,_) >= (y,_) = F.(x >= y)
  let (x,_) != (y,_) = F.(x != y)
}

-- We instantiate it with the `f64_field` module defined above:

module dual_f64 = mk_dual f64_field

-- To show off forward-mode AD, we define various functions
-- parameterised over the field representation.  This lets us evaluate
-- them using either normal numbers or dual numbers.  In Haskell we'd
-- use type classes for this, but in Futhark and other ML languages,
-- we use a parametric module yet again.  This carries a fair bit of
-- overhead, unfortunately.

module mk_test (F: ordered_field) = {
  let test (x: F.t) (y: F.t) =
    F.((x*x) + (x*y))
}

-- We can instantiate this module using both ordinary numbers and dual
-- numbers:

module test_f64 = mk_test f64_field
module test_dual = mk_test dual_f64

-- Ordinary evaluation works:
--
-- ```
-- > :t test_f64.test
-- test_f64.test : (x: f64) -> (y: f64) -> f64
-- > test_f64.test 1 2
-- 3.0f64
-- ```
--
-- When we want to compute the partial derivative of a function with
-- respect to one of its parameters, we pass it a dual number with
-- initial tangent *1* for *just that* parameter (using the `dual1`
-- function), and make the initial tangent *0* for all the other
-- parameters:
--
-- ```
-- > dual_f64.tangent (test_dual.test (dual_f64.dual1 1) (dual_f64.dual0 2))
-- 4.0f64
-- > dual_f64.tangent (test_dual.test (dual_f64.dual0 1) (dual_f64.dual1 2))
-- 1.0f64
-- ```
--
-- For a function with *n* inputs, we need to perform *n* evaluations
-- to obtain all partial derivatives.  This is the main weakness of
-- forward-mode AD.
--
-- More complex functions work well.  Like this implementation of
-- square root by naive Newton-Rhapson iteration (note that this is
-- not numerically stable):

module mk_sqrt (F: ordered_field) = {

  let abs (x: F.t) =
    if F.(x < f64 0)
    then F.negate x
    else x

  let sqrt (x: F.t) =
    let difference = F.f64 0.001
    in loop guess = F.f64 1
       while F.(abs(guess * guess - x) >= difference) do
         F.((x/guess + guess)/(f64 2))
}

module sqrt_dual = mk_sqrt dual_f64

-- Trying it out in the REPL:
--
-- ```
-- > dual_f64.tangent (sqrt_dual.sqrt (dual_f64.dual1 9))
-- 0.16673279002623667f64
-- ```
--
-- In practice, we usually add square roots and similar primitive
-- functions to our field interface and directly implement its known
-- derivative.  But this shows that even something as nasty as a
-- `while`-loop works fine with forward-mode AD.
