-- # Abstract data types
--
-- Sometimes we wish to define a data type that does not expose its
-- representation.  Such data types are called *abstract* or *opaque*.
-- Suppose we wish to define operations on [normalised vectors in 2D
-- space](https://mathworld.wolfram.com/NormalizedVector.html).  If we
-- directly exposed the representation, then it would be easy to
-- accidentally break normalisation.  To avoid mistakes, we need all
-- operations to go through functions that are careful to re-normalise
-- when needed.
--
-- In Futhark, this kind of information hiding is done via the module
-- system.  First we define a *module type* that describes an
-- interface consisting of abstract types and operations on those
-- types:

module type normvec = {
  type vec
  val mk : {x: f64, y: f64} -> vec
  val unmk : vec -> {x: f64, y: f64}
  val add : vec -> vec -> vec
}

-- The module type states that there is some abstract type `vec`, a
-- function `mk` that can turn [records](tuples-and-records.html) into
-- `vec`s, and a function `unmk` that can turn `vec`s into records.
-- The idea is that the `mk` function will perform normalisation.  The
-- module type also specifies a function `add` for adding normalised
-- vectors.  This is of course a much smaller set of operations than
-- you'd need for real programming, but it's enough to show the idea.
--
-- We can define a module that *implements* the `normvec` module type
-- like this:

module normvec : normvec = {

-- Note that module names and module type names live in different
-- namespaces, and there is no requirement that the name of a module
-- must match the module type it is implementing.
--
-- We first define the `vec` type, as a record.

  type vec = {x: f64, y: f64}

-- The definition of `vec` is visible inside the module itself, but
-- outside users are only able to use it through the operations
-- specified by the `normvec` module type.

-- The `mk` function normalises the given coordinates.

  def mk {x, y} : vec =
    let norm = f64.sqrt (x**2+y**2)
    in {x = x / norm, y = y / norm}

-- The `unmk` function just returns the coordinates.

  def unmk {x, y} = {x, y}

-- Finally, the `add` function adds the components, then re-normalises
-- the vector.

  def add (a: vec) (b: vec) : vec =
    {x = (a.x+b.x)/2, y = (a.y+b.y)/2}

}

-- Now we can use the module as e.g:
--
-- ```
-- > let a = normvec.mk {x=2,y=1}
-- > let b = normvec.mk {x=2,y=10}
-- > let c = normvec.add a b
-- > normvec.unmk c
-- {x = 0.54527166306905f64, y = 0.7138971355954391f64}
-- ```
--
-- If we directly tried to access `a.x`, we would get a type error.
--
-- Note that if you actually load this into `futhark repl` to play
-- around with it, you'll be able to directly observe the contents of
-- `normvec.vec` values.  To ease debugging, the interpreter does not
-- respect abstraction boundaries when prettyprinting.
--
-- # See also
--
-- [Complex numbers](complex-numbers.html), [Triangular
-- arrays](triangular.html), [Nominal types](nominal-types.html), [AD
-- with dual numbers](dual-numbers.html), [Three-dimensional
-- vectors](3d-vectors.html).
--
-- Reference manual:
-- [Module System](https://futhark.readthedocs.io/en/latest/language-reference.html#module-system).
