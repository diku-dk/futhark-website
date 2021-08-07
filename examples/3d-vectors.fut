-- # Three-dimensional vectors
--
-- This example shows how we might define a module type for
-- programming with vectors in normal 3D space.

module type vspace_3d = {

-- We keep the type of vector components abstract, because it will be
-- useful to have the same interface for vectors with components that
-- are `f32`, `f64`, or something even more exotic.

  type scalar

-- However, the vector type itself is opaque.  It must be a record
-- with `x`/`y`/`z` fields.

  type vector = {x: scalar, y: scalar, z: scalar}

-- Vectors must support the usual binary operations.

  val +: vector -> vector -> vector
  val -: vector -> vector -> vector
  val *: vector -> vector -> vector
  val /: vector -> vector -> vector


-- As well as dot products and cross products.

  val dot: vector -> vector -> scalar
  val cross: vector -> vector -> vector

-- Vectors can be scaled by a scalar.

  val scale: scalar -> vector -> vector

-- We can also take the length of a vector, as well as normalise a
-- vector to have unit length.

  val length: vector -> scalar
  val normalise: vector -> vector

-- Finally, vectors can be rotated around the three axes.

  val rot_x : (radians: scalar) -> vector -> vector
  val rot_y : (radians: scalar) -> vector -> vector
  val rot_z : (radians: scalar) -> vector -> vector

}

-- So for which scalars can this module type be implemented?  We
-- describe this with yet another module type.

module type scalar = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t

  val f64: f64 -> t
  val sqrt : t -> t
  val cos : t -> t
  val sin : t -> t
}

-- Because of Futhark's structural type system, the built-in modules
-- `f32` and `f64` already satisfy this interface.
--
-- We can now define a parametric module for constructing vector
-- spaces:

module mk_vspace_3d(P: scalar): vspace_3d with scalar = P.t = {
  type scalar = P.t

  type vector = {x: scalar, y: scalar, z: scalar}

  let zero = {x = P.f64 0, y = P.f64 0, z = P.f64 0}
  let one  = P.f64 1

-- Most of the definitions are straight out of a textbook, and so we
-- won't be providing much commentary.
--
-- We start out by defining two helper functions for doing operations
-- on the components of a vector.

  let map (f: scalar -> scalar) (v : vector) =
    {x = f v.x, y = f v.y, z = f v.z}

  let map2 (f: scalar -> scalar -> scalar) (a : vector) (b : vector) =
    {x = f a.x b.x, y = f a.y b.y, z = f a.z b.z}

-- This allows us to conveniently define vector arithmetic and scaling.

  let (+) = map2 (P.+)
  let (-) = map2 (P.-)
  let (*) = map2 (P.*)
  let (/) = map2 (P./)
  let scale (s: scalar) = map (s P.*)

-- The remaining operations are defined explicitly.

  let dot (a: vector) (b: vector) =
    P.(a.x*b.x + a.y*b.y + a.z*b.z)

  let cross ({x=ax,y=ay,z=az}: vector)
            ({x=bx,y=by,z=bz}: vector): vector =
    P.({x=ay*bz-az*by, y=az*bx-ax*bz, z=ax*by-ay*bx})

  let length v = P.sqrt (dot v v)

  let normalise (v: vector): vector =
    let l = length v
    in scale (one P./ l) v

  let rot_x (theta: scalar) ({x,y,z} : vector) =
    let cos_theta = P.cos theta
    let sin_theta = P.sin theta
    in { x
       , y = P.(cos_theta * y - sin_theta * z)
       , z = P.(sin_theta * y + cos_theta * z)}

  let rot_y (theta: scalar) ({x,y,z} : vector) =
    let cos_theta = P.cos theta
    let sin_theta = P.sin theta
    in { x = P.(cos_theta * x - sin_theta * z)
       , y
       , z = P.(sin_theta * x + cos_theta * z)}

  let rot_z (theta: scalar) ({x,y,z} : vector) =
    let cos_theta = P.cos theta
    let sin_theta = P.sin theta
    in { x = P.(cos_theta * x - sin_theta * y)
       , y = P.(sin_theta * x + cos_theta * y)
       , z}
}

-- Instantiating the module:

module f64_3d = mk_vspace_3d f64

-- And trying it out:

-- ```
-- > f64_3d.cross {x=1,y=2,z=3} {x=4,y=5,z=6}
-- {x = -3.0f64, y = 6.0f64, z = -3.0f64}
-- ```

-- # See also
--
-- [Dex: Multi-step ray tracer](dex-raytrace.html)
--
-- The [vector](https://github.com/athas/vector) package.
