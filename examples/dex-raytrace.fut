-- # Dex: Multi-step ray tracer
--
-- The following is a port of
-- [raytrace.dx](https://google-research.github.io/dex-lang/mandelbrot.html),
-- which is itself based on [this Jax
-- program](https://github.com/ericjang/pt-jax/blob/master/jaxpt_vmap.ipynb)
-- by Eric Jang.  See [this blog
-- post](https://blog.evjang.com/2019/11/jaxpt.html) for details on
-- the algorithm - this example won't be getting into those details.
-- We've tried to maintain the naming scheme of the original program,
-- but we've re-ordered most definitions bring them closer to their
-- first usage.  This is mostly necessary due to the somewhat
-- convoluted program structure forced by the use of modules for AD.

import "dex-prelude"

-- The biggest issue is that the Dex program uses the `grad` operator
-- to convert distance functions into [normal
-- functions](https://en.wikipedia.org/wiki/Normal_(geometry)).
-- Futhark does not (yet) support anything like this as a language
-- construct.  Instead, we use forward-mode AD with dual numbers,
-- which can be [implemented as a library in
-- Futhark](dual-numbers.html).  It works, and is reasonably efficient
-- in this case, but it is certainly *not* as concise.

module ad = import "dual-numbers"

module type expanded_field = {
  include ad.ordered_field
  val max : t -> t -> t
  val sqrt : t -> t
  val cos : t -> t
  val sin : t -> t
  val abs : t -> t
}

module f64_field : (expanded_field with t = f64) = {
  let max = f64.max
  let sqrt = f64.sqrt
  let cos = f64.cos
  let sin = f64.sin
  let abs = f64.abs

-- We expose all the definitions from `f64_field` rather than
-- repeating them.

  open ad.f64_field
}

-- The field implementation for dual numbers is mostly boilerplate.
-- We cannot reuse the `mk_dual` parametric module from
-- [dual-numbers.fut](dual-numbers.html), because that module chose to
-- make the representation abstract.

module f64_dual_field = {
  type underlying = f64
  type t = (underlying, underlying)

  let primal ((x, _) : t) = x
  let tangent ((_, x') : t) = x'
  let dual0 x : t = (x, 0)
  let dual1 x : t = (x, 1)

  let max (x,x' : f64) (y,y') =
    (f64.max x y,
     if x >= y then x' else y')

  let sqrt (x,x') =
    (f64.sqrt x, x' / (2 * f64.sqrt x))

  let cos (x, x') =
    (f64.cos x, f64.negate x' * f64.sin x)

  let sin (x, x') =
    (f64.sin x, x' * f64.cos x)

  let abs (x,x') =
    (f64.abs x, f64.sgn x * x')

  let negate (x,x') : t = (-x, -x')
  let recip (x,x') : t = (1/x, -(x'/(x*x)))

  let (x,x') + (y,y') : t = f64.((x + y, x' + y'))
  let (x,x') * (y,y') : t = f64.((x * y, x' * y + x * y'))
  let x - y = x + negate y
  let x / y = x * recip y

  let (x,_) == (y,_) = f64.(x == y)
  let (x,_) < (y,_) = f64.(x < y)
  let (x,_) > (y,_) = f64.(x > y)
  let (x,_) <= (y,_) = f64.(x <= y)
  let (x,_) >= (y,_) = f64.(x >= y)
  let (x,_) != (y,_) = f64.(x != y)

  let f64 x = dual0 x
  let zero = f64 0
  let one = f64 1
}

-- ## Geometry

-- Sine we need the basic geometry functions of the ray tracer to work
-- with both normal and dual numbers, we define it as a parametric
-- module.

module mk_geometry(Float: expanded_field) = {

type Float = Float.t

-- The original Dex program implements vectors in three-dimensional
-- space with three-element arrays.  This is not optimal in Futhark
-- for [reasons explained
-- elsewhere](https://futhark-lang.org/blog/2019-01-13-giving-programmers-what-they-want.html),
-- so instead we import the vector module from [another example
-- program](3d-vectors.html).

module threed = import "3d-vectors"

-- And instantiate it with whatever `Float` type we have at hand:

module vspace = threed.mk_vspace_3d Float

-- Convenient shorthands for various vector types and operations:

type Vec = vspace.vector
let (*>) = vspace.scale
let (<+>) = (vspace.+)
let (<->) = (vspace.-)
let (<*>) = (vspace.*)
let dot = vspace.dot
let length = vspace.length
let normalize = vspace.normalise
let cross = vspace.cross
let rotateY = flip vspace.rot_y

-- These vectors are not arrays and we cannot `map` them, but we can
-- define our own mapping function.

let vmap (f: Float -> Float) (v : Vec) =
  {x = f v.x, y = f v.y, z = f v.z}

-- Dex calls the scaling operator `.*`, but that name is not valid in
-- Futhark.  Note that the `length` function will shadow the one in
-- the Futhark prelude.

-- Dex's `Color` type is from the
-- [plot.dex](https://github.com/google-research/dex-lang/blob/main/lib/plot.dx)
-- file, which we're not going to implement in its entirety.
--
-- Much of the following is just as straighforward in Futhark as in
-- Dex, and so will not have much commentary.

type Color = Vec
type Angle = Float -- angle in radians

type Distance = Float

type Position = Vec
type Direction = Vec  -- Should be normalized.

type BlockHalfWidths = Vec
type Radius = Float
type Radiance = Color

type ObjectGeom
  = #Wall Direction Distance
  | #Block Position BlockHalfWidths Angle
  | #Sphere Position Radius

type Surface
  = #Matte Color
  | #Mirror

type OrientedSurface = (Direction, Surface)

type Object
  = #PassiveObject ObjectGeom Surface
  -- position, half-width, intensity (assumed to point down)
  | #Light Position Float Radiance

-- ### The signed distance function
--
-- This function is the reason why the Futhark implementation is so
-- convoluted, as we will need its derivative.

let sdObject (pos:Position) (obj:Object) : Distance =
  match obj
  case #PassiveObject geom _ ->
    (match geom
     case #Wall nor d -> Float.(d + dot nor pos)
     case #Block blockPos halfWidths angle ->
       let pos' = rotateY (pos <-> blockPos) angle
       in length (vmap (Float.max Float.zero) (vmap Float.abs pos' <-> halfWidths))
     case #Sphere spherePos r ->
       let pos' = pos <-> spherePos
       in Float.(max (length pos' - r) (f64 0)))
  case #Light squarePos hw _ ->
    let pos' = pos <-> squarePos
    let halfWidths = {x=hw, y=Float.f64 0.1, z=hw}
    in length (vmap (Float.max Float.zero)
                    (vmap Float.abs pos' <-> halfWidths))

-- With that done, the rest of the ray tracer can be written in a
-- non-modularised way, using just floating-point numbers.

}

-- ## Working with the geometry
--
-- We'll need to instantiate the geometry module twice: once with
-- ordinary floats, and once with dual numbers.

module geometry = mk_geometry f64_field
module geometry_dual = mk_geometry f64_dual_field

-- To use `geometry_dual.sdObject`, we'll need to convert objects to
-- objects-with-dual-numbers.  For this, we'll first define some
-- utility functions.

let dual0 = f64_dual_field.dual0
let dual1 = f64_dual_field.dual1
let dualVec {x,y,z} = {x=dual0 x, y=dual0 y, z=dual0 z}

-- And then a bunch of boilerplate definitions.

let dualGeom (geom: geometry.ObjectGeom) : geometry_dual.ObjectGeom =
  match geom
  case #Wall nor d ->
    #Wall (dualVec nor) (dual0 d)
  case #Block blockPos halfWidths angle ->
    #Block (dualVec blockPos) (dualVec halfWidths) (dual0 angle)
  case #Sphere spherePos r ->
    #Sphere (dualVec spherePos) (dual0 r)

let dualSurface (surf: geometry.Surface) : geometry_dual.Surface =
  match surf
  case #Matte c -> #Matte (dualVec c)
  case #Mirror -> #Mirror

let dualObject (obj: geometry.Object) : geometry_dual.Object =
  match obj
  case #PassiveObject geom surface ->
    #PassiveObject (dualGeom geom) (dualSurface surface)
  case #Light squarePos hw r ->
    #Light (dualVec squarePos) (dual0 hw) (dualVec r)

-- The non-dual-number geometry module contains various useful type
-- definitions.  Instead of replicating their definitions, we make
-- abbreviations.

type Object = geometry.Object
type Position = geometry.Position
type Direction = geometry.Direction
type Distance = geometry.Distance
type Radiance = geometry.Radiance
type Color = geometry.Color
type Surface = geometry.Surface
type OrientedSurface = geometry.OrientedSurface
type Vec = geometry.Vec

-- We also need some of the geometry functions:

let dot = geometry.dot
let (<+>) = (geometry.<+>)
let (<->) = (geometry.<->)
let (<*>) = (geometry.<*>)
let (*>) = (geometry.*>)
let normalize = geometry.normalize
let length = geometry.length

-- And a few more shorthands:

let vec x y z : Vec = {x,y,z}

-- Finally we can define the function that computes surface normals at
-- a given position.  This function is the reason we went to all that
-- trouble with dual numbers.

let calcNormal (obj: Object) (pos: Position) : Direction =
  let f i =
    let pos' = {x = if i == 0 then dual1 pos.x else dual0 pos.x,
                y = if i == 1 then dual1 pos.y else dual0 pos.y,
                z = if i == 2 then dual1 pos.z else dual0 pos.z}
    in f64_dual_field.tangent (geometry_dual.sdObject pos' (dualObject obj))
  in geometry.normalize (vec (f 0) (f 1) (f 2))

-- ## Random sampling
--
-- Since [dex-prelude.fut](dex-prelude.html) defines the same random
-- number generation interface as the Dex program, the sampling
-- functions are straightforward ports.
--
-- First, a function for generating a number in a range:

let randuniform (lower: f64) (upper: f64) (k: Key) =
  let x = rand k
  in (lower + x * (upper-lower))

-- Sample within a square with side length *2\*hw*:

let sampleSquare (hw: f64) (k: Key) : Position =
  let (kx, kz) = splitKey k
  let x = randuniform (- hw) hw kx
  let z = randuniform (- hw) hw kz
  in {x, y=0.0, z}

-- Sample within a hemisphere in the direction of the normal vector:

let sampleCosineWeightedHemisphere (normal: Vec) (k: Key) : Vec =
  let (k1, k2) = splitKey k
  let u1 = rand k1
  let u2 = rand k2
  let uu = geometry.normalize (geometry.cross normal (vec 0.0 1.1 1.1))
  let vv = geometry.cross uu normal
  let ra = f64.sqrt u2
  let rx = ra * f64.cos (2 * f64.pi * u1)
  let ry = ra * f64.sin (2 * f64.pi * u1)
  let rz = f64.sqrt (1.0 - u2)
  let rr = (rx *> uu) <+> (ry *> vv) <+> (rz *> normal)
  in normalize rr

-- ## Ray marching
--
-- The essence of ray marching is simple: move along a given vector
-- and find the first object we collide with.  We do this the naive
-- way, by invoking the distance function for every single object.
-- This is not efficient, to put it mildly, but it is easy to
-- implement.
--
-- First we need a `minimumBy` function.  Despite Futhark lacking
-- Dex's support for ad-hoc polymorphism, which matters here as we end
-- up passing in the less-than operator, it doesn't end up looking
-- *too* bad.

let minBy 'a 'o (lt: o -> o -> bool) (f: a->o) (x:a) (y:a) : a =
  if f x `lt` f y then x else y

let minimumBy [n] 'a 'o (lt: o -> o -> bool) (f: a->o) (xs: [n]a) : a =
  reduce (minBy lt f) xs[0] xs

-- A scene is a collection of objects.

type Scene [n] = [n]Object

-- We can now define a function for finding the closest object, given
-- a position.

let sdScene (objs: Scene []) (pos: Position) : (Object, Distance) =
  let i =
    minimumBy (<) (\i -> geometry.sdObject pos objs[i])
              (indices objs)
  in (objs[i], geometry.sdObject pos objs[i])

-- When we ray march, we either collide with nothing and disappear
-- into the aether, hit a light, or hit an object, in which case we
-- also produce information about the surface, including the surface
-- normal.

type Ray = (Position, Direction)

type RayMarchResult
  = #HitObj Ray OrientedSurface
  | #HitLight Radiance
  | #HitNothing

-- The ray marching function is defined with the `iter` function in
-- Dex.  In Futhark, a `while` loop can express it in an equally
-- natural way.

let positiveProjection (x: Vec) (y: Vec) =
  dot x y > 0

let raymarch [n] (scene:Scene [n]) (ray:Ray) : RayMarchResult =
  let max_iters = 100
  let tol = 0.01
  let (rayOrigin, rayDir) = ray
  let (_, _, res) =
   loop (i, rayLength, _) = (0, 10 * tol, #HitNothing)
     while i < max_iters do
     let rayPos = rayOrigin <+> (rayLength *> rayDir)
     let (obj, d) = sdScene scene rayPos
     let dNew = rayLength + 0.9 * d
     in if d >= tol
        then (i+1, dNew, #HitNothing)
        else
        let surfNorm = calcNormal obj rayPos
        in if positiveProjection rayDir surfNorm
           then (i+1, dNew, #HitNothing)
           else (max_iters,
                 dNew,
                 match obj
                 case #PassiveObject _ surf ->
                   #HitObj (rayPos, rayDir) (surfNorm, surf)
                 case #Light _ _ radiance ->
                   #HitLight radiance)
  in res

-- ## Light sampling
--
-- These definitions are pretty straightforward, and similar to those
-- in Dex.
--
-- To figure out whether a light is shining on us along a given path,
-- we just march along that path and see if we hit a light.  If we do,
-- the radiance is that light.

let rayDirectRadiance [n] (scene: Scene [n]) (ray: Ray) : Radiance =
  match raymarch scene ray
  case #HitLight intensity -> intensity
  case #HitNothing -> vec 0 0 0
  case #HitObj _ _ -> vec 0 0 0

-- Shorthands for vectors along the cardinal axes will become useful
-- in the following.

let xHat : Vec = vec 1 0 0
let yHat : Vec = vec 0 1 0
let zHat : Vec = vec 0 0 1

let relu (x: f64) = f64.max x 0

let probReflection ((nor, surf): OrientedSurface) (_:Ray) ((_, outRayDir):Ray) : f64 =
  match surf
  case #Matte _ -> relu (dot nor outRayDir)
  case #Mirror  -> 0

let directionAndLength (x: Vec) =
  (normalize x, length x)

-- The following function determines how much light is shining on a
-- given point.  It's very similar to the Dex function.  Dex uses an
-- explicit accumulator that is updated via effects, but I don't think
-- it makes it much more readable in this case.

let sampleLightRadiance [n] (scene: Scene [n])
                            (osurf: OrientedSurface)
                            (inRay: Ray)
                            (k: Key) : Radiance =
  let (surfNor, _) = osurf
  let (rayPos, _) = inRay
  in loop radiance = vec 0 0 0 for obj in scene do
       match obj
       case #PassiveObject _ _ -> radiance
       case #Light lightPos hw _ ->
         let (dirToLight, distToLight) =
           directionAndLength (lightPos <+> sampleSquare hw k <-> rayPos)
         in if ! (positiveProjection dirToLight surfNor)
            then radiance -- light on the far side of current surface
            else
            let fracSolidAngle = relu (dot dirToLight yHat) *
                                 sq hw / (f64.pi * sq distToLight)
            let outRay = (rayPos, dirToLight)
            let coeff = fracSolidAngle * probReflection osurf inRay outRay
            in radiance <+> (coeff *> rayDirectRadiance scene outRay)

-- ## Tracing
--
-- Almost done.  Everything here is very similar to the Dex code.

type Filter = Color

let applyFilter (filter:Filter) (radiance:Radiance) : Radiance =
  filter <*> radiance

let surfaceFilter (filter:Filter) (surf:Surface) : Filter =
  match surf
  case #Matte color -> filter <*> color
  case #Mirror      -> filter

let sampleReflection ((nor, surf): OrientedSurface) ((pos, dir): Ray) (k: Key) : Ray =
  let newDir = match surf
               case #Matte _ -> sampleCosineWeightedHemisphere nor k
               case #Mirror  -> dir <-> 2 * dot dir nor *> nor
  in (pos, newDir)

-- We're excluding Dex's `shareSeed` field from the tracing
-- parameters, since it seems mostly relevant for showing the impact
-- of poor seeding on the convergence of the algorithm.

type Params = { numSamples : i32,
                maxBounces : i32 }

let trace [n] (params: Params) (scene: Scene [n]) (init_ray: Ray) (k: Key) : Color =
  (.2) <|
  loop
    (i, filter, radiance, ray) = (0, (vec 1 1 1), (vec 0 0 0), init_ray)
  while i < params.maxBounces do
    match raymarch scene ray
    case #HitNothing ->
      (params.maxBounces, filter, radiance, ray)
    case #HitLight intensity ->
      if i == 0
      then (params.maxBounces, filter, intensity, ray)
      else (params.maxBounces, filter, radiance, ray)
    case #HitObj incidentRay osurf ->
      let (k1, k2) = splitKey (hash k i)
      let lightRadiance = sampleLightRadiance scene osurf incidentRay k1
      let outRayHemisphere = sampleReflection osurf incidentRay k2
      let newFilter = surfaceFilter filter osurf.1
      let newRadiance = radiance <+> applyFilter newFilter lightRadiance
      in (i+1, newFilter, newRadiance, outRayHemisphere)

-- ## Camera
--
-- The camera controls how the initial rays are sent into the scene.

type Camera =
  { numPix     : i64,
    pos        : Position,
    halfWidth  : f64,
    sensorDist : f64 }

-- And now we're ready to produce the *n\*n* array of initial rays and
-- RNG states.

let cameraRays (n: i64) (camera: Camera) : [n][n](Ray, Key) =
  let halfWidth = camera.halfWidth
  let pixHalfWidth = halfWidth / f64.i64 n
  let ys = reverse (linspace n (-halfWidth) halfWidth)
  let xs = linspace n (-halfWidth) halfWidth
  let kss = tabulate_2d n n (\i j -> newKey (i32.i64 (1+i*n+j)))
  let rayForPixel y x k =
    let dx = randuniform (-pixHalfWidth) pixHalfWidth k
    let dy = randuniform (-pixHalfWidth) pixHalfWidth k
    in ((camera.pos,
         normalize (vec (x + dx) (y + dy) (-(camera.sensorDist)))),
        k)
  in map2 (\y ks -> map2 (rayForPixel y) xs ks) ys kss

-- Most ray tracers perform multiple samples per pixel and take the
-- average.  The Dex program defines the `sampleAveraged` function to
-- be (potentially) parallel.  We know the parallelism would never be
-- exploited anyway, so we define it as a sequential loop, mostly to
-- make the random number state management simpler.

let sampleAveraged (sample: Key -> Vec) (n: i32) (k: Key) : Vec =
  (loop acc = vec 0 0 0 for i < n do
   (acc <+> sample (ixkey k (i64.i32 i))))
  |> ((1/f64.i32 n) *>)

-- The Dex implementation uses an unusual relative colorisation
-- strategy, where the final color values are all *divided* by the
-- average intensity.  This is likely so we won't have to fiddle with
-- the light intensities to avoid very bright or very dark images.

let meanIntensity image =
  image |> flatten |> map (\{x,y,z} -> (x+y+z)/3) |> mean

-- Smile for the camera!

let takePicture [m] (params: Params) (scene: Scene [m]) (camera: Camera) =
  let n = camera.numPix
  let rays = cameraRays n camera
  let sample (r, k) =
    sampleAveraged (trace params scene r) params.numSamples k
  let image = map (map sample) rays
  let mean = meanIntensity image
  in map (map ((1/mean)*>)) image

-- ## Scene definition
--
-- The only odd thing here is that apparently our vectors rotate
-- opposite of the way Dex does it, so I had to give the block a
-- rotation of *-0.5* instead of *0.5*.

let lightColor = vec 0.2 0.2 0.2
let leftWallColor  = 1.5 *> vec 0.611 0.0555 0.062
let rightWallColor = 1.5 *> vec 0.117 0.4125 0.115
let whiteWallColor = (1/255) *> vec 255.0 239.0 196.0
let blockColor     = (1/255) *> vec 200.0 200.0 255.0

let neg = ((-1)*>)

let theScene : Scene [] =
    [ #Light (1.9 *> yHat) 0.5 lightColor
    , #PassiveObject (#Wall      xHat  2.0) (#Matte leftWallColor)
    , #PassiveObject (#Wall (neg xHat) 2.0) (#Matte rightWallColor)
    , #PassiveObject (#Wall      yHat  2.0) (#Matte whiteWallColor)
    , #PassiveObject (#Wall (neg yHat) 2.0) (#Matte whiteWallColor)
    , #PassiveObject (#Wall      zHat  2.0) (#Matte whiteWallColor)
    , #PassiveObject (#Block  (vec 1.0 (-1.6) 1.2) (vec 0.6 0.8 0.6) (-0.5)) (#Matte blockColor)
    , #PassiveObject (#Sphere (vec (-1.0) (-1.2) 0.2) 0.8) (#Matte (0.7 *> whiteWallColor))
    , #PassiveObject (#Sphere (vec 2 2 (-2)) 1.5) #Mirror
    ]

let defaultParams : Params = { numSamples = 50
                             , maxBounces = 10 }

let defaultCamera : Camera = { numPix     = 250
                             , pos        = 10.0 *> zHat
                             , halfWidth  = 0.3
                             , sensorDist = 1.0 }

-- ## Entry point
--
-- This is not part of the Dex program, but is needed if we want
-- Futhark to produce anything for the outside world.
--
-- First, a function to convert floating-point colors to RGB-packed
-- colors with 8 bits per channel.  Note that we have to cap each
-- channel, since there is no guarantee that the color components
-- produced by the ray tracer cannot exceed *1.0*.

let pix (c: Color) =
  (u32.f64 (f64.min 255 (c.x * 255)) << 16) |
  (u32.f64 (f64.min 255 (c.y * 255)) << 8) |
  (u32.f64 (f64.min 255 (c.z * 255)))

let main n =
  takePicture defaultParams theScene (defaultCamera with numPix = n)
  |> map (map pix)

-- As with [the Mandelbrot set](dex-mandelbrot.html), we use
-- [data2png.py](https://github.com/diku-dk/futhark/blob/master/tools/data2png.py)
-- to convert the output to an image:
--
-- ```
-- $ futhark c dex-raytrace.fut
-- $ echo 500 | ./dex-raytrace -b | data2png.py /dev/stdin dex-raytrace.png
-- ```
--
-- ![](dex-raytrace.png)
