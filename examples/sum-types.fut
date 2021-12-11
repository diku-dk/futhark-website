-- # Sum types and pattern matching
--
-- Futhark supports nonrecursive sum types (sometimes called variants
-- or tagged unions).  A sum type indicates one or more `#`-prefixed
-- constructors, and for each constructor a *payload*.

type int_or_float = #int i32 | #float f32

def an_int : int_or_float = #int 2

-- Given a value of a sum type, we use a `match` expression to handle
-- each possible case.

def increment (v: int_or_float): int_or_float =
  match v
  case #int x -> #int (x+1)
  case #float x -> #float (x+1)

-- There must be a `case` for each possible constructor.
--
-- Sum type constructors can have any number of payload values,
-- including zero.

type dir = #left | #right

-- Such types are sometimes called *enumeration types* (or *enums*),
-- but to Futhark they are the same as sum types.
--
-- A common trick is to define a constructor payload as a
-- [record](tuples-and-records.html) in order to imitate named fields.

type point = #one_d {x: f32, y: f32}
           | #two_d {x: f32, y: f32, z: f32}

def scale (p: point) (d: f32) : point =
  match p
  case #one_d {x,y}   -> #one_d {x=x*d, y=y*d}
  case #two_d {x,y,z} -> #two_d {x=x*d, y=y*d, z=z*d}

-- Like all other types in Futhark, sum types are structurally typed.
-- This means that naming with `type` is optional, and we can just
-- inline the type instead:

def fast (c: #hedgehog | #train) =
  match c
  case #hedgehog -> true
  case #train -> false

def is_fast = fast #hedgehog

-- In practice, this means that we often need to add type annotations
-- to disambiguate which type we mean, as there are an infinite number
-- of possible types with the same constructor.  For example, to
-- supplement `dir` from before, we can define the following:

type dir_3d = #left | #right | #up | #down

-- When the compiler encounters an expression `#left`, how is it
-- supposed to know whether it is of type `dir`, `dir_3d`, or some
-- anonymous type that we have not even bothered naming?  The type
-- checker will complain in these cases, and we can solve the problem
-- by adding, for example, a return type annotation to the function.
