-- # Parametric polymorphism
--
-- A polymorphic function is one in which one of the arguments can be
-- of any type.  We write a polymorphic function by first listing
-- the type parameters in which the function is polymorphic.

let pair 'a 'b (x: a) (y: b) : (a,b) =
  (x,y)

-- We do not pass the type parameters `a` and `b` explicitly when we
-- call `pair`.  Rather, they are automatically inferred from the
-- value parameters.

let a_pair = pair 1 true

-- Although using explicit type parameters is recommended, a
-- polymorphic type can also be inferred automatically:

let pair_implicit x y =
  (x,y)

-- We can use type parameters to make a function *less polymorphic*
-- than it would normally be.  For example, we can write a variant of
-- `pair` where we require that both arguments have the same type:

let pair_same 'a (x: a) (y: a) : (a,a) =
  (x,y)

-- When a polymorphic function is called with an array argument, all
-- arrays passed for the same polymorphic type must be of the same
-- size.  For example, `pair_same [1] [2,3]` is illegal, but `pair [1]
-- [2,3]` is fine.
