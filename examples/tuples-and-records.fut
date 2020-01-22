-- # Tuples and records
--
-- A tuple is written a two or more comma-separated components
-- enclosed by parentheses:

let a_tuple : (i32, bool) = (1, true)

-- The components of a tuple can be extracted with projections or with
-- a pattern binding:

let projection =
  a_tuple.0

let patmatch =
  let (a,b) = a_tuple
  in a

-- Pattern bindings are not allowed in top-level `let` declarations,
-- but only inside `let` expressions (which are terminated with `in`).

-- Like [arrays](arrays.html), tuple projection is 1-indexed.
--
-- Records are written as fields enclosed by curly braces:

let a_record : {foo: i32, bar: bool} =
  {foo = 1, bar = true}

-- Similarly to tuples, their components are also accessed with
-- projections and pattern binding:

let record_projection =
  a_record.foo

let record_patmatch =
  let {foo = x, bar = y} = a_record
  in x

-- When pattern binding, we can also leave off variable names for the
-- field, in order to bind variables of the same name as the fields:

let record_patmatch_impl =
  let {foo, bar} = a_record
  in foo

-- We can use record updates to change the value of a field:

let another_record =
  a_record with bar = false

-- Records and tuples are very similar.  In fact, a tuple is just a
-- record where all the fields look like numbers counted from 0:

let another_tuple : (i32, bool) =
  {0 = 0, 1 = true}
