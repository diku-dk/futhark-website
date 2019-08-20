-- # Tuples and records
--
-- A tuple is written a two or more comma-separated components
-- enclosed by parentheses:

let a_tuple : (i32, bool) = (1, true)

-- The components of a tuple can be extracted with projections or with
-- pattern matching:

let projection =
  a_tuple.1

let patmatch =
  let (a,b) = a_tuple
  in a

-- Pattern matching is not allowed in top-level `let` bindings, but
-- only inside `let` expressions (which are terminated with `in`).

-- Note that tuple projection is 1-indexed, as [opposed to
-- arrays](arrays.html), which are 0-indexed.
--
-- Records are written as fields enclosed by curly braces:

let a_record : {foo: i32, bar: bool} =
  {foo = 1, bar = true}

-- Similarly to tuples, their components are also accessed with
-- projections and pattern matching:

let record_projection =
  a_record.foo

let record_patmatch =
  let {foo = x, bar = y} = a_record
  in x

-- When pattern matching, we can also leave off variable names for the
-- field, in order to bind variables of the same name as the fields:

let record_patmatch_impl =
  let {foo, bar} = a_record
  in foo

-- We can use record updates to change the value of a field:

let another_record =
  a_record with bar = false

-- Records and tuples are very similar.  In fact, a tuple is just a
-- record where all the fields look like numbers counted from 1:

let another_tuple : (i32, bool) =
  {1 = 0, 2 = true}
