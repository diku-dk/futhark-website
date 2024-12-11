-- # Option types
--
-- Futhark does not predefine an option type, but we can define one
-- ourselves.

type opt 'a = #some a | #none

-- It can be used for example to define a safe indexing function.

def index_maybe [n] 't (x: [n]t) (i: i64) : opt t =
  if i >= 0 && i < n then #some x[i] else #none

-- ## See also
--
-- The [containers](https://github.com/diku-dk/containers) package,
-- which defines an option type and various helper functions.
