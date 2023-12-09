-- # Reading and writing files in literate Futhark
--
-- Futhark is not a general purpose language, but `futhark literate`
-- can read files and pass their contents to Futhark entry points.
-- Sometimes that is enough to do interesting things. File contents
-- are provided as values of type `[]u8`, meaning the raw bytes. Any
-- text decoding must be done manually, if applicable.

def rev_first_n (n: i64) (s: []u8) = reverse (take n s)

-- Reading a file is done with the `$loadbytes` builtin, which is only
-- valid in FutharkScript.

-- > rev_first_n 10 ($loadbytes "examples/literate-files.fut")

-- # See also
--
-- [The other Literate Futhark
-- examples.](http://localhost:8000/examples.html#literate-futhark)
