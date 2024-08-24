-- # Decoding MNIST data files
--
-- The [MNIST database of handwritten
-- digits](https://yann.lecun.com/exdb/mnist/) is a common dataset for
-- demonstrating machine learning techniques. It is distributed in the
-- form of "label fields" and "image files", with a simple binary file
-- format. The functions below decode this file format.

-- First we need a utility function for decoding a four-element byte
-- array as a big-endian integer.

def decode_u32_be (w: [4]u8) =
  (u32.u8 w[0] << 24)
  | (u32.u8 w[1] << 16)
  | (u32.u8 w[2] << 8)
  | (u32.u8 w[3] << 0)

-- Then we can define the decoding functions. These use assertions for
-- error reporting. Some applications may prefer to use an [option
-- type](opt.fut) instead.

entry decode_label_file (s: []u8) : []i8 =
  let magic = decode_u32_be (take 4 s)
  let n = i64.u32 (decode_u32_be (take 4 (drop 4 s)))
  in assert (magic==2049)
            (map i8.u8 (take n (drop 8 s)))

entry decode_image_file (s: []u8) : [][][]u8 =
  let magic = decode_u32_be (take 4 s)
  let n = i64.u32 (decode_u32_be (take 4 (drop 4 s)))
  let rows = i64.u32 (decode_u32_be (take 4 (drop 8 s)))
  let columns = i64.u32 (decode_u32_be (take 4 (drop 12 s)))
  let get_img i = unflatten (take (rows*columns) (drop (16+i*rows*columns) s))
  in assert (magic==2051) (tabulate n get_img)
