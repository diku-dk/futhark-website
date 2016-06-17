-- Split the original three-dimensional array into three
-- two-dimensional arrays of floats: one per colour channel.  The
-- elements of the arrays will have a value from 0 to 1.0.
fun ([rows][cols]f32,
     [rows][cols]f32,
     [rows][cols]f32) splitIntoChannels([rows][cols][3]u8 image) =
  -- The maps themselves will return an array of triples, so we use
  -- unzip to turn it into a triple of arrays.  Due to the way the
  -- Futhark compiler represents arrays in the generated code, zip and
  -- unzip are entirely free.
  unzip(map(fn [cols](f32,f32,f32) ([cols][3]u8 row) =>
              map(fn (f32,f32,f32) ([3]u8 pixel) =>
                    (f32(pixel[0]) / 255f32,
                     f32(pixel[1]) / 255f32,
                     f32(pixel[2]) / 255f32),
                  row),
              image))

-- The inverse of splitIntoChannels.
fun [rows][cols][3]u8 combineChannels([rows][cols]f32 rs,
                                         [rows][cols]f32 gs,
                                         [rows][cols]f32 bs) =
  zipWith(fn [cols][3]u8 ([cols]f32 rs_row,
                            [cols]f32 gs_row,
                            [cols]f32 bs_row) =>
            zipWith(fn [3]u8 (f32 r, f32 g, f32 b) =>
                      [u8(r * 255f32),
                       u8(g * 255f32),
                       u8(b * 255f32)],
                    rs_row, gs_row, bs_row),
            rs, gs, bs)

-- Compute the new value for the pixel at the given position.  The
-- pixel must not be located on the edges or an out-of-bounds access
-- will occur.
fun f32 newValue([rows][cols]f32 image, int row, int col) =
  -- The Futhark compiler cannot prove that these accesses are safe,
  -- and cannot perform dynamic bounds checks in parallel code.  We
  -- use the 'unsafe' keyword to elide the bounds checks.  If we did
  -- not do this, the code generator would fail with an error message.
  unsafe
  let sum =
    image[row-1,col-1] + image[row-1,col] + image[row-1,col+1] +
    image[row,  col-1] + image[row,  col] + image[row,  col+1] +
    image[row+1,col-1] + image[row+1,col] + image[row+1,col+1]
  in sum / 9f32

-- The actual stencil: call newValue on every pixel in the interior,
-- leaving the edges unchanged.
fun [rows][cols]f32 blurChannel([rows][cols]f32 channel) =
  map(fn [cols]f32 (int row) =>
        map(fn f32 (int col) =>
              if row > 0 && row < rows-1 && col > 0 && col < cols-1
              then newValue(channel, row, col)
              else channel[row,col],
            iota(cols)),
        iota(rows))

  -- Perform the specified number of blurring operations on the image.
fun [rows][cols][3]u8 main(int iterations, [rows][cols][3]u8 image) =
  -- First we split the image apart into component channels.
  let (rs, gs, bs) = splitIntoChannels(image)
  -- Then we loop 'iterations' times.
  loop ((rs, gs, bs)) = for i < iterations do
    -- Blur each channel by itself.  The Futhark compiler will fuse
    -- these together into just one loop.
    let rs = blurChannel(rs)
    let gs = blurChannel(gs)
    let bs = blurChannel(bs)
    in (rs, gs, bs)
  -- Finally, combine the separate channels back into a single image.
  in combineChannels(rs, gs, bs)
