-- Split the original three-dimensional array into three
-- two-dimensional arrays of floats: one per colour channel.  The
-- elements of the arrays will have a value from 0 to 1.0.
fun {[[f32,cols],rows],
     [[f32,cols],rows],
     [[f32,cols],rows]} splitIntoChannels([[[u8,3],cols],rows] image) =
  -- The maps themselves will return an array of triples, so we use
  -- unzip to turn it into a triple of arrays.  Due to the way the
  -- Futhark compiler represents arrays in the generated code, zip and
  -- unzip are entirely free.
  unzip(map(fn [{f32,f32,f32},cols] ([[u8,3],cols] row) =>
              map(fn {f32,f32,f32} ([u8,3] pixel) =>
                    {f32(pixel[0]) / 255f32,
                     f32(pixel[1]) / 255f32,
                     f32(pixel[2]) / 255f32},
                  row),
              image))

-- The inverse of splitIntoChannels.
fun [[[u8,3],cols],rows] combineChannels([[f32,cols],rows] rs,
                                         [[f32,cols],rows] gs,
                                         [[f32,cols],rows] bs) =
  zipWith(fn [[u8,3],cols] ([f32,cols] rs_row,
                            [f32,cols] gs_row,
                            [f32,cols] bs_row) =>
            zipWith(fn [u8,3] (f32 r, f32 g, f32 b) =>
                      [u8(r * 255f32),
                       u8(g * 255f32),
                       u8(b * 255f32)],
                    rs_row, gs_row, bs_row),
            rs, gs, bs)

-- Compute the new value for the pixel at the given position.  The
-- pixel must not be located on the edges or an out-of-bounds access
-- will occur.
fun f32 newValue([[f32,cols],rows] image, int row, int col) =
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
fun [[f32,cols],rows] blurChannel([[f32,cols],rows] channel) =
  map(fn [f32,cols] (int row) =>
        map(fn f32 (int col) =>
              if row > 0 && row < rows-1 && col > 0 && col < cols-1
              then newValue(channel, row, col)
              else channel[row,col],
            iota(cols)),
        iota(rows))

  -- Perform the specified number of blurring operations on the image.
fun [[[u8,3],cols],rows] main(int iterations, [[[u8,3],cols],rows] image) =
  -- First we split the image apart into component channels.
  let {rs, gs, bs} = splitIntoChannels(image)
  -- Then we loop 'iterations' times.
  loop ({rs, gs, bs}) = for i < iterations do
    -- Blur each channel by itself.  The Futhark compiler will fuse
    -- these together into just one loop.
    let rs = blurChannel(rs)
    let gs = blurChannel(gs)
    let bs = blurChannel(bs)
    in {rs, gs, bs}
  -- Finally, combine the separate channels back into a single image.
  in combineChannels(rs, gs, bs)
