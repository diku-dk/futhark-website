fun {[[f32,cols],rows],
     [[f32,cols],rows],
     [[f32,cols],rows]} splitIntoChannels([[[u8,3],cols],rows] image) =
  unzip(map(fn [{f32,f32,f32},cols] ([[u8,3],cols] row) =>
              map(fn {f32,f32,f32} ([u8,3] pixel) =>
                    {f32(pixel[0]) / 255f32,
                     f32(pixel[1]) / 255f32,
                     f32(pixel[2]) / 255f32},
                  row),
              image))

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

fun f32 newValue([[f32,cols],rows] image, int row, int col) =
  unsafe
  let sum =
    image[row-1,col-1] + image[row-1,col] + image[row-1,col+1] +
    image[row,  col-1] + image[row,  col] + image[row,  col+1] +
    image[row+1,col-1] + image[row+1,col] + image[row+1,col+1]
  in sum / 9f32

fun [[f32,cols],rows] blurChannel([[f32,cols],rows] channel) =
  map(fn [f32,cols] (int row) =>
        map(fn f32 (int col) =>
              if row > 0 && row < rows-1 && col > 0 && col < cols-1
              then newValue(channel, row, col)
              else channel[row,col],
            iota(cols)),
        iota(rows))

fun [[[u8,3],cols],rows] main(int iterations, [[[u8,3],cols],rows] image) =
  let {rs, gs, bs} = splitIntoChannels(image)
  loop ({rs, gs, bs}) = for i < iterations do
    let rs = blurChannel(rs)
    let gs = blurChannel(gs)
    let bs = blurChannel(bs)
    in {rs, gs, bs}
  in combineChannels(rs, gs, bs)
