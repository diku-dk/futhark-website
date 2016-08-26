-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/hotspot/hotspot_openmp.cpp
--
-- ==
-- tags { futhark-c futhark-opencl }
-- compiled input @ data/64.in
-- output @ data/64.out
--
-- compiled input @ data/512.in
-- output @ data/512.out
--
-- notravis input @ data/1024.in
-- output @ data/1024.out

default(f32)

fun str_size(): int = 256

-- Maximum power density possible (say 300W for a 10mm x 10mm chip)
fun max_pd(): f32 = 3.0e6

-- Required precision in degrees
fun precision(): f32 = 0.001

fun spec_heat_si(): f32 = 1.75e6

fun k_si(): f32 = 100.0

-- Capacitance fitting factor
fun factor_chip(): f32 = 0.5

-- Chip parameters
fun t_chip(): f32 = 0.0005
fun chip_height(): f32 = 0.016
fun chip_width(): f32 = 0.016

-- Ambient temperature assuming no package at all
fun amb_temp(): f32 = 80.0

-- Single iteration of the transient solver in the grid model.
-- advances the solution of the discretized difference equations by
-- one time step
fun single_iteration(temp: [row][col]f32, power: [row][col]f32,
                              Cap: f32, Rx: f32, Ry: f32, Rz: f32,
                              step: f32): [][]f32 =
  map (fn (r: int): []f32  =>
         map(fn (c: int): f32  =>
               let delta =
                 (step / Cap) *
               (power[r,c] +
                unsafe
                  (if r == 0 && c == 0 then -- Corner 1
                     (temp[r,c+1] - temp[r,c]) / Rx +
                     (temp[r+1,c] - temp[r,c]) / Ry
                   else if r == 0 && c == col-1 then -- Corner 2
                     (temp[r,c-1] - temp[r,c]) / Rx +
                     (temp[r+1,c] - temp[r,c]) / Ry
                   else if r == row-1 && c == col-1 then -- Corner 3
                     (temp[r,c-1] - temp[r,c]) / Rx +
                     (temp[r-1,c] - temp[r,c]) / Ry
                   else if r == row-1 && c == 0 then -- Corner 4
                     (temp[r,c+1] - temp[r,c]) / Rx +
                     (temp[r-1,c] - temp[r,c]) / Ry
                   else if r == 0 then -- Edge 1
                     (temp[r,c+1] + temp[r,c-1] - 2.0*temp[r,c]) / Rx +
                     (temp[r+1,c] - temp[r,c]) / Ry
                   else if c == col-1 then -- Edge 2
                     (temp[r,c-1] - temp[r,c]) / Rx +
                     (temp[r+1,c] + temp[r-1,c] - 2.0*temp[r,c]) / Ry
                   else if r == row-1 then -- Edge 3
                     (temp[r,c+1] + temp[r,c-1] - 2.0*temp[r,c]) / Rx +
                     (temp[r-1,c] - temp[r,c]) / Ry
                   else if c == 0 then -- Edge 4
                     (temp[r,c+1] - temp[r,c]) / Rx +
                     (temp[r+1,c] + temp[r-1,c] - 2.0*temp[r,c]) / Ry
                   else
                     (temp[r,c+1] + temp[r,c-1] - 2.0 * temp[r,c]) / Rx +
                     (temp[r+1,c] + temp[r-1,c] - 2.0 * temp[r,c]) / Ry) +
                  (amb_temp() - temp[r,c]) / Rz) in
               temp[r,c] + delta
            , iota(col)),
         iota(row))

-- Transient solver driver routine: simply converts the heat transfer
-- differential equations to difference equations and solves the
-- difference equations by iterating
fun compute_tran_temp(num_iterations: int, temp: []row[col]f32, power: []row[col]f32): [][]f32 =
  let grid_height = chip_height() / f32(row) in
  let grid_width = chip_width() / f32(col) in
  let Cap = factor_chip() * spec_heat_si() * t_chip() * grid_width * grid_height in
  let Rx = grid_width / (2.0 * k_si() * t_chip() * grid_height) in
  let Ry = grid_height / (2.0 * k_si() * t_chip() * grid_width) in
  let Rz = t_chip() / (k_si() * grid_height * grid_width) in
  let max_slope = max_pd() / (factor_chip() * t_chip() * spec_heat_si()) in
  let step = precision() / max_slope in
  loop (temp) = for i < num_iterations do
    single_iteration(temp, power, Cap, Rx, Ry, Rz, step) in
  temp

fun main(num_iterations: int, temp: [row][col]f32, power: [row][col]f32): [][]f32 =
  compute_tran_temp(num_iterations, temp, power)
