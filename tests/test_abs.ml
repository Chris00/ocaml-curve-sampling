open Gg

let () =
  let xmin = -1. and xmax = 1. in
  let f x = if x <= 0.5 then abs_float x else 1. -. x in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "test_abs0.dat";
  let t = Curve_sampling.fn f xmin xmax in
  Curve_sampling.to_file t "test_abs.dat"
