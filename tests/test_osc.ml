open Gg

let () =
  let f x = if x = 0. then 0. else x *. sin (1. /. x) in
  let xmin = -0.4 and xmax = 0.4 in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "test_osc0.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:200 in
  Curve_sampling.to_file t "test_osc.dat"
