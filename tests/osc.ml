
let () =
  let f x = if x = 0. then 0. else x *. sin (1. /. x) in
  let xmin = -0.4 and xmax = 0.4 in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "osc0.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:300 in
  Curve_sampling.to_file t "osc.dat";

  let f x = sin (1. /. x) in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "osc1.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:250 in
  Curve_sampling.to_file t "osc2.dat"
