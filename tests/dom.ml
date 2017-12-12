
let () =
  let xmin = -1. and xmax = 2. in
  let f = sqrt in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "dom0.dat";
  let t = Curve_sampling.fn f xmin xmax in
  Curve_sampling.to_file t "dom.dat"
