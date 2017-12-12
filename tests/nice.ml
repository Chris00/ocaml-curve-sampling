open Gg

let () =
  let f t = P2.v (cos t) (sin (2. *. t)) in
  let t0 = Curve_sampling.P2.uniform f 0. Float.two_pi ~n:1000 in
  Curve_sampling.to_file t0 "nice0.dat";
  let t = Curve_sampling.P2.param f 0. Float.two_pi in
  Curve_sampling.to_file t "nice.dat"
