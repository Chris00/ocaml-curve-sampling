open Gg

let () =
  let b = Box2.v (P2.v 0. 0.) (Size2.v 1. 1.) in
  let t = Curve_sampling.of_path
            [(0., -0.5); (1.5, 1.); (0.2, 0.5); (0.3, 1.5); (1., 0.6);
             (nan, nan); (-1., 0.); (0.5, 0.5)] in
  Curve_sampling.to_file t "test_clip0.dat";
  let t1 = Curve_sampling.clip t b in
  Curve_sampling.to_file t1 "test_clip1.dat"
