open Gg

let () =
  let b = Box2.v (P2.v 0. 0.) (Size2.v 1. 1.) in
  let t = Curve_sampling.of_path
            [(0., -0.5); (1.5, 1.); (0.2, 0.5); (0.3, 1.5); (1., 0.6);
             (nan, nan); (-0.5, 0.5); (-1., 0.); (0.5, 0.5)] in
  Curve_sampling.to_file t "clip0.dat";
  Curve_sampling.to_latex t "clip0.tex";
  let t1 = Curve_sampling.clip t b in
  Curve_sampling.to_file t1 "clip1.dat";
  Curve_sampling.to_latex t1 "clip1.tex"
