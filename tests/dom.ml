open Gg

let () =
  let xmin = -1. and xmax = 2. in
  let f = sqrt in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "dom0.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:50 in
  Curve_sampling.to_file t "dom.dat";

  let f x = if x > 0. then 1. /. x else nan in
  let t0 = Curve_sampling.uniform f 1e-3 xmax ~n:1000 in
  Curve_sampling.to_file t0 "dom1.dat";
  let t = Curve_sampling.fn f xmin xmax in
  Curve_sampling.to_file t "dom2.dat";
  let t = Curve_sampling.fn f xmin xmax
            ~viewport:(Box2.v (P2.v 0. 0.) (Size2.v 2. 3.)) in
  Curve_sampling.to_file t "dom3.dat";

  let f x = 1. /. x in
  let t = Curve_sampling.fn f xmin xmax
            ~viewport:(Box2.v (P2.v (-1.) (-100.)) (Size2.v 3. 200.)) in
  Curve_sampling.to_file t "dom4.dat";
  let t1 = Curve_sampling.fn f xmin xmax in
  Curve_sampling.to_file t1 "dom5.dat";
