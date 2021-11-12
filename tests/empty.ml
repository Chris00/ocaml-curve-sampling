module C = Curve_sampling

(* Produce empty samplings *)

let () =
  (try ignore(C.fn (fun _ -> 1.) 0. 0.);
       (* Expected to say that the interval is empty. *)
       assert false
   with Invalid_argument _ -> ());

  let s = C.fn (fun _ -> nan) 0. 1. in
  assert(C.is_empty s);

  let s = C.fn (fun x -> if x = 0. || x = 1. then x else nan) 0. 1. in
  assert(List.length (C.to_list s) = 2);

  let s = C.param (fun _ -> (nan, nan)) 0. 1. in
  assert(C.is_empty s)


