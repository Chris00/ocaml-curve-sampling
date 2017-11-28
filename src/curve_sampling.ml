open Printf
open Gg


type t = {
    n: int; (* Number of samples *)
    xy : p2 list; (* FIXME *)
  }

let cut_point = P2.v nan nan

let[@inline] is_cut p = Float.is_nan (P2.x p) || Float.is_nan (P2.y p)

(* FIXME: if several consecutive elements are not printable, do we put
   1 or several empty lines? *)
let p2_to_channel fh v =
  if is_cut v then
    output_char fh '\n'
  else
    fprintf fh "%e\t%e\n" (P2.x v) (P2.y v)

let to_channel fh t =
  List.iter (p2_to_channel fh) t.xy

let to_file fname t =
  let fh = open_out fname in
  to_channel fh t;
  close_out fh

let to_list t =
  List.map (fun v -> (P2.x v, P2.y v)) t.xy

let to_array t =
  let x = Array.make t.n 0. and y = Array.make t.n 0. in
  List.iteri (fun i v -> x.(i) <- P2.x v; y.(i) <- P2.y v) t.xy;
  x,y

(* Uniform *)

let uniform ?(n=100) f a b =
  let dx = (b -. a) /. float(n-1) in
  let l = ref [] in
  for i = n - 1 downto 0 do
    let x = a +. float i *. dx in
    l := P2.v x (f x) :: !l
  done;
  { n;  xy = !l }


let tr m t =
  { n = t.n;
    xy = List.map (P2.tr m) t.xy }

let of_path p =
  let n = ref 0 in
  let xy = List.map (fun (x,y) -> incr n; P2.v x y) p in
  { n = !n;  xy }

let concat ?(join=false) t1 t2 =
  let xy = if join then t1.xy @ t2.xy (* FIXME: want tail rec? *)
           else t1.xy @ (cut_point :: t2.xy) in
  { n = t1.n + t2.n;  xy }


(** Generic box clipping *)

let clip t b =
  if Box2.is_empty b then invalid_arg "Curve_sampling.crop: empty box";
  let xmin = Box2.minx b and ymin = Box2.miny b in
  let xmax = Box2.maxx b and ymax = Box2.maxy b in
  let path = ref t.xy in
  let clipped = ref [] in
  (* Use Liang–Barsky algorithm.  Use an imperative algorithm to allow
     the float unboxing optimization to be triggered. *)
  let continue = ref true in
  let t0 = ref 0. and t1 = ref 1. in (* param of each segment *)
  let last_pt_known_inside = ref false in
  (* `→ If we know that the previous point is inside the box. *)
  while !continue do
    match !path with
    | p0 :: ((p1 :: ttl) as tl) ->
       if is_cut p0 then (
         last_pt_known_inside := false;
         clipped := cut_point :: !clipped;
         path := tl
       )
       else if is_cut p1 then (
         last_pt_known_inside := false;
         clipped := cut_point :: !clipped;
         path := ttl
       )
       else if !last_pt_known_inside then (
         (* We know [p0] is inside the box [b] as was ALREADY ADDED to
            the [clipped] path.  This is a particular case of the
            general one below in which we know from previous
            computations that t0 = 0. *)
         t1 := 1.;
         (* Coordinate X. *)
         let x0 = P2.x p0 and x1 = P2.x p1 in
         let dx = x1 -. x0 in
         if dx = 0. then () (* x0 = x1 is known inside *)
         else if dx > 0. (* x0 < x1 *) then (
           let r1 = (xmax -. x0) /. dx in (* r0 ≤ r1 *)
           if r1 < !t0 then t1 := -1. (* drop segment *)
           else (if r1 < !t1 then t1 := r1)
         )
         else (* dx < 0 i.e., x0 > x1 *) (
           let r1 = (xmin -. x0) /. dx in
           if r1 < !t0 then t1 := -1. (* drop segment *)
           else (if r1 < !t1 then t1 := r1)
         );
         let y0 = P2.y p0 and y1 = P2.y p1 in
         let dy = y1 -. y0 in
         if !t1 >= 0. (* segment not dropped *) then (
           (* Treat coordinate Y. *)
           if dy = 0. then () (* y0 = y1 known to be inside *)
           else if dy > 0. then (
             let r1 = (ymax -. y0) /. dy in (* 0 ≤ r1 *)
             if r1 < !t0 then t1 := -1. (* drop segment *)
             else (if r1 < !t1 then t1 := r1)
           )
           else (* dy < 0. *) (
             let r1 = (ymin -. y0) /. dy in
             if r1 < !t0 then t1 := -1. (* drop segment *)
             else (if r1 < !t1 then t1 := r1)
           )
         );
         if !t1 >= 0. (* segment not dropped *) then (
           let p1 = if !t1 = 1. then p1 (* last_pt_known_inside = true *)
                    else (last_pt_known_inside := false;
                          P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy)) in
           clipped := p1 :: !clipped; (* [p0] previously added. *)
           if not !last_pt_known_inside then
             clipped := cut_point :: !clipped;
         )
         else last_pt_known_inside := false; (* whole segment dropped *)
         path := tl
       )
       else (
         (* General case *)
         t0 := 0.;  t1 := 1.;  (* convention: t1 < 0 ⇒ dropped segment *)
         (* Coordinate X. *)
         let x0 = P2.x p0 and x1 = P2.x p1 in
         let dx = x1 -. x0 in
         if dx = 0. then (
           if x0 < xmin || x0 > xmax then t1 := -1.; (* drop segment *)
         )
         else if dx > 0. (* x0 < x1 *) then (
           let r0 = (xmin -. x0) /. dx in
           let r1 = (xmax -. x0) /. dx in (* r0 ≤ r1 *)
           if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
           else (if r0 > !t0 then t0 := r0;
                 if r1 < !t1 then t1 := r1; )
         )
         else (* dx < 0 i.e., x0 > x1 *) (
           let r0 = (xmax -. x0) /. dx in
           let r1 = (xmin -. x0) /. dx in
           if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
           else (if r0 > !t0 then t0 := r0;
                 if r1 < !t1 then t1 := r1; )
         );
         let y0 = P2.y p0 and y1 = P2.y p1 in
         let dy = y1 -. y0 in
         if !t1 >= 0. (* segment not dropped *) then (
           (* Treat coordinate Y. *)
           if dy = 0. (* y0 = y1 *) then (
             if y0 < ymin || y0 > ymax then t1 := -1.; (* drop segment *)
           )
           else if dy > 0. (* i.e., y0 < y1 *) then (
             let r0 = (ymin -. y0) /. dy in
             let r1 = (ymax -. y0) /. dy in (* r0 ≤ r1 *)
             if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
             else (if r0 > !t0 then t0 := r0;
                   if r1 < !t1 then t1 := r1)
           )
           else (* dy < 0. i.e., y0 > y1 *) (
             let r0 = (ymax -. y0) /. dy in
             let r1 = (ymin -. y0) /. dy in
             if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
             else (if r0 > !t0 then t0 := r0;
                   if r1 < !t1 then t1 := r1)
           )
         );
         if !t1 >= 0. (* segment not dropped *) then (
           let p0 = if !t0 = 0. then p0
                    else P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy) in
           let p1 = if !t1 = 1. then (last_pt_known_inside := true;
                                      p1)
                    else P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy) in
           clipped := p1 :: p0 :: !clipped;
           if not !last_pt_known_inside then
             clipped := cut_point :: !clipped;
         );
         (* last_pt_known_inside remains false if segment dropped.
            In this case a [cut_point] was already added. *)
         path := tl
       )
    | [_] | [] ->
       (* The previous point was already added to the clipped list (or
          was removed if outside the box). *)
       continue := false
  done;
  (* FIXME: revert list and compute [n] at the same time. *)
  { n = List.length !clipped;
    xy = List.rev !clipped }

(* Adaptive sampling 2D *)



module P2 = struct
  let uniform ?(n=100) f a b =
    let dx = (b -. a) /. float(n-1) in
    let l = ref[] in
    for i = n - 1 downto 1 do
      l := f(a +. float i *. dx) :: !l
    done;
    { n;  xy = !l }

  let of_path xy =
    { n = List.length xy;  xy }

  let to_list t = t.xy

end

;;
