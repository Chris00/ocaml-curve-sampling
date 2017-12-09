open Printf
open Gg

type valid = P0 | P1 | Both

(* Many of the algorithms below require to traverse the list of
   segments in an ordered fashion.  Instead of sorting the segments
   each time, one could link them (they thus cannot be used in another
   sampling). *)
type segment = {
    t0: float; (* must be finite *)
    p0: P2.t;
    t1: float; (* must be finite; t1 ≠ t0 *)
    p1: P2.t;
    valid: valid; (* which of the two points is finite *)
    cost: float; (* absolute cost *)
  }

type t = {
    seg: segment PQ.t; (* DISJOINT segments, all t0 & t1 in same order *)
    viewport: Box2.t; (* Viewing area ⇒ threshold for the cost *)
    viewport_given: bool;
  }


let[@inline] is_finite_float x = match classify_float x with
  | FP_normal | FP_zero | FP_subnormal -> true
  | FP_infinite | FP_nan -> false

let[@inline] is_finite p =
  is_finite_float (P2.x p) && is_finite_float (P2.y p)

(* [t_prev] is NaN if the previous point was invalid.  [cut] is
   applied for any path interruption. *)
let rec fold_points_sorted_segments ~t_prev f ~cut acc = function
  | [] -> acc
  | s :: tl ->
     match s.valid with
     | P0 ->
        let acc = if s.t0 = t_prev then acc else f acc s.p0 in
        let acc = cut acc in (* p1 invalid ⇒ path interrupted *)
        fold_points_sorted_segments ~t_prev f ~cut acc tl
     | P1 ->
        let acc = cut acc in (* for p0 *)
        fold_points_sorted_segments ~t_prev:s.t1 f ~cut (f acc s.p1) tl
     | Both ->
        let acc = if s.t0 = t_prev then acc else f acc s.p0 in
        fold_points_sorted_segments ~t_prev:s.t1 f ~cut (f acc s.p1) tl

(** Sort segments by value of [t0]. *)
let compare_seg s1 s2 = Float.compare s1.t0 s2.t0
let compare_decr_seg s1 s2 = Float.compare s2.t0 s1.t0

(** [fold t ~init f] fold [f] once on each valid point.  The points
   are passed in the order of the curve. *)
let fold_points t ~init ~cut f =
  let seg = PQ.fold t.seg ~init:[] (fun l s -> s :: l) in
  let seg = List.sort compare_seg seg in
  fold_points_sorted_segments ~t_prev:nan f ~cut init seg

(** Same as [fold] but the points are passed in the opposite order of
   the curve. *)
let fold_points_decr t ~init ~cut f =
  let seg = PQ.fold t.seg ~init:[] (fun l s -> s :: l) in
  let seg = List.sort compare_decr_seg seg in
  fold_points_sorted_segments ~t_prev:nan f ~cut init seg

(** Iterate [f] on all segments of which BOTH endpoints are VALID.
   The order in which the segments are passed to [f] is unspecified. *)
let iter_segments t f =
  PQ.iter t.seg (fun s -> match s.valid with
                          | Both -> f s.p0 s.p1
                          | P0 | P1 -> ())

(** Save *)

let to_channel t fh =
  fold_points t ~init:()
    (fun () p -> fprintf fh "%e\t%e\n" (P2.x p) (P2.y p))
    ~cut:(fun () -> output_char fh '\n')

let to_file t fname =
  let fh = open_out fname in
  to_channel t fh;
  close_out fh

let to_latex t fname =
  let fh = open_out fname in
  output_string fh "% Written by OCaml Curve_sampling %%VERSION%%\n";
  iter_segments t (fun p0 p1 ->
      fprintf fh "\\pgfpathmoveto{\\pgfpointxy{%.16f}{%.16f}}\n\
                  \\pgfpathlineto{\\pgfpointxy{%.16f}{%.16g}}"
        (P2.x p0) (P2.y p0) (P2.x p1) (P2.y p1));
  close_out fh

let to_list t =
  let path, seg = fold_points_decr t ~init:([], [])
    (fun (path, seg) p -> (path, (P2.x p, P2.y p) :: seg))
    ~cut:(fun (path, seg) -> (seg :: path, [])) in
  if seg <> [] then seg :: path else path


let tr_segment m s = {
    t0 = s.t0;
    p0 = P2.tr m s.p0;
    t1 = s.t1;
    p1 = P2.tr m s.p1;
    valid = s.valid;
    cost = s.cost;
  }

let tr m t =
  { seg = PQ.map t.seg (tr_segment m);
    viewport = t.viewport;
    viewport_given = t.viewport_given }

(** Generic box clipping *)

(* Since the segments will be presented in an unknown order, we cannon
   use information about the previous point.  *)
let clip_segment b s =
  (* Use Liang–Barsky algorithm. *)
  match s.valid with
  | P0 -> if Box2.mem s.p0 b then Some s else None
  | P1 -> if Box2.mem s.p1 b then Some s else None
  | Both ->
     (* FIXME: what about infinite coordinates? *)
     let t0 = ref 0. in
     let t1 = ref 1. in  (* convention: t1 < 0 ⇒ drop segment *)
     (* Coordinate X. *)
     let x0 = P2.x s.p0 and x1 = P2.x s.p1 in
     let dx = x1 -. x0 in
     if dx = 0. then (
       if x0 < Box2.minx b || x0 > Box2.maxx b then t1 := -1.; (* drop [s] *)
     )
     else if dx > 0. (* x0 < x1 *) then (
       let r0 = (Box2.minx b -. x0) /. dx in
       let r1 = (Box2.maxx b -. x0) /. dx in (* r0 ≤ r1 *)
       if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment [s] *)
       else (if r0 > !t0 then t0 := r0;
             if r1 < !t1 then t1 := r1; )
     )
     else (* dx < 0 i.e., x0 > x1 *) (
       let r0 = (Box2.maxx b -. x0) /. dx in
       let r1 = (Box2.minx b -. x0) /. dx in
       if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
       else (if r0 > !t0 then t0 := r0;
             if r1 < !t1 then t1 := r1; )
     );
     let y0 = P2.y s.p0 and y1 = P2.y s.p1 in
     let dy = y1 -. y0 in
     if !t1 >= 0. (* segment not dropped *) then (
       (* Treat coordinate Y. *)
       if dy = 0. (* y0 = y1 *) then (
         if y0 < Box2.miny b || y0 > Box2.maxy b then t1 := -1.; (* drop [s] *)
       )
       else if dy > 0. (* i.e., y0 < y1 *) then (
         let r0 = (Box2.miny b -. y0) /. dy in
         let r1 = (Box2.maxy b -. y0) /. dy in (* r0 ≤ r1 *)
         if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
         else (if r0 > !t0 then t0 := r0;
               if r1 < !t1 then t1 := r1)
       )
       else (* dy < 0. i.e., y0 > y1 *) (
         let r0 = (Box2.maxy b -. y0) /. dy in
         let r1 = (Box2.miny b -. y0) /. dy in
         if r0 > !t1 || r1 < !t0 then t1 := -1. (* drop segment *)
         else (if r0 > !t0 then t0 := r0;
               if r1 < !t1 then t1 := r1)
       )
     );
     if !t1 >= 0. (* segment not dropped *) then (
       (* FIXME: The values of [t0] and [t1] are only linear
          estimates.  Is it a problem for refinement of the sampling? *)
       let s' =
         if !t0 = 0. then
           if !t1 = 1. then s
           else { t0 = s.t0;  p0 = s.p0;
                  t1 = s.t0 +. !t1 *. (s.t1 -. s.t0);
                  p1 = P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy);
                  valid = Both;  cost = s.cost}
         else
           if !t1 = 1. then
             { t0 = s.t0 +. !t0 *. (s.t1 -. s.t0);
               p0 = P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy);
               t1 = s.t1;  p1 = s.p1;
               valid = Both;  cost = s.cost }
           else
             let ds = s.t1 -. s.t0 in
             { t0 = s.t0 +. !t0 *. ds;
               p0 = P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy);
               t1 = s.t0 +. !t1 *. ds;
               p1 = P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy);
               valid = Both;  cost = s.cost } in
       Some s'
     )
     else None

let clip t b =
  if Box2.is_empty b then invalid_arg "Curve_sampling.crop: empty box";
  { seg = PQ.filter_map t.seg (clip_segment b);
    viewport = t.viewport;
    viewport_given = t.viewport_given }

(** Sub-module using Gg point representation. *)
module P2 = struct

  let uniform_unsafe ?(n=100) ?viewport f a b =
    (* [a] and [b] assumed to be finite. *)
    let dt = (b -. a) /. float(n-1) in
    let pb = f b in
    let prev_t = ref b in
    let prev_pt = ref pb in
    let prev_valid = ref (is_finite pb) in
    let seg = ref PQ.empty in
    for i = n - 2 downto 0 do
      let t = a +. float i *. dt in
      let p = f t in
      let valid = is_finite p in
      (* FIXME: compute costs (on demand??) *)
      if !prev_valid || valid then (
        let s = { t0 = t;  p0 = p;  t1 = !prev_t;  p1 = !prev_pt;
                  valid = (if !prev_valid then
                             if valid then Both else P1
                           else (* valid *) P0);
                  cost = 0. } in
        seg := PQ.add !seg 0. s;
      );
      prev_t := t;
      prev_pt := p;
      prev_valid := valid;
    done;
    let viewport, viewport_given = match viewport with
      | None -> Box2.unit, false
      | Some v -> v, true in
    { seg = !seg;  viewport;  viewport_given }

  let uniform ?n ?viewport f a b =
    if not(is_finite_float a && is_finite_float b) then
      invalid_arg "Curve_sampling.P2.uniform: the endpoints a and b \
                   must be finite";
    uniform_unsafe ?n ?viewport f a b


  let rec rm_invalid_prefix i0 = function
    | [] -> ([], i0)
    | (p :: tl) as l ->
       if is_finite p then (l, i0) else rm_invalid_prefix (succ i0) tl

  let rec segments_of_path q p0 i0 = function
    | [] -> q
    | p1 :: tl ->
       let i1 = succ i0 in
       if is_finite p1 then
         let s = { t0 = float i0;  p0;  t1 = float i1;  p1;
                   valid = Both;  cost = 0. } in
         segments_of_path (PQ.add q 0. s) p1 i1 tl
       else (* remove p1 *)
         let l, i0 = rm_invalid_prefix (succ i1) tl in
         match l with
         | [] -> q
         | p0 :: tl -> segments_of_path q p0 i0 tl

  let of_path p =
    let seg, i0 = rm_invalid_prefix 0 p in
    let seg = match seg with
      | [] | [ _ ] -> PQ.empty
      | p0 :: tl -> segments_of_path PQ.empty p0 i0 tl in
    { seg;
      viewport = Box2.unit;
      viewport_given = false }

  type point_or_cut = Point of P2.t | Cut

  let to_list t =
    fold_points_decr t ~init:[]
      (fun l p -> Point p :: l)
      ~cut:(fun l -> Cut :: l)

end

let p2_of_couple (x,y) = Gg.P2.v x y

let of_path p = P2.of_path (List.map p2_of_couple p)

;;
