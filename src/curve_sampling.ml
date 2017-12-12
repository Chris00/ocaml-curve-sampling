open Printf
open Gg

let () = Random.self_init()

type valid = P0 | P1 | Both
(* Which endpoint of a segment are valid (= both coordinate are
   finite).  It is important to keep segments with at least a valid
   endpoint to refine them and detect the boundary of the domain. *)

(* Many of the algorithms below require to traverse the list of
   segments in an ordered fashion.  Instead of sorting the segments
   each time, one could link them (they thus cannot be used in another
   sampling). *)
type segment = {
    t0: float; (* must be finite *)
    p0: P2.t;
    t1: float; (* must be finite; t1 > t0 *)
    p1: P2.t;
    valid: valid; (* which of the two points is finite *)
  }

type t = {
    seg: segment PQ.t; (* DISJOINT segments, all t0 & t1 in same order *)
    viewport: Box2.t; (* Viewing area ⇒ threshold for the cost *)
  }


let[@inline] is_finite_float x = match classify_float x with
  | FP_normal | FP_zero | FP_subnormal -> true
  | FP_infinite | FP_nan -> false

let[@inline] is_finite p =
  is_finite_float (P2.x p) && is_finite_float (P2.y p)

(* [prev_t] is NaN if the previous point was invalid (and so a [cut]
   was already executed).  [cut] is applied for any path interruption. *)
let rec fold_points_incr_segments ~prev_t f ~cut acc = function
  | [] -> acc
  | s :: tl ->
     match s.valid with
     | P0 -> (* p1 invalid ⇒ path interrupted *)
        let acc = if s.t0 = prev_t then cut acc (* p1 *)
                  else if Float.is_nan prev_t then f acc s.p0
                  else let acc = cut acc in f acc s.p0 in
        fold_points_incr_segments ~prev_t:nan f ~cut acc tl
     | P1 ->
        let acc = if Float.is_nan prev_t then acc else cut acc in
        fold_points_incr_segments ~prev_t:s.t1 f ~cut (f acc s.p1) tl
     | Both ->
        let acc = if s.t0 = prev_t then acc
                  else if Float.is_nan prev_t then f acc s.p0
                  else let acc = cut acc in f acc s.p0 in
        fold_points_incr_segments ~prev_t:s.t1 f ~cut (f acc s.p1) tl

let rec fold_points_decr_segments ~prev_t f ~cut acc = function
  | [] -> acc
  | s :: tl ->
     match s.valid with
     | P0 -> (* p1 invalid ⇒ path interrupted *)
        let acc = if Float.is_nan prev_t then acc else cut acc in
        fold_points_decr_segments ~prev_t:s.t0 f ~cut (f acc s.p0) tl
     | P1 ->
        let acc = if s.t1 = prev_t then acc
                  else if Float.is_nan prev_t then f acc s.p1
                  else let acc = cut acc in f acc s.p1 in
        fold_points_decr_segments ~prev_t:nan f ~cut acc tl
     | Both ->
        let acc = if s.t1 = prev_t then acc
                  else if Float.is_nan prev_t then f acc s.p1
                  else let acc = cut acc in f acc s.p1 in
        fold_points_decr_segments ~prev_t:s.t0 f ~cut (f acc s.p0) tl

(** Sort segments by value of [t0]. *)
let compare_seg s1 s2 = Float.compare s1.t0 s2.t0
let compare_decr_seg s1 s2 = Float.compare s2.t0 s1.t0

(** [fold t ~init f] fold [f] once on each valid point.  The points
   are passed in the order of the curve. *)
let fold_points t ~init ~cut f =
  let seg = PQ.fold t.seg ~init:[] (fun l s -> s :: l) in
  let seg = List.sort compare_seg seg in
  fold_points_incr_segments ~prev_t:nan f ~cut init seg

(** Same as [fold] but the points are passed in the opposite order of
   the curve. *)
let fold_points_decr t ~init ~cut f =
  let seg = PQ.fold t.seg ~init:[] (fun l s -> s :: l) in
  let seg = List.sort compare_decr_seg seg in
  fold_points_decr_segments ~prev_t:nan f ~cut init seg

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
  output_string fh "% Written by OCaml Curve_sampling (version %%VERSION%%)\n";
  output_string fh "\\begin{pgfscope}\n";
  iter_segments t (fun p0 p1 ->
      fprintf fh "\\pgfpathmoveto{\\pgfpointxy{%.16f}{%.16f}}\n\
                  \\pgfpathlineto{\\pgfpointxy{%.16f}{%.16f}}\n\
                  \\pgfusepath{stroke}\n"
        (P2.x p0) (P2.y p0) (P2.x p1) (P2.y p1));
  output_string fh "\\end{pgfscope}\n";
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
  }

let tr m t =
  { seg = PQ.map t.seg (tr_segment m);
    viewport = t.viewport }

(** Generic box clipping *)

(* Since the segments will be presented in an unknown order, we cannot
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
                  valid = Both }
         else
           if !t1 = 1. then
             { t0 = s.t0 +. !t0 *. (s.t1 -. s.t0);
               p0 = P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy);
               t1 = s.t1;  p1 = s.p1;
               valid = Both }
           else
             let ds = s.t1 -. s.t0 in
             { t0 = s.t0 +. !t0 *. ds;
               p0 = P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy);
               t1 = s.t0 +. !t1 *. ds;
               p1 = P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy);
               valid = Both } in
       Some s'
     )
     else None

let clip t b =
  if Box2.is_empty b then invalid_arg "Curve_sampling.crop: empty box";
  { seg = PQ.filter_map t.seg (clip_segment b);
    viewport = t.viewport }

module Cost = struct

  let angle_dist vp p1 pm p2 =
    let dx1m = P2.x p1 -. P2.x pm and dy1m = P2.y p1 -. P2.y pm in
    let dx2m = P2.x p2 -. P2.x pm and dy2m = P2.y p2 -. P2.y pm in
    let dx12 = P2.x p1 -. P2.x p2 and dy12 = P2.y p1 -. P2.y p2 in
    let sq_d1m = dx1m *. dx1m +. dy1m *. dy1m
    and sq_d2m = dx2m *. dx2m +. dy2m *. dy2m
    and sq_d12 = dx12 *. dx12 +. dy12 *. dy12 in
    let cos_m = (sq_d1m +. sq_d2m -. sq_d12) /. (2. *. sqrt sq_d1m *. sqrt sq_d2m) in
    let hw = Box2.w vp +. Box2.h vp in
    let rel_dist = (sq_d1m +. sq_d2m) /. (hw *. hw) in
    (* rel_dist *. sqrt(cos_m +. 1.) (\* ~ π - acos cos_m *\) *)
    if abs_float cos_m <= 1. then rel_dist *. (Float.pi -. acos cos_m)
    else rel_dist *. Float.pi

  let dist_line viewport p1 pm p2 =
    let dx21 = P2.x p2 -. P2.x p1 and dy21 = P2.y p2 -. P2.y p1 in
    let c = P2.x p2 *. P2.y p1 -. P2.y p2 *. P2.x p1 in
    abs_float(dy21 *. P2.x pm -. dx21 *. P2.y pm +. c) /. hypot dy21 dx21

  let estimate viewport t1 p1 tm pm t2 p2 =
    dist_line viewport p1 pm p2

end


(** Construct an initial sampling.  An external iterators will provide
   the points in INCREASING order of the curve parameter and then
   close the path to get the initial sampling. *)
module Init = struct
  (* Mutable state needed to construct the initial sampling. *)
  type state = {
      mutable pprev_t: float; (* antepenultimate t, always finite *)
      mutable pprev_p: Gg.p2;
      mutable pprev_valid: bool;
      mutable prev_t: float;  (* prev_t > pprev_t ; always finite *)
      mutable prev_p: Gg.p2;
      mutable prev_valid: bool;
      len_t: float; (* length of the interval in which t is. *)
      cost: Box2.t -> float -> p2 -> float -> p2 -> float -> p2 -> float;
      seg: segment PQ.t; (* the priority queue, so far *)
      vp: Box2.t;
    }

  let last_t st = st.prev_t (* last time [add]ed. *)

  let make ~cost ~viewport ~len_t t0 p0 =
    { pprev_t = t0;  pprev_p = p0;  pprev_valid = false;
      prev_t = t0;   prev_p = p0;  prev_valid = is_finite p0;
      len_t;  cost;
      seg = PQ.make();
      vp = viewport }

  (* [t] is assumed to be finite and >= [st.prev_t]. *)
  let add st t p =
    (* If a point is duplicated, ignore the new instance. *)
    if t > st.prev_t then (
      let valid = is_finite p in
      if valid then
        (* [p] valid. *)
        if st.prev_valid then
          let c = if st.pprev_valid then
                    st.cost st.vp st.pprev_t st.pprev_p st.prev_t st.prev_p t p
                  else max_float (* force explore it *) in
          let s = { t0 = st.prev_t;  p0 = st.prev_p;  t1 = t;  p1 = p;
                    valid = Both } in
          PQ.add st.seg c s
        else (* st.prev not valid *)
          let s = { t0 = st.prev_t;  p0 = st.prev_p;  t1 = t;  p1 = p;
                    valid = P1 } in
          PQ.add st.seg ((t -. st.prev_t) /. st.len_t) s
      else if st.prev_valid then (
        (* Otherwise none of the last 2 points are valid and we drop the
         segment. *)
        let s = { t0 = st.prev_t;  p0 = st.prev_p;  t1 = t;  p1 = p;
                  valid = P0 } in
        PQ.add st.seg ((t -. st.prev_t) /. st.len_t) s
      );
      st.pprev_t <- st.prev_t;
      st.pprev_p <- st.prev_p;
      st.pprev_valid <- st.prev_valid;
      st.prev_t <- t;
      st.prev_p <- p;
      st.prev_valid <- valid
    )

  let segments st = st.seg
end


(** Sub-module using Gg point representation. *)
module P2 = struct

  let uniform_unsafe ~n f a b =
    (* [a] and [b] assumed to be finite and [a] < [b]. *)
    let dt = (b -. a) /. float(n-1) in
    let fa = f a in
    let viewport = Box2.unit in
    let st = Init.make ~cost:Cost.estimate ~viewport ~len_t:(b -. a) a fa in
    for i = 1 to n - 1 do
      let t = a +. float i *. dt in
      let ft = f t in
      Init.add st t ft;
    done;
    { seg = Init.segments st;  viewport }

  let uniform ?(n=100) f a b =
    if not(is_finite_float a && is_finite_float b) then
      invalid_arg "Curve_sampling.P2.uniform: the endpoints a and b \
                   must be finite";
    if a = b then invalid_arg "Curve_sampling.P2.uniform: empty interval";
    if a < b then uniform_unsafe ~n f a b
    else uniform_unsafe ~n f b a


  let rec rm_invalid_prefix i0 = function
    | [] -> ([], i0)
    | (p :: tl) as l ->
       if is_finite p then (l, i0) else rm_invalid_prefix (succ i0) tl

  let rec add_segments_of_path q p0 i0 = function
    | [] -> ()
    | p1 :: tl ->
       let i1 = succ i0 in
       if is_finite p1 then
         let s = { t0 = float i0;  p0;  t1 = float i1;  p1;
                   valid = Both } in
         PQ.add q 0. s;
         add_segments_of_path q p1 i1 tl
       else (* remove p1 *)
         let l, i0 = rm_invalid_prefix (succ i1) tl in
         match l with
         | [] -> ()
         | p0 :: tl -> add_segments_of_path q p0 i0 tl

  let of_path p =
    let p, i0 = rm_invalid_prefix 0 p in
    let seg = PQ.make() in
    (match p with
     | [] | [ _ ] -> ()
     | p0 :: tl -> add_segments_of_path seg p0 i0 tl);
    { seg;  viewport = Box2.unit }

  type point_or_cut = Point of P2.t | Cut

  let to_list t =
    fold_points_decr t ~init:[]
      (fun l p -> Point p :: l)
      ~cut:(fun l -> Cut :: l)

  let[@inline] guess_viewport viewport ~xmin ~xmax ~ymin ~ymax =
    match viewport with
    | None ->
       if is_finite_float xmin && is_finite_float xmax
          && is_finite_float ymin && is_finite_float ymax then
         Box2.v (P2.v xmin ymin)
           (Size2.v (xmax -. xmin) (ymax -. ymin))
       else
         Box2.unit
    | Some v -> v

  let almost_uniform ~n ?viewport f a b =
    (* Assume [a] and [b] are finite and [a] < [b]. *)
    (* Bounding box of initial sampling; to be used as viewport *)
    let xmin = ref infinity in
    let xmax = ref neg_infinity in
    let ymin = ref infinity in
    let ymax = ref neg_infinity in
    let[@inline] add_pt p =
      let x = P2.x p in
      if x < !xmin then xmin := x;  (* ⇒ [x] not NaN *)
      if x > !xmax then xmax := x;
      let y = P2.y p in
      if y < !ymin then ymin := y;
      if y > !ymax then ymax := y in
    let dt = (b -. a) /. float(n-1) in
    let fa = f a in
    add_pt fa;
    let st = Init.make ~cost:Cost.estimate ~viewport:Box2.unit
               ~len_t:(b -. a) a fa in
    for i = 1 to n - 2 do
      (* Slightly randomize points except for the first and last ones. *)
      let t = a +. (float i +. Random.float 0.125 -. 0.0625) *. dt in
      let ft = f t in
      Init.add st t ft;
      add_pt ft;
    done;
    let fb = f b in
    Init.add st b fb;
    add_pt fb;
    let viewport = guess_viewport viewport
                     ~xmin:!xmin ~xmax:!xmax ~ymin:!ymin ~ymax:!ymax in
    { seg = Init.segments st;  viewport }


  let intial_sampling_complete ~n ?viewport f init (a:float) (b:float) =
    (* Try to complete the [init] sampling so as to explore the
       interval [a,b] with almost the same density. *)
    assert(a < b);
    match List.sort (fun (t1,_) (t2,_) -> Float.compare t1 t2) init with
    | [] ->
       uniform_unsafe ~n f a b
    | ((t0, p0) :: tl) as init ->
       (* Bounding box of initial sampling; to be used as viewport *)
       let xmin = ref infinity in
       let xmax = ref neg_infinity in
       let ymin = ref infinity in
       let ymax = ref neg_infinity in
       let[@inline] add_pt p =
         let x = P2.x p in
         if x < !xmin then xmin := x;  (* ⇒ [x] not NaN *)
         if x > !xmax then xmax := x;
         let y = P2.y p in
         if y < !ymin then ymin := y;
         if y > !ymax then ymax := y in
       (* Sort by length of intervals, making sure the entire interval
          [a, b] is covered. *)
       let len_t = b -. a in
       let cost_length _ _t0 _ t1 _ t2 _ = (t2 -. t1) /. len_t in
       let n, t0, p0, tl = if t0 = a then (n, t0, p0, tl)
                           else (n - 1, a, f a, init) in
       add_pt p0;
       let st = Init.make ~cost:cost_length ~viewport:Box2.unit ~len_t t0 p0 in
       List.iter (fun (t, p) -> Init.add st t p; add_pt p) tl;
       let n = ref(if Init.last_t st <> b then (
                     let fb = f b in
                     Init.add st b fb;
                     add_pt fb;
                     n - 1)
                   else n) in
       let seg = Init.segments st in
       (* Insert points to cut the largest intervals.  Here we don't
          care whether the points are valid or not, we just want to
          cover the interval [a,b] well enough. *)
       while !n > 0 do
         let s = PQ.delete_max seg in
         let t = s.t0 +. (0.45 *. Random.float 0.1) *. (s.t1 -. s.t0) in
         let ft = f t in
         decr n;
         add_pt ft;
         let len1 = t -. s.t0 in
         let s1 = { t0 = s.t0;  p0 = s.p0;  t1 = t;  p1 = ft;
                    valid = Both (* don't care *) } in
         let len2 = s.t1 -. t in
         let s2 = { t0 = t;  p0 = ft;  t1 = s.t1;  p1 = s.p1;
                    valid = Both (* don't care *) } in
         PQ.add seg len1 s1;
         PQ.add seg len2 s2
       done;
       (* Guess reasonable viewport from sampling. *)
       let viewport = guess_viewport viewport
                        ~xmin:!xmin ~xmax:!xmax ~ymin:!ymin ~ymax:!ymax in
       (* Now, build the queue based on the curvature costs and check
          the points are valid. *)
       let seg = PQ.fold seg ~init:[] (fun l s -> s :: l) in
       let seg = List.sort compare_seg seg in
       match seg with
       | [] -> assert false (* at least the segment [a,b] *)
       | s :: tl ->
          let st = Init.make ~cost:Cost.estimate ~viewport ~len_t s.t0 s.p0 in
          Init.add st s.t1 s.p1;
          List.iter (fun s -> Init.add st s.t0 s.p0; Init.add st s.t1 s.p1) seg;
          { seg = Init.segments st;  viewport }

  let param_gen name ?(n=100) ?viewport ?(init=[]) ?(init_pt=[])
        (f: float -> Gg.p2) (a: float) (b: float) =
    if not(is_finite_float a && is_finite_float b) then
      invalid_arg(name ^ ": a and b must be finite");
    if a = b then invalid_arg(name ^ ": empty interval [a,b]");
    let a, b = if a < b then a, b else b, a in
    let init_pt = List.filter (fun (t,_) -> a <= t && t <= b) init_pt in
    let init = List.fold_left (fun l t ->
                   if a <= t && t <= b then (t, f t) :: l else l) init_pt init in
    let n0 = truncate(0.1 *. float n) in
    let n0 = if n0 <= 10 then 10 else n0 in
    let init = match init with
      | [] -> almost_uniform ~n:n0 ?viewport f a b
      | _ -> intial_sampling_complete ~n:n0 ?viewport f init a b in
    assert(not(PQ.is_empty init.seg));
    (* to_file init "/tmp/init.dat"; *)
    let seg = init.seg in (* will be mutated but [init] local to this fn *)
    let n = ref (n - n0) in
    while !n > 0 do
      let c0 = PQ.max_priority seg in
      let s = PQ.delete_max seg in
      let t = s.t0 +. (0.45 +. Random.float 0.1) *. (s.t1 -. s.t0) in
      let p = f t in
      decr n;
      let valid = is_finite p in
      match s.valid with
      | Both ->
         if valid then
           let c = Cost.estimate viewport s.t0 s.p0 t p s.t1 s.p1 in
           let s0 = { t0 = s.t0; p0 = s.p0;  t1 = t; p1 = p; valid = Both } in
           let s1 = { t0 = t; p0 = p;  t1 = s.t1; p1 = s.p1; valid = Both } in
           PQ.add seg c s0;
           PQ.add seg c s1
         else
           let s0 = { t0 = s.t0; p0 = s.p0;  t1 = t; p1 = p; valid = P0 } in
           let s1 = { t0 = t; p0 = p;  t1 = s.t1; p1 = s.p1; valid = P1 } in
           let c = 0.5 *. c0 in
           PQ.add seg c s0;
           PQ.add seg c s1
      | P0 ->
         if valid then
           let c = V2.(norm (s.p0 - p)) in
           let s0 = { t0 = s.t0; p0 = s.p0;  t1 = t; p1 = p; valid = Both } in
           let s1 = { t0 = t; p0 = p;  t1 = s.t1; p1 = s.p1; valid = P0 } in
           PQ.add seg c s0;
           PQ.add seg c s1
         else
           let c = 0.5 *. c0 in
           let s0 = { t0 = s.t0; p0 = s.p0;  t1 = t; p1 = p; valid = P0 } in
           (* Both endpoints of [p, p1] are invalid; drop. *)
           PQ.add seg c s0
      | P1 ->
         if valid then
           let c = V2.(norm (s.p1 - p)) in
           let s0 = { t0 = s.t0; p0 = s.p0;  t1 = t; p1 = p; valid = P1 } in
           let s1 = { t0 = t; p0 = p;  t1 = s.t1; p1 = s.p1; valid = Both } in
           PQ.add seg c s0;
           PQ.add seg c s1
         else
           let c = 0.5 *. c0 in
           let s1 = { t0 = t; p0 = p;  t1 = s.t1; p1 = s.p1; valid = P1 } in
           PQ.add seg c s1
    done;
    { seg = seg;  viewport = init.viewport }

  let param ?n ?viewport ?init ?init_pt f a b =
    param_gen "Curve_sampling.P2.param" ?n ?viewport ?init ?init_pt f a b
end

(** Uniform *)

let uniform ?(n=100) f a b =
  if not(is_finite_float a && is_finite_float b) then
    invalid_arg "Curve_sampling.uniform: the endpoints a and b must be finite";
  if a = b then invalid_arg "Curve_sampling.P2.uniform: empty interval";
  let f_p2 x = Gg.P2.v x (f x) in
  if a < b then P2.uniform_unsafe ~n f_p2 a b
  else P2.uniform_unsafe ~n f_p2 b a

let p2_of_couple (x,y) = Gg.P2.v x y

let of_path p = P2.of_path (List.map p2_of_couple p)

(* Adaptive sampling 2D *)

let fn ?n ?viewport ?init ?init_pt f a b =
  let init_pt = match init_pt with
    | Some xy -> Some(List.map (fun (x,y) -> (x, Gg.P2.v x y)) xy)
    | None -> None in
  P2.param_gen "Curve_sampling.fn" ?n ?viewport ?init ?init_pt
    (fun x -> Gg.P2.v x (f x)) a b

let param ?n ?viewport ?init ?init_pt f a b =
  let init_pt = match init_pt with
    | Some txy -> Some(List.map (fun (t,(x,y)) -> (t, Gg.P2.v x y)) txy)
    | None -> None in
  P2.param_gen "Curve_sampling.param" ?n ?viewport ?init ?init_pt
    (fun t -> let x, y = f t in Gg.P2.v x y) a b

;;
