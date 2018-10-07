open Printf
open Gg

let () = Random.self_init()

type valid = P0 | P1 | Both
(* Which endpoint of a segment are valid (= both coordinate are
   finite).  It is important to keep segments with at least a valid
   endpoint to refine them and detect the boundary of the domain. *)

(* WARNING: Because of mutability, segments may only belong to at most
   one sampling. *)
type segment = {
    t0: float; (* must be finite *)
    p0: P2.t;
    t1: float; (* must be finite; t1 > t0 *)
    p1: P2.t;
    valid: valid; (* which of the two points is finite *)
    mutable cost: float; (* to be able to recreate a queue when
                            traversing segments *)
    (* Segments are ordered by increasing values of [t].  There may be
       jumps however in [t] values. *)
    mutable prev: segment; (* previous segment in curve; oneself if first. *)
    mutable next: segment; (* next segment in curve; oneself if last. *)
    mutable witness: segment PQ.witness option;
  }

let[@inline] is_first s = s.prev == s
let[@inline] is_last s = s.next == s

let dummy_pt = P2.v nan nan
let rec dummy_seg = {
    t0 = nan;  p0 = dummy_pt;  t1 = nan;  p1 = dummy_pt;
    valid = Both;  cost = nan;
    prev = dummy_seg;  next = dummy_seg;  witness = None }

(* Segment LINKED as ROOT. *)
let[@inline] segment ~t0 ~p0 ~t1 ~p1 ~valid ~cost =
  let rec s = { t0;  p0;  t1;  p1;  valid;  cost;  prev = s;  next = s;
                witness = None } in
  s

let add_with_witness pq p s =
  let w = PQ.witness_add pq p s in
  s.witness <- Some w

type t = {
    seg: segment PQ.t; (* DISJOINT segments. *)
    mutable first: segment; (* or dummy if [seg] is empty. *)
    mutable last: segment;  (* or dummy if [seg] is empty. *)
    viewport: Box2.t; (* Viewing area ⇒ threshold for the cost *)
  }

let[@inline] is_empty t = PQ.is_empty t.seg

let make_empty () = {
    seg = PQ.make();  first = dummy_seg;  last = dummy_seg;
    viewport = Box2.unit }

let[@inline] is_finite_float (x: float) = x -. x = 0.

let[@inline] is_finite p =
  is_finite_float (P2.x p) && is_finite_float (P2.y p)

(* [prev_t] is NaN if the previous point was invalid (and so a [cut]
   was already executed).  [cut] is applied for any path interruption. *)
let rec fold_points_incr_segments ~prev_t f ~cut acc seg =
  match seg.valid with
  | P0 -> (* p1 invalid ⇒ path interrupted *)
     let acc = if seg.t0 = prev_t then acc
               else if Float.is_nan prev_t then f acc seg.p0
               else let acc = cut acc in f acc seg.p0 in
     let acc = cut acc in (* p1 *)
     if is_last seg then acc
     else fold_points_incr_segments ~prev_t:nan f ~cut acc seg.next
  | P1 ->
     let acc = if Float.is_nan prev_t then acc else cut acc in
     let acc = f acc seg.p1 in
     if is_last seg then acc
     else fold_points_incr_segments ~prev_t:seg.t1 f ~cut acc seg.next
  | Both ->
     let acc = if seg.t0 = prev_t then acc
               else if Float.is_nan prev_t then f acc seg.p0
               else let acc = cut acc in f acc seg.p0 in
     let acc = f acc seg.p1 in
     if is_last seg then acc
     else fold_points_incr_segments ~prev_t:seg.t1 f ~cut acc seg.next

(** [fold t ~init f] fold [f] once on each valid point.  The points
   are passed in the order of the curve. *)
let fold_points t ~init ~cut f =
  if is_empty t then init
  else fold_points_incr_segments ~prev_t:nan f ~cut init t.first

let rec fold_points_decr_segments ~prev_t f ~cut acc seg =
  match seg.valid with
  | P0 -> (* p1 invalid ⇒ path interrupted *)
     let acc = if Float.is_nan prev_t then acc else cut acc in
     let acc = f acc seg.p0 in
     if is_first seg then acc
     else fold_points_decr_segments ~prev_t:seg.t0 f ~cut acc seg.prev
  | P1 ->
     let acc = if seg.t1 = prev_t then acc
               else if Float.is_nan prev_t then f acc seg.p1
               else let acc = cut acc in f acc seg.p1 in
     let acc = cut acc in (* p0 *)
     if is_first seg then acc
     else fold_points_decr_segments ~prev_t:nan f ~cut acc seg.prev
  | Both ->
     let acc = if seg.t1 = prev_t then acc
               else if Float.is_nan prev_t then f acc seg.p1
               else let acc = cut acc in f acc seg.p1 in
     let acc = f acc seg.p0 in
     if is_first seg then acc
     else fold_points_decr_segments ~prev_t:seg.t0 f ~cut acc seg.prev

(** Same as [fold] but the points are passed in the opposite order of
   the curve. *)
let fold_points_decr t ~init ~cut f =
  if is_empty t then init
  else fold_points_decr_segments ~prev_t:nan f ~cut init t.last


let rec iter_segments s f =
  f s;
  if not(is_last s) then iter_segments s.next f

(** Apply [f] to all segments in the order of the path. *)
let iter t ~f =
  if not(is_empty t) then iter_segments t.first f

let rec map_segments ~seg ~prev_s s f =
  let s' = f s in
  s'.prev <- prev_s;
  prev_s.next <- s';
  add_with_witness seg s'.cost s'; (* allow to change costs *)
  if is_last s then (s'.next <- s'; s')
  else map_segments ~seg ~prev_s:s' s.next f

(** Create a new sampling by applying [f] to all segments.  Changing
   the priority is OK but one must be careful if one changes the
   values of [t0] and [t1] (invariant: [t0] < [t1] and respect
   monotonicity between segments). *)
let map t ~f =
  if is_empty t then make_empty()
  else
    let seg = PQ.make() in (* new fresh queue for new sampling *)
    let first' = f t.first in
    first'.prev <- first';
    if is_last t.first then (
      first'.next <- first';
      { seg;  first = first';  last = first';  viewport = t.viewport }
    )
    else
      let last' = map_segments ~seg ~prev_s:first' t.first.next f in
      { seg;  first = first';  last = last';  viewport = t.viewport }

(** Filter map *)

let rec seek_first_kept s f =
  match f s with
  | None -> if is_last s then None
            else seek_first_kept s.next f
  | Some s' -> Some(s, s')

let rec filter_map_segments ~seg ~prev_s s f =
  match f s with
  | None -> if is_last s then prev_s
            else filter_map_segments ~seg ~prev_s s.next f
  | Some s' ->
     s'.prev <- prev_s;
     prev_s.next <- s';
     add_with_witness seg s'.cost s';
     if is_last s then (s'.next <- s'; s')
     else filter_map_segments ~seg ~prev_s:s' s.next f

let filter_map t ~f =
  if is_empty t then make_empty()
  else
    match seek_first_kept t.first f with
    | None -> make_empty()
    | Some(s, first') ->
       let seg = PQ.make() in
       first'.prev <- first';
       if is_last s then (
         first'.next <- first';
         { seg;  first = first';  last = first';  viewport = t.viewport }
       )
       else
         let last' = filter_map_segments ~seg ~prev_s:first' t.first.next f in
         { seg;  first = first';  last = last';  viewport = t.viewport }

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
  iter t ~f:(fun s ->
      if s.valid = Both then
        fprintf fh "\\pgfpathmoveto{\\pgfpointxy{%.16f}{%.16f}}\n\
                    \\pgfpathlineto{\\pgfpointxy{%.16f}{%.16f}}\n\
                    \\pgfusepath{stroke}\n"
          (P2.x s.p0) (P2.y s.p0) (P2.x s.p1) (P2.y s.p1));
  output_string fh "\\end{pgfscope}\n";
  close_out fh

let to_list t =
  let path, seg = fold_points_decr t ~init:([], [])
    (fun (path, seg) p -> (path, (P2.x p, P2.y p) :: seg))
    ~cut:(fun (path, seg) -> (seg :: path, [])) in
  if seg <> [] then seg :: path else path

(** Transform *)

let tr m t =
  map t ~f:(fun s -> { s with p0 = P2.tr m s.p0; p1 = P2.tr m s.p1 })

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
           else { s with t0 = s.t0;  p0 = s.p0;
                         t1 = s.t0 +. !t1 *. (s.t1 -. s.t0);
                         p1 = P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy);
                         valid = Both }
         else
           if !t1 = 1. then
             { s with t0 = s.t0 +. !t0 *. (s.t1 -. s.t0);
                      p0 = P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy);
                      t1 = s.t1;  p1 = s.p1;
                      valid = Both }
           else
             let ds = s.t1 -. s.t0 in
             { s with t0 = s.t0 +. !t0 *. ds;
                      p0 = P2.v (x0 +. !t0 *. dx) (y0 +. !t0 *. dy);
                      t1 = s.t0 +. !t1 *. ds;
                      p1 = P2.v (x0 +. !t1 *. dx) (y0 +. !t1 *. dy);
                      valid = Both } in
       Some s'
     )
     else None

let clip t b =
  if Box2.is_empty b then invalid_arg "Curve_sampling.crop: empty box";
  filter_map t ~f:(clip_segment b)


module Cost = struct

  let _angle_dist vp p1 pm p2 =
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

  let dist_line _viewport p1 pm p2 =
    let dx21 = P2.x p2 -. P2.x p1 and dy21 = P2.y p2 -. P2.y p1 in
    let c = P2.x p2 *. P2.y p1 -. P2.y p2 *. P2.x p1 in
    abs_float(dy21 *. P2.x pm -. dx21 *. P2.y pm +. c) /. hypot dy21 dx21

  let estimate viewport _t1 p1 _tm pm _t2 p2 =
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
      mutable pprev_valid: bool; (* i.e., [pprev_p] is set & finite *)
      mutable prev_t: float;  (* prev_t > pprev_t ; always finite *)
      mutable prev_p: Gg.p2;
      mutable prev_valid: bool;
      mutable first_seg: segment;
      mutable prev_s: segment; (* last segment created *)
      mutable no_seg_yet: bool;  (* no segment created yet *)
      len_t: float; (* length of the interval in which t is. *)
      cost: Box2.t -> float -> p2 -> float -> p2 -> float -> p2 -> float;
      seg: segment PQ.t; (* the priority queue, so far *)
      vp: Box2.t;
    }

  let last_t st = st.prev_t (* last time [add]ed. *)

  let make ~cost ~viewport ~len_t t0 p0 =
    { pprev_t = t0;  pprev_p = p0;  pprev_valid = false;
      prev_t = t0;   prev_p = p0;  prev_valid = is_finite p0;
      first_seg = dummy_seg;  prev_s = dummy_seg;  no_seg_yet = true;
      len_t;  cost;
      seg = PQ.make();
      vp = viewport }

  (** Add the point (t,p) but do not check that [p] is valid.  Thus the
     segments [s] may contain invalid points and this is not reflected
     by [s.valid].  It may also raise an exn if the cost is NaN. *)
  let add_unsafe st t p =
    if t > st.prev_t then (
      let cost = st.cost st.vp st.pprev_t st.pprev_p st.prev_t st.prev_p t p in
      let s = { t0 = st.prev_t;  p0 = st.prev_p;  t1 = t;  p1 = p;
                valid = Both;  cost;  prev = st.prev_s;  next = dummy_seg;
                witness = None } in
      if st.no_seg_yet then (
        s.prev <- s; (* convention for initial segment *)
        st.first_seg <- s;
        st.no_seg_yet <- false;
      )
      else st.prev_s.next <- s; (* link prev node *)
      add_with_witness st.seg cost s;
      st.prev_s <- s;
      st.pprev_t <- st.prev_t;
      st.pprev_p <- st.prev_p;
      st.pprev_valid <- st.prev_valid;
      st.prev_t <- t;
      st.prev_p <- p;
      st.prev_valid <- true;
    )

  (* [t] is assumed to be finite and >= [st.prev_t]. *)
  let add st t p =
    (* If a point is duplicated, ignore the new instance. *)
    if t > st.prev_t then (
      let valid = is_finite p in
      if valid || st.prev_valid then (
        (* Otherwise none of the last 2 points are valid and we drop
           the segment. *)
        let cost, valid =
          if valid && st.prev_valid then
            ((if st.pprev_valid then
               st.cost st.vp st.pprev_t st.pprev_p st.prev_t st.prev_p t p
             else max_float (* force explore it *) )
            , Both)
          else (* [p] or [st.prev] not valid *)
            ((t -. st.prev_t) /. st.len_t,
             if valid then P1 else P0) in
        let s = { t0 = st.prev_t;  p0 = st.prev_p;  t1 = t;  p1 = p;
                  valid;  cost;  prev = st.prev_s;  next = dummy_seg;
                  witness = None } in
        if st.no_seg_yet then (
          s.prev <- s; (* convention for initial segment *)
          st.first_seg <- s;
          st.no_seg_yet <- false;
        )
        else st.prev_s.next <- s;
        add_with_witness st.seg cost s;
        st.prev_s <- s;
      );
      st.pprev_t <- st.prev_t;
      st.pprev_p <- st.prev_p;
      st.pprev_valid <- st.prev_valid;
      st.prev_t <- t;
      st.prev_p <- p;
      st.prev_valid <- valid
    )

  let unsafe_close ~viewport st =
    st.prev_s.next <- st.prev_s; (* Close the last segment. *)
    { seg = st.seg;  first = st.first_seg;  last = st.prev_s;
      viewport = viewport }

  let close st =
    if st.no_seg_yet then make_empty()
    else unsafe_close ~viewport:st.vp st

  let close_with_viewport ~viewport st =
    if st.no_seg_yet then make_empty()
    else unsafe_close ~viewport st
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
    Init.close st

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

  (* [p0] is at index [i0] and the list starts at index [i0 + 1]. *)
  let rec add_segments_of_path seg ~is_first ~first ~prev_s p0 i0 = function
    | [] ->
       if is_first then make_empty()
       else (
         prev_s.next <- prev_s; (* close last segment *)
         { seg;  first;  last = prev_s;  viewport = Box2.unit }
       )
    | p1 :: tl ->
       let i1 = succ i0 in
       if is_finite p1 then
                       prev = prev_s;  next = dummy_seg;  witness = None } in
         let s = { t0 = float i0;  p0;  t1 = float i1;  p1;
                   valid = Both;  cost = 0.;
         let first = if is_first then (s.prev <- s;  s) else first in
         prev_s.next <- s;
         add_with_witness seg 0. s;
         add_segments_of_path seg ~is_first:false ~first ~prev_s:s p1 i1 tl
       else (* remove p1 and make no segment *)
         let l, i0 = rm_invalid_prefix (succ i1) tl in
         match l with
         | [] -> (* Will terminate the path *)
            add_segments_of_path seg ~is_first ~first ~prev_s p0 i0 l
         | p0 :: tl ->
            add_segments_of_path seg ~is_first ~first ~prev_s p0 i0 tl

  let of_path p =
    let p, i0 = rm_invalid_prefix 0 p in
    let seg = PQ.make() in
    match p with
    | [] | [ _ ] -> make_empty()
    | p0 :: tl -> add_segments_of_path seg ~is_first:true ~first:dummy_seg
                    ~prev_s:dummy_seg p0 i0 tl

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
    Init.close_with_viewport st ~viewport

  (** Replace the segment [s] removed from the sampling [t] by [s']. *)
  let replace_seg_by t ~s ~s' =
    if is_first s then (s'.prev <- s';  t.first <- s')
    else (s'.prev <- s.prev;  s.prev.next <- s');
    if is_last s then (s'.next <- s';  t.last <- s')
    else (s'.next <- s.next;  s.next.prev <- s');
    add_with_witness t.seg s'.cost s'

  (** Replace the segment [s] removed from the sampling [t] by 2
     segments, [s1] followed by [s2]. *)
  let replace_seg_by2 t ~s ~s0 ~s1 =
    s0.next <- s1;  s1.prev <- s0;
    if is_first s then (s0.prev <- s0;  t.first <- s0)
    else (s0.prev <- s.prev;  s.prev.next <- s0);
    if is_last s then (s1.next <- s1;  t.last <- s1)
    else (s1.next <- s.next;  s.next.prev <- s1);
    add_with_witness t.seg s0.cost s0;
    add_with_witness t.seg s1.cost s1

  let complete_intial_sampling ~n ?viewport f init (a:float) (b:float) =
    (* Try to complete the [init] sampling so as to explore the
       interval [a,b] with almost the same density.  ASSUME all values
       of t are finite and in [a,b]. *)
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
          [a, b] is covered.  Use [Init.add_unsafe] because we do not
          want to exclude non-finite (and the intervals they delimit!)
          points at this stage as we have not explored the interval
          enough yet. *)
       let len_t = b -. a in
       let cost_length _ _t0 _ t1 _ t2 _ = (t2 -. t1) /. len_t in
       let n, t0, p0, tl = if t0 = a then (n, t0, p0, tl)
                           else (n - 1, a, f a, init) in
       add_pt p0;
       let st = Init.make ~cost:cost_length ~viewport:Box2.unit ~len_t t0 p0 in
       List.iter (fun (t, p) -> Init.add_unsafe st t p; add_pt p) tl;
       let n = ref(if Init.last_t st <> b then (
                     let fb = f b in
                     Init.add_unsafe st b fb;
                     add_pt fb;
                     n - 1)
                   else n) in
       let sampl = Init.close st in
       (* Insert points to cut the largest intervals.  Here we don't
          care whether the points are valid or not, we just want to
          cover the interval [a,b] well enough.  We want to preserve
          the links between the segments to traverse them after. *)
       while !n > 0 do
         let s = PQ.delete_max sampl.seg in
         let t = s.t0 +. (0.45 *. Random.float 0.1) *. (s.t1 -. s.t0) in
         let ft = f t in
         decr n;
         add_pt ft;
         let len1 = t -. s.t0 in
         let len2 = s.t1 -. t in
         let rec s1 = { t0 = s.t0;  p0 = s.p0;  t1 = t;  p1 = ft;
                        valid = Both; (* use both endpoints *) cost = len1;
                        prev = s.prev;  next = s2;  witness = None }
         and s2 = { t0 = t;  p0 = ft;  t1 = s.t1;  p1 = s.p1;
                    valid = Both; cost = len2;
                    prev = s1;  next = s.next;  witness = None } in
         if is_first s then (s1.prev <- s1;  sampl.first <- s1);
         if is_last s then (s2.next <- s2;   sampl.last <- s2);
         add_with_witness sampl.seg len1 s1;
         add_with_witness sampl.seg len2 s2;
       done;
       (* Guess reasonable viewport from sampling. *)
       let viewport = guess_viewport viewport
                        ~xmin:!xmin ~xmax:!xmax ~ymin:!ymin ~ymax:!ymax in
       (* Now, build the queue based on the curvature costs and check
          the points are valid (Iter.add does that check).  *)
       let st = Init.make ~cost:Cost.estimate ~viewport ~len_t t0 p0 in
       iter sampl ~f:(fun s -> Init.add st s.t0 s.p0;
                               Init.add st s.t1 s.p1);
       Init.close st

  (* Assume that [Both] points of segment [s] are finite.  Look at the
     previous segment of [s] and update its cost. *)
  let update_cost_prev t s =
    if not(is_first s) then
      let s_prev = s.prev in
      if s_prev.valid = Both && s_prev.t1 = s.t0  then (
        match s_prev.witness with
        | Some w ->
           let cost = Cost.estimate t.viewport
                        s_prev.t0 s_prev.p0 s.t0 s.p0 s.t1 s.p1 in
           s_prev.cost <- cost;
           PQ.increase_priority cost w
        | None -> assert false (* Well init samplings must have witnesses *)
      )

  (* Same as [update_cost_prev] but for the next node. *)
  let update_cost_next t s =
    if not(is_last s) then
      let s_next = s.next in
      if s_next.valid = Both && s_next.t0 = s.t1 then (
        match s_next.witness with
        | Some w ->
           let cost = Cost.estimate t.viewport
                        s.t0 s.p0 s.t1 s.p1 s_next.t1 s_next.p1 in
           s_next.cost <- cost;
           PQ.increase_priority cost w
        | None -> assert false
      )

  let param_gen name ?(n=100) ?viewport ?(init=[]) ?(init_pt=[])
        (f: float -> Gg.p2) (a: float) (b: float) =
    if not(is_finite_float a && is_finite_float b) then
      invalid_arg(name ^ ": a and b must be finite");
    if a = b then invalid_arg(name ^ ": empty interval [a,b]");
    let a, b = if a < b then a, b else b, a in
    (* Make sure all t are finite and in the interval [a,b]. *)
    let init_pt = List.filter (fun (t,_) -> a <= t && t <= b) init_pt in
    let init = List.fold_left (fun l t ->
                   if a <= t && t <= b then (t, f t) :: l else l) init_pt init in
    let n0 = truncate(0.1 *. float n) in
    let n0 = if n0 <= 10 then 10 else n0 in
    let init = match init with
      | [] -> almost_uniform ~n:n0 ?viewport f a b
      | _ -> complete_intial_sampling ~n:n0 ?viewport f init a b in
    assert(not(PQ.is_empty init.seg));
    (* to_file init "/tmp/init.dat"; *)
    let n = ref (n - n0) in
    while !n > 0 do
      let c0 = PQ.max_priority init.seg in
      let s = PQ.delete_max init.seg in
      let t = s.t0 +. (0.45 +. Random.float 0.1) *. (s.t1 -. s.t0) in
      let p = f t in
      decr n;
      let valid = is_finite p in
      match s.valid with
      | Both ->
         if valid then
           let cost = Cost.estimate init.viewport s.t0 s.p0 t p s.t1 s.p1 in
           let s0 = segment ~t0:s.t0 ~p0:s.p0 ~t1:t ~p1:p ~valid:Both ~cost in
           let s1 = segment ~t0:t ~p0:p ~t1:s.t1 ~p1:s.p1 ~valid:Both ~cost in
           replace_seg_by2 init ~s ~s0 ~s1;
           (* Update the cost of neighbor intervals *)
           update_cost_prev init s0;
           update_cost_next init s1
         else
           let cost = 0.5 *. c0 in
           let s0 = segment ~t0:s.t0 ~p0:s.p0 ~t1:t ~p1:p ~valid:P0 ~cost in
           let s1 = segment ~t0:t ~p0:p ~t1:s.t1 ~p1:s.p1 ~valid:P1 ~cost in
           replace_seg_by2 init ~s ~s0 ~s1
      | P0 ->
         if valid then
           let cost = V2.(norm (s.p0 - p)) in
           let s0 = segment ~t0:s.t0 ~p0:s.p0 ~t1:t ~p1:p ~valid:Both ~cost in
           let s1 = segment ~t0:t ~p0:p ~t1:s.t1 ~p1:s.p1 ~valid:P0 ~cost in
           replace_seg_by2 init ~s ~s0 ~s1;
           update_cost_prev init s0
         else
           let cost = 0.5 *. c0 in
           let s0 = segment ~t0:s.t0 ~p0:s.p0 ~t1:t ~p1:p ~valid:P0 ~cost in
           (* Both endpoints of [p, p1] are invalid; drop. *)
           replace_seg_by init ~s ~s':s0
      | P1 ->
         if valid then
           let cost = V2.(norm (s.p1 - p)) in
           let s0 = segment ~t0:s.t0 ~p0:s.p0 ~t1:t ~p1:p ~valid:P1 ~cost in
           let s1 = segment ~t0:t ~p0:p ~t1:s.t1 ~p1:s.p1 ~valid:Both ~cost in
           replace_seg_by2 init ~s ~s0 ~s1;
           update_cost_next init s1
         else
           let cost = 0.5 *. c0 in
           let s1 = segment ~t0:t ~p0:p ~t1:s.t1 ~p1:s.p1 ~valid:P1 ~cost in
           replace_seg_by init ~s ~s':s1
    done;
    init

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
