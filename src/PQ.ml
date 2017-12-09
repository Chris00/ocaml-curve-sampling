(* File: curve_sampling_pq.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* Maximum priority queue.
   Implemented as a Pairing heap (http://en.wikipedia.org/wiki/Pairing_heap).
 *)


type 'a tree = Heap of float * 'a * 'a tree list
(* Heap(priority, el, sub_heaps with lower or equal priorities) *)

type 'a t = 'a tree option (* None: empty heap *)

let empty = None

let is_empty q = (q = None) [@@inline]

let max = function
  | None -> failwith "Curve_Sampling.PQ.max: empty"
  | Some(Heap(_, x, _)) -> x

let max_priority = function
  | None -> neg_infinity
  | Some(Heap(p, _, _)) -> p

let[@inline] merge_tree (Heap(p1, e1, h1) as t1) (Heap(p2, e2, h2) as t2) =
  if p1 > p2 then Heap(p1, e1, t2 :: h1)
  else Heap(p2, e2, t1 :: h2)

let add q p x =
  if Gg.Float.is_nan p then
    invalid_arg "Curve_Sampling.PQ: NaN priority not allowed";
  Some(match q with
       | None -> Heap(p, x, [])
       | Some t -> merge_tree (Heap(p, x, [])) t)

(* [merge_pairs_trees l] merges a NON-EMPTY list of trees. *)
let rec merge_pairs_trees = function
  | [h] -> h
  | [h1; h2] -> merge_tree h1 h2
  | h1 :: h2 :: tl -> merge_tree (merge_tree h1 h2) (merge_pairs_trees tl)
  | [] -> assert false

let[@inline] merge_pairs = function
  | [] -> None
  | l -> Some(merge_pairs_trees l)

let delete_max = function
  | None -> failwith "Curve_Sampling.PQ.delete_max: empty"
  | Some(Heap(_, x, hs)) -> (merge_pairs hs, x)

let rec iter_trees f (Heap(_, x, hs)) =
  f x;
  List.iter (iter_trees f) hs

let iter q f = match q with
  | None -> ()
  | Some t -> iter_trees f t


let rec fold_trees f init (Heap(_, x, hs)) =
  let init = f init x in
  List.fold_left (fold_trees f) init hs

let fold q ~init f = match q with
  | None -> init
  | Some t -> fold_trees f init t

let rec map_trees f (Heap(priority, x, hs)) =
  Heap(priority, f x, List.map (map_trees f) hs)

let map q f = match q with
  | None -> None
  | Some t -> Some(map_trees f t)

let rec filter_map_trees acc f = function
  | Heap(priority, x, hs) :: tl ->
     let y = f x in
     (match y with
      | Some y -> let hs = filter_map_trees [] f hs in
                  filter_map_trees (Heap(priority, y, hs) :: acc) f tl
      | None -> (* move sub-heaps [hs] one level up *)
         let acc = filter_map_trees acc f hs in
         filter_map_trees acc f tl)
  | [] -> acc

let rec filter_map_tree f (Heap(priority, x, hs)) =
  match f x with
  | Some y -> Some(Heap(priority, y, filter_map_trees [] f hs))
  | None -> merge_pairs(filter_map_trees [] f hs)

let filter_map q f = match q with
  | None -> None
  | Some t -> filter_map_tree f t
