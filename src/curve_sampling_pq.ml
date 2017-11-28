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


type 'a t =
| Empty
| Heap of float * 'a * 'a t list
(* Heap(priority, el, sub_heaps) *)

let empty = Empty

let is_empty q = (q = Empty) [@@inline]

let max = function
  | Empty -> failwith "Curve_Sampling.PQ.max: empty"
  | Heap(_, x, _) -> x

let max_priority = function
  | Empty -> neg_infinity
  | Heap(p, _, _) -> p

let[@inline] merge q1 q2 =
  match q1, q2 with
  | Empty, _ -> q2
  | _, Empty -> q1
  | Heap(p1, e1, h1), Heap(p2, e2, h2) ->
    if p1 > p2 then Heap(p1, e1, q2 :: h1)
    else Heap(p2, e2, q1 :: h2)

let add q p x =
  if Gg.Float.is_nan p then
    invalid_arg "Curve_Sampling.PQ: NaN priority not allowed";
  merge (Heap(p, x, [])) q

let rec merge_pairs = function
  | [] -> Empty
  | [h] -> h
  | h1 :: h2 :: tl -> merge (merge h1 h2) (merge_pairs tl)

let delete_max = function
  | Empty -> failwith "Curve_Sampling.PQ.delete_max: empty"
  | Heap(_, x, []) -> (Empty, x)
  | Heap(_, x, [h]) -> (h, x)
  | Heap(_, x, hs) -> (merge_pairs hs, x)


let rec iter_ph f = function
  | Empty -> ()
  | Heap(_, x, hs) -> f x;  List.iter (fun h -> iter_ph f h) hs

let iter q f = iter_ph f q

