(* File: curve_sampling_pq.mli

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

type 'a t
(** Immutable maximum priority queue, with float priority. *)

val empty : 'a t
(** [empty] is the empty priority queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] tells whether the queue [q] is empty. *)

val add : 'a t -> float -> 'a -> 'a t
(** [add q p x] returns a new queue consisting of [q] to which the
    elements [x] with priority [p] was added.

    @raise Invalid_argument if [p] is NaN. *)

val max : 'a t -> 'a
(** [max q] returns an element of [q] with maximum priority.
    @raise Failwith if the queue is empty. *)

val max_priority : 'a t -> float
(** [max_priority q] returns the maximum priority of elements in [q]
    or [neg_infinity] if [q] is empty.  *)

val delete_max : 'a t -> 'a t * 'a
(** [delete_max q] delete an element with maximum priority of [q],
    return the new queue and the element.

    @raise Failwith if the queue is empty. *)

val fold : 'a t -> init:'b -> ('b -> 'a -> 'b) -> 'b
(** [fold q init f] folds the function [f] on all elements present in
   the queue [q].  The order in which elements are passed is
   unspecified. *)

val iter : 'a t -> ('a -> unit) -> unit
(** [iter q f] iterates the function [f] on all elements present in
    the queue [q] (which is unchanged).  The order in which elements
    are passed is unspecified. *)

val map : 'a t -> ('a -> 'b) -> 'b t
(** [map q f] return a priority queue with the same priority structure
   than [q] but with [f x] instead of each data value [x]. *)

val filter_map : 'a t -> ('a -> 'b option) -> 'b t
(** [filter_map q f] Same as [map] be remove the values for which [f]
   returns [None]. *)

;;
