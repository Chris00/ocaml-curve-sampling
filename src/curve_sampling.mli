
type t
(** Represent a 2D sampling.  This can be thought as a sequence of
    paths. *)

(** {2 Adaptive sampling of parametric curves} *)


(** {2 Uniform sampling} *)

val uniform : ?n:int -> (float -> float) -> float -> float -> t
(** [uniform f a b] returns a sampling of the graph of [f] on [n]
    equidistant points in the interval \[[a], [b]\] (the boundaries
    [a] and [b] being always included — so [n >= 2]).  The resulting
    sampling may have less than [n] points because evaluations
    returning points with NaN components are discarded (they split the
    path).

    @param n the number of points.  If [n <= 2] is given, it is
    considered as if [n=2] was passed.  Default: [n = 100]. *)


(** {2 Working with samplings} *)

val tr : Gg.m3 -> t -> t
(** [tr m t] apply the transform [m] on [t].  See {!Gg.P2.tr} for more
   details. *)

val clip : t -> Gg.box2 -> t
(** [clip t b] returns the sampling [t] but cropped to the 2D box.  A
   path that crosses the boundary will get additional nodes at the
   points of crossing and the part outside the bounding box will be
   dropped.  (Thus a path entirely out of the bounding box will be
   removed.) *)

val of_path : (float * float) list -> t

val concat : ?join:bool -> t -> t -> t
(** [concat t1 t2] make a sampling representing the curves in [t1]
   followed by those in [t2].
   @param join whether to join the two samplings by a line.
          Default: [false]. *)


(** {2 GG interface} *)

(** Interface using [Gg.p2] to represent points. *)
module P2 : sig
  val uniform : ?n:int -> (float -> Gg.p2) -> float -> float -> t
  (** [uniform f a b] return a sampling of the image of [f] on [n]
      equidistant points in the interval \[[a], [b]\] (the boundaries
      [a] and [b] being always included — so [n >= 2]).

      @param n the number of points.  If [n <= 2] is given, it is
      considered as if [n=2] was passed.  Default: [n = 100]. *)

  val of_path : Gg.p2 list -> t
  (** Use the provided path as the sampling. *)

  val to_list : t -> Gg.p2 list

end

(** {2 Accessors to the sampling data} *)

val to_list : t -> (float * float) list

val to_array : t -> float array * float array

val to_channel : out_channel -> t -> unit
(** [to_channel ch t] writes the sampling [t] to the channel [ch].
   Each point is written as "x y" on a single line (in scientific
   notation).  If the path is interrupted, a blank line is printed.
   This format is compatible with gnuplot. *)

val to_file : string -> t -> unit
(** [to_file fname t] saves the sampling [t] to the file [fname] using
   the format described in {!to_channel}. *)

;;
