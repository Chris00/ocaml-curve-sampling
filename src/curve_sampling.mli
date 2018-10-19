(** Adaptive sampling of 2D curves. *)

type t
(** Representation of a 2D sampling.  This can be thought as a path,
   with possible "jumps" because of discontinuities or leaving the
   "domain". *)


(** {2 Adaptive sampling of parametric curves} *)

val fn : ?n:int -> ?viewport:Gg.Box2.t ->
         ?init: float list -> ?init_pt: (float * float) list ->
         (float -> float) -> float -> float -> t
(** [fn f a b] returns a sampling of the graph of [f] on the interval
   \[[a], [b]\] by evaluating [f] at [n] points.
   For the optional arguments, see {!param}. *)

val param :
  ?n:int -> ?viewport:Gg.Box2.t ->
  ?init: float list -> ?init_pt: (float * (float * float)) list ->
  (float -> float * float) -> float -> float -> t
(** [param f a b] returns a sampling of the range of [f] on the
   interval \[[a], [b]\] by evaluating [f] at [n] points (or less).

   @param n The maximum number of evaluations of [f].  Default: [100].
            If [n] ≤ 10, then [n = 10] is used instead.
   @param init Initial values of [t] such that [f t] must be included
          into the sampling in addition to the [n] evaluations.  Only
          the values between [a] and [b] are taken into account.
          Default: empty.
   @param init_pt Initial points [(t, f t)] to include into the
          sampling in addition to the [n] evaluations.  This allows
          you to use previous evaluations of [f]. Only the couples
          with first coordinate [t] between [a] and [b] are
          considered. Default: empty. *)


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
(** [clip t b] returns the sampling [t] but clipped to the 2D box.  A
   path that crosses the boundary will get additional nodes at the
   points of crossing and the part outside the bounding box will be
   dropped.  (Thus a path entirely out of the bounding box will be
   removed.) *)

val of_path : (float * float) list -> t
(** Use the provided path as the sampling. *)


(** {2 GG interface} *)

(** Interface using [Gg.p2] to represent points.  This is the
   preferred interface. *)
module P2 : sig
  val param : ?n:int -> ?viewport:Gg.Box2.t ->
              ?init: float list -> ?init_pt: (float * Gg.p2) list ->
              (float -> Gg.p2) -> float -> float -> t
  (** See {!Curve_Sampling.param}. *)

  val uniform : ?n:int -> (float -> Gg.p2) -> float -> float -> t
  (** [uniform f a b] return a sampling of the image of [f] on [n]
      equidistant points in the interval \[[a], [b]\] (the boundaries
      [a] and [b] being always included — so [n >= 2]).

      @param n the number of points.  If [n <= 2] is given, it is
      considered as if [n=2] was passed.  Default: [n = 100]. *)

  val of_path : Gg.p2 list -> t
  (** Use the provided path as the sampling. *)

  type point_or_cut = Point of Gg.p2 | Cut

  val to_list : t -> point_or_cut list
  (** [to_list s] return the sampling as a list of points in
     increasing order of the parameter of the curve.  The curve is
     possibly made of several pieces separated by a single [Cut]. *)
  ;;
end

(** {2 Accessors to the sampling data} *)

val to_list : t -> (float * float) list list
(** [to_list t] return the sampling as a list of connected components
   of the path, each of which is given as a list of (x,y) couples. *)

val to_channel : t -> out_channel -> unit
(** [to_channel t ch] writes the sampling [t] to the channel [ch].
   Each point is written as "x y" on a single line (in scientific
   notation).  If the path is interrupted, a blank line is printed.
   This format is compatible with gnuplot. *)

val to_file : t -> string -> unit
(** [to_file t fname] saves the sampling [t] to the file [fname] using
   the format described in {!to_channel}. *)

val to_latex : t -> string -> unit
(** [to_latex t fname] saves the sampling [t] as PGF/TikZ commands.  *)

;;
