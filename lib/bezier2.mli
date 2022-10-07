(** Generation and measurement of 2d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := V2.t (** @inline *)

(** {1 2d specific functionality} *)

(** [line_intersection ~line pts]

    Compute the positions (between [0.] and [1.]) along the bezier curve defined
    by the control points [pts] that [line] intersects with. *)
val line_intersection : line:V2.line -> V2.t list -> float list

(** {1 Basic Transfomations} *)

val translate : V2.t -> t -> t
val xtrans : float -> t -> t
val ytrans : float -> t -> t
val rotate : ?about:V2.t -> float -> t -> t
val zrot : ?about:V2.t -> float -> t -> t
val scale : V2.t -> t -> t
val mirror : V2.t -> t -> t
val affine : Affine2.t -> t -> t
val affine3 : Affine3.t -> t -> float -> V3.t
val quaternion : ?about:V3.t -> Quaternion.t -> t -> float -> V3.t
val axis_rotate : ?about:V3.t -> V3.t -> float -> t -> float -> V3.t
