(** Generation and measurement of 3d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := V3.t (** @inline *)

(** {1 Basic Transfomations} *)

val translate : V3.t -> t -> t
val xtrans : float -> t -> t
val ytrans : float -> t -> t
val ztrans : float -> t -> t
val rotate : ?about:V3.t -> V3.t -> t -> t
val xrot : ?about:V3.t -> float -> t -> t
val yrot : ?about:V3.t -> float -> t -> t
val zrot : ?about:V3.t -> float -> t -> t
val quaternion : ?about:V3.t -> Quaternion.t -> t -> t
val axis_rotate : ?about:V3.t -> V3.t -> float -> t -> t
val affine : Affine3.t -> t -> t
val scale : V3.t -> t -> t
val mirror : V3.t -> t -> t
