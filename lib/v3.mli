(** 3d vector *)
type t = Gg.v3

(** [v x y z]

    Construct a vector from [x], [y], and [z] coordinates. *)
val v : float -> float -> float -> t

(** [of_tup (x, y, z)]

    Construct a vector from a tuple of xyz coordinates. *)
val of_tup : float * float * float -> t

(** [to_tup t]

    Convert the vector [t] to a tuple of xyz coordinates. *)
val to_tup : t -> float * float * float

include V.S with type t := t (** @inline *)

(** {1 Transformations}

    Spatial transformations. Quaternion operations are
    provided when this module is included in {!module:OCADml}. *)

(** [xrot ?about theta t]

    Rotate [t] by [theta] radians in around the x-axis through the origin (or
    the point [about] if provided). *)
val xrot : ?about:t -> float -> t -> t

(** [yrot ?about theta t]

    Rotate [t] by [theta] radians in around the y-axis through the origin (or
    the point [about] if provided). *)
val yrot : ?about:t -> float -> t -> t

(** [zrot ?about theta t]

    Rotate [t] by [theta] radians in around the z-axis through the origin (or
    the point [about] if provided). *)
val zrot : ?about:t -> float -> t -> t

(** [rotate ?about r t]

    Euler (zyx) rotation of [t] by the [r] (in radians) around the origin (or
    the point [about] if provided). Equivalent to [xrot x t |> yrot y |> zrot z],
    where [{x; y; z} = r]. *)
val rotate : ?about:t -> t -> t -> t

(** [translate p t]

    Translate [t] along the vector [p]. Equivalent to {!val:add}. *)
val translate : t -> t -> t

(** [xtrans x t]

    Translate [t] by the distance [x] along the x-axis. *)
val xtrans : float -> t -> t

(** [ytrans y t]

    Translate [t] by the distance [y] along the y-axis. *)
val ytrans : float -> t -> t

(** [ztrans z t]

    Translate [t] by the distance [z] along the z-axis. *)
val ztrans : float -> t -> t

(** [scale s t]

    Scale [t] by factors [s]. Equivalent to {!val:mul}. *)
val scale : t -> t -> t

(** [xscale x t]

    Scale [t] by the factor [x] in the x-dimension. *)
val xscale : float -> t -> t

(** [yscale y t]

    Scale [t] by the factor [y] in the y-dimension. *)
val yscale : float -> t -> t

(** [zscale z t]

    Scale [t] by the factor [z] in the z-dimension. *)
val zscale : float -> t -> t

(** [mirror ax t]

    Mirrors [t] on a plane through the origin, defined by the normal vector
    [ax]. *)
val mirror : t -> t -> t

(** [projection t]

    Project [t] onto the XY plane. *)
val projection : t -> t

(** {1 2d - 3d conversion} *)

(** [of_v2 ?z v]

    Create a 3d vector from the 2d vector [v] by adding a [z] coordinate
    (default = [0.]) *)
val of_v2 : ?z:float -> V.v2 -> t
