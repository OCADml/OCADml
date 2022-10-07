(** A 3d affine transformation matrix. *)

type row = float * float * float * float

type t =
  { r0c0 : float
  ; r0c1 : float
  ; r0c2 : float
  ; r0c3 : float
  ; r1c0 : float
  ; r1c1 : float
  ; r1c2 : float
  ; r1c3 : float
  ; r2c0 : float
  ; r2c1 : float
  ; r2c2 : float
  ; r2c3 : float
  ; r3c0 : float
  ; r3c1 : float
  ; r3c2 : float
  ; r3c3 : float
  }

include Affine.Ops with type t := t (** @inline *)

(** {1 Construction by Rows} *)

(** [of_rows rows]

    Create an affine transformation matrix from three rows. The last row is set
    to [0., 0., 0., 1.]. *)
val of_rows : row -> row -> row -> t

(** [of_row_matrix_exn m]

    Convert the float matrix [m] into a [t] if it is the correct shape (4 x 4),
    otherwise raise [Invalid_argument]. *)
val of_row_matrix_exn : float array array -> t

(** [of_row_matrix m]

    Convert the float matrix [m] into a [t] if it is the correct shape (4 x 4). *)
val of_row_matrix : float array array -> (t, string) result

(** {1 Transforms} *)

(** [scale v]

    Create an affine transformation matrix from the xyz scaling vector [v]. *)
val scale : V3.t -> t

(** [translate v]

    Create an affine transformation matrix from the xyz translation vector [v]. *)
val translate : V3.t -> t

(** [xtrans x]

    Create an affine transformation matrix that applies a translation of [x]
    distance along the x-axis. *)
val xtrans : float -> t

(** [ytrans y]

    Create an affine transformation matrix that applies a translation of [y]
    distance along the y-axis. *)
val ytrans : float -> t

(** [ztrans z]

    Create an affine transformation matrix that applies a translation of [z]
    distance along the z-axis. *)
val ztrans : float -> t

(** [mirror ax]

    Create an affine transformation matrix that applies a reflection across the
    axis [ax]. *)
val mirror : V3.t -> t

(** [xrot ?about r]

    Create an affine transformation matrix that applies a x-axis rotation of [r]
    radians around the origin (or the point [about] if provided). *)
val xrot : ?about:V3.t -> float -> t

(** [yrot ?about r]

    Create an affine transformation matrix that applies a y-axis rotation of [r]
    radians around the origin (or the point [about] if provided). *)
val yrot : ?about:V3.t -> float -> t

(** [zrot ?about r]

    Create an affine transformation matrix that applies a z-axis rotation of [r]
    radians around the origin (or the point [about] if provided). *)
val zrot : ?about:V3.t -> float -> t

(** [rotate ?about r]

    Create an affine transformation matrix that applies the euler (zyx) rotation
    represented by [r] radians around the origin (or the point [about] if provided). *)
val rotate : ?about:V3.t -> V3.t -> t

(** [axis_rotate ?about ax r]

    Create an affine transfomation matrix that applies a rotation of [r] radians
    around the axis [ax] through the origin (or the point [about] if provided). *)
val axis_rotate : ?about:V3.t -> V3.t -> float -> t

(** [align a b]

    Compute an affine transformation matrix that would bring the vector [a] into
    alignment with [b]. *)
val align : V3.t -> V3.t -> t

(** [skew ?xy ?xz ?yx ?yz ?zx ?zy ()]

    Create an affine transformation matrix that applies a skew transformation
    with the given skew factor multipliers. Factors default to [0.] if not
    provided.

    - [xy]: skew along the x-axis as you get farther from the y-axis
    - [xz]: skew along the x-axis as you get farther from the z-axis
    - [yx]: skew along the y-axis as you get farther from the x-axis
    - [yz]: skew along the y-axis as you get farther from the z-axis
    - [zx]: skew along the z-axis as you get farther from the x-axis
    - [zy]: skew along the z-axis as you get farther from the y-axis *)
val skew
  :  ?xy:float
  -> ?xz:float
  -> ?yx:float
  -> ?yz:float
  -> ?zx:float
  -> ?zy:float
  -> unit
  -> t

(** [skew_xy xa ya]

    Create an affine transformation matrix that applies a skew transformation
    along the xy plane.

    - [xa]: skew angle (in radians) in the direction of the x-axis
    - [ya]: skew angle (in radians) in the direction of the y-axis *)
val skew_xy : float -> float -> t

(** [skew_xz xa za]

    Create and affine transformation matrix that applies a skew transformation
    along the xz plane.

    - [xa]: skew angle (in radians) in the direction of the x-axis
    - [za]: skew angle (in radians) in the direction of the z-axis *)
val skew_xz : float -> float -> t

(** [skew_yz ya za]

    Create and affine transformation matrix that applies a skew transformation
    along the yz plane.

    - [ya]: skew angle (in radians) in the direction of the y-axis
    - [za]: skew angle (in radians) in the direction of the z-axis *)
val skew_yz : float -> float -> t

(** [transform t v]

    Apply the affine transformation matrix [t] to the vector [v]. *)
val transform : t -> V3.t -> V3.t

(** {1 Output} *)

(** [to_string t]

    Convert the matrix [t] to a simple string representation compatible with
    OpenSCAD
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}
    multmatrix}. *)
val to_string : t -> string
