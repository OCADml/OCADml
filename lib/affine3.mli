(** A 3d affine transformation matrix. *)

type row = float * float * float * float
type t = Gg.m4

include Affine.Ops with type t := t (** @inline *)

(** {1 Construction} *)

(** [v e00 e01 e02 e03 e10 e11 e12 e13 e20 e21 e22 e23 e30 e31 e32 e33]

    Create a 2d affine matrix from elements in row major order. *)
val v
  :  float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> float
  -> t

(** {2 Construction by Rows} *)

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

(** {1 Element Accessors} *)

val e00 : t -> float
val e01 : t -> float
val e02 : t -> float
val e03 : t -> float
val e10 : t -> float
val e11 : t -> float
val e12 : t -> float
val e13 : t -> float
val e20 : t -> float
val e21 : t -> float
val e22 : t -> float
val e23 : t -> float
val e30 : t -> float
val e31 : t -> float
val e32 : t -> float
val e33 : t -> float

(** {1 Transforms} *)

(** [scale v]

    Create an affine transformation matrix from the xyz scaling vector [v]. *)
val scale : V3.t -> t

(** [xscale x]

    Create a 2d affine transformation matrix that applies x-axis scaling. *)
val xscale : float -> t

(** [yscale y]

    Create a 2d affine transformation matrix that applies y-axis scaling. *)
val yscale : float -> t

(** [zscale z]

    Create a 2d affine transformation matrix that applies z-axis scaling. *)
val zscale : float -> t

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

    Create an affine transformation matrix that applies a reflection over the
    plane decsribed by the normal vector [ax]. *)
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
