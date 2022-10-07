(** Normalized cartesian plane operations facilitating conversion between 2d
    and 3d space. *)

(** Normalized cartesian equation of a plane.

    The type {!t} contains the coefficients {b A}, {b B}, {b C}, and {b D},
    which describe the cartesian equation of a plane where [Ax + By + Cz = D]. *)
type t = private
  { a : float
  ; b : float
  ; c : float
  ; d : float
  }

(** [to_tup t]

 Return the plane [t] as a tuple of its coefficients. *)
val to_tup : t -> float * float * float * float

(** [make p1 p2 p3]

 Create a {!type:t}, which represents the normalized cartesian equation of a
 plane, with three points. Returns [(a, b, c, d)] where [ax + by + cz = d] is
 the equation of a plane. Throws [Invalid_argument] if the points are colinear. *)
val make : V3.t -> V3.t -> V3.t -> t

(** [of_normal ?point normal]

 Create a normalized cartesian plane {!type:t} from a [normal] vector, and a
 [point] (defaulting to [(0., 0.)]) located on the plane that the [normal] is
 projecting off of. *)
val of_normal : ?point:V3.t -> V3.t -> t

(** [to_affine ~op t]

    Compute an affine transformation matrix that moves 3d objects into
    ([~op:`Project]) or out of ([~op:`Lift]) the coordinate system of the plane
    [t] from/to the base coordinate system (XY plane). *)
val to_affine : op:[ `Project | `Lift ] -> t -> Affine3.t

(** [project t p]

 Project the 3d point [p] onto the plane [t]. On partial application of [t],
 a {!Affine3.t} is computed to perform the projection transform. *)
val project : t -> V3.t -> V2.t

(** [lift t p]

 Lift the 2d point [p] onto the plane [t]. On partial application of [t],
 a {!Affine3.t} is computed to perform the lift transform. *)
val lift : t -> V2.t -> V3.t

(** [normal t]

 Return the normalized (unit length) normal vector of the plane [t]. *)
val normal : t -> V3.t

(** [offset t]

 Obtain the coefficient {b d} of the normalized plane [t], or the scalar offset
 of the plane from the origin. The absolute value of this coefficient is the
 distance of the plane from the origin. *)
val offset : t -> float

(** [normalize t]

  Normalize the {b a}, {b b}, and {b c} coefficients of the plane [t], such
  that their vector norm is equal to one. *)
val normalize : t -> t

(** [neg_normal t]

    Negate the plane [t]. *)
val neg : t -> t

(** [distance_to_point t p]

 Calculate the distance to the point [p] from the plane [t]. A negative
 distance indicates that [p] resides below [t]. *)
val distance_to_point : t -> V3.t -> float

(** [greatest_distance t ps]

 Calculate the greatest absolute distance between the plane [t], and the 3d
 points [ps]. *)
val greatest_distance : t -> V3.t list -> float

(** [are_points_on ?eps t ps]

    Returns [true] if all points [ps] are within [eps] distance of the plane [t]. *)
val are_points_on : ?eps:float -> t -> V3.t list -> bool

(** [is_point_above ?eps t p]

    Returns [true] is point [p] is [eps] (default [1e-9]) distance above the
    plane [t]. *)
val is_point_above : ?eps:float -> t -> V3.t -> bool

(** [line_angle t line]

 Calculate the angle between the plane [t] and a 3d [line]. The resulting angle
 is positive if the line vector lies above the plane (on the same side as the
 normal vector of [t]). *)
val line_angle : t -> V3.line -> float

(** [line_intersection t line]

    Find the intersection between a 3d [line] and the plane [t] (within [eps]
    tolerance, default [1e-9]), if it exists. [bounds] indicates whether [line]
    is capped at either (ray) or both (segment) of its ends (default is unbounded
    ([bounds = (false, false)]).  *)
val line_intersection
  :  ?eps:float
  -> ?bounds:bool * bool
  -> t
  -> V3.line
  -> [ `OutOfBounds | `OnPlane of V3.line | `Parallel | `Point of V3.t * float ]

val to_string : t -> string

(** {1 Basic Workplanes }*)

val xy : t
val xz : t
val yz : t
