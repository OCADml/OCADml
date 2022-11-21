(** Four-dimensional vector
    @canonical OCADml.v4 *)
type v4 =
  { x : float
  ; y : float
  ; z : float
  ; w : float
  }

(** Three-dimensional vector
    @canonical OCADml.v3 *)
type v3 =
  { x : float
  ; y : float
  ; z : float
  }

(** Two-dimensional vector
    @canonical OCADml.v2 *)
type v2 =
  { x : float
  ; y : float
  }

let[@inline] v2 x y = { x; y }
let[@inline] v3 x y z = { x; y; z }
let[@inline] v4 x y z w = { x; y; z; w }

module type S = sig
  type t

  (** Zero vector *)
  val zero : t

  (** A line segment between two points. *)
  type line =
    { a : t
    ; b : t
    }

  (** Bounding box. *)
  type bbox =
    { min : t
    ; max : t
    }

  (** {1 Comparison} *)

  (** [equal a b]

      Float equality between the vectors [a] and [b]. *)
  val equal : t -> t -> bool

  (** [compare a b]

      Compare the vectors [a] and [b]. *)
  val compare : t -> t -> int

  (** [approx ?eps a b]

    Returns true if the distance between vectors [a] and [b] is less than or
    equal to the epsilon [eps]. *)
  val approx : ?eps:float -> t -> t -> bool

  (** {1 Basic Arithmetic} *)

  (** [horizontal_op f a b]

    Hadamard (element-wise) operation between vectors [a] and [b] using the
    function [f]. *)
  val horizontal_op : (float -> float -> float) -> t -> t -> t

  (** [add a b]

    Hadamard (element-wise) addition of vectors [a] and [b]. *)
  val add : t -> t -> t

  (** [sub a b]

    Hadamard (element-wise) subtraction of vector [b] from [a]. *)
  val sub : t -> t -> t

  (** [mul a b]

    Hadamard (element-wise) product of vectors [a] and [b]. *)
  val mul : t -> t -> t

  (** [div a b]

    Hadamard (element-wise) division of vector [a] by [b]. *)
  val div : t -> t -> t

  (** [neg t]

    Negation of all elements of [t]. *)
  val neg : t -> t

  (** [add_scalar t s]

    Element-wise addition of [s] to [t]. *)
  val sadd : t -> float -> t

  (** [sub_scalar t s]

    Element-wise subtraction of [s] from [t]. *)
  val ssub : t -> float -> t

  (** [mul_scalar t s]

    Element-wise multiplication of [t] by [s]. *)
  val smul : t -> float -> t

  (** [div_scalar t s]

    Element-wise division of [t] by [s]. *)
  val sdiv : t -> float -> t

  (** {1 Vector Math} *)

  (** [norm t]

    Calculate the vector norm (a.k.a. magnitude) of [t]. *)
  val norm : t -> float

  (** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)
  val distance : t -> t -> float

  (** [normalize t]

    Normalize [t] to a vector for which the magnitude is equal to 1.
    e.g. [norm (normalize t) = 1.] *)
  val normalize : t -> t

  (** [dot a b]

    Vector dot product of [a] and [b]. *)
  val dot : t -> t -> float

  (** [cross a b]

    Vector cross product of [a] and [b]. In the case of 2d vectors, the cross
    product is performed with an assumed z = 0. *)
  val cross : t -> t -> v3

  (** [mid a b]

      Compute the midpoint between the vectors [a] and [b]. *)
  val mid : t -> t -> t

  (** [mean l]

    Calculate the mean / average of all vectors in [l]. *)
  val mean : t list -> t

  (** [mean' a]

    Calculate the mean / average of all vectors in the array [a]. *)
  val mean' : t array -> t

  (** [angle a b]

    Calculate the angle between the vectors [a] and [b]. *)
  val angle : t -> t -> float

  (** [angle_points a b c]

    Calculate the angle between the points [a], [b], and [c]. *)
  val angle_points : t -> t -> t -> float

  (** [ccw_theta t]

       Calculate the angle in radians counter-clockwise [t] is from the positive
       x-axis along the xy plane. *)
  val ccw_theta : t -> float

  (** [vector_axis a b]

       Compute the vector perpendicular to the vectors [a] and [b]. *)
  val vector_axis : t -> t -> v3

  (** [clockwise_sign ?eps a b c]

    Returns the rotational ordering (around the z-axis, from the perspective of
    the origin, looking "up" the z-axis) of the points [a], [b], and [c] as a
    signed float, [1.] for clockwise, and [-1.] for counter-clockwise. If the
    points are collinear (not forming a valid triangle, within the tolerance of
    [eps]), [0.] is returned. *)
  val clockwise_sign : ?eps:float -> t -> t -> t -> float

  (** [collinear p1 p2 p3]

    Returns [true] if [p2] lies on the line between [p1] and [p3]. *)
  val collinear : t -> t -> t -> bool

  (** [lerp a b u]

    Linearly interpolate between vectors [a] and [b]. *)
  val lerp : t -> t -> float -> t

  (** [lerpn a b n]

    Linearly interpolate [n] vectors between vectors [a] and [b]. If [endpoint]
    is [true], the last vector will be equal to [b], otherwise, it will be about
    [a + (b - a) * (1 - 1 / n)]. *)
  val lerpn : ?endpoint:bool -> t -> t -> int -> t list

  (** [distance_to_vector p v]

    Distance from point [p] to the line passing through the origin with unit
    direction [v]. *)
  val distance_to_vector : t -> t -> float

  (** [distance_to_line ?bounds ~line t]

      Distance between the vector [t], and any point on [line]. [bounds]
    indicates whether each end [{a; b}] of [line] is bounded, or a ray (default
    = [(false, false)], indicating an infinite line in both directions.). *)
  val distance_to_line : ?bounds:bool * bool -> line:line -> t -> float

  (** [point_on_line ?eps ?bounds ~line t]

      Return [true] if the point [t] falls within [eps] distance of the [line].
   [bounds] indicates whether each end [{a; b}] of [line] is bounded, or a ray
   (default = [(false, false)], indicating an infinite line in both
   directions.) *)
  val point_on_line : ?eps:float -> ?bounds:bool * bool -> line:line -> t -> bool

  (** [line_closest_point ?bounds ~line t]

      Find the closest point to [t] lying on the provided [line]. [bounds]
   indicates whether each end [{a; b}] of [line] is bounded, or a ray (default =
   [(false, false)], indicating an infinite line in both directions.) *)
  val line_closest_point : ?bounds:bool * bool -> line:line -> t -> t

  (** [lower_bounds a b]

       Compute the lower bounds (minima of each dimension) of the vectors [a] and [b]. *)
  val lower_bounds : t -> t -> t

  (** [upper_bounds a b]

       Compute the upper bounds (maxima of each dimension) of the vectors [a] and [b]. *)
  val upper_bounds : t -> t -> t

  (** [bbox a b]

       Compute the bounding box that contains the vectors [a] and [b]. *)
  val bbox : t -> t -> bbox

  (** [bbox_intersect a b]

      Compute the intersect of the bounding boxes [a] and [b]. *)
  val bbox_intersect : bbox -> bbox -> bbox option

  (** [bbox_area bb]

      Compute the area of the bounding box [bb]. *)
  val bbox_area : bbox -> float

  (** [bbox_centroid bb]

      Compute the centroid of the bounding box [bb]. *)
  val bbox_centroid : bbox -> t

  (** {1 Utilities} *)

  val map : (float -> float) -> t -> t
  val get_x : t -> float
  val get_y : t -> float
  val get_z : t -> float
  val to_v2 : t -> v2
  val to_string : t -> string

  (** [deg_of_rad t]

    Element-wise conversion of [t] from radians to degrees. *)
  val deg_of_rad : t -> t

  (** [rad_to_deg t]

    Element-wise conversion of [t] from degrees to radians. *)
  val rad_of_deg : t -> t

  (** {1 Infix operations} *)

  (** [a +@ b]

    Hadamard (element-wise) addition of [a] and [b]. *)
  val ( +@ ) : t -> t -> t

  (** [a -@ b]

    Hadamard (element-wise) subtraction of [b] from [a]. *)
  val ( -@ ) : t -> t -> t

  (** [a *@ b]

    Hadamard (element-wise) product of [a] and [b]. *)
  val ( *@ ) : t -> t -> t

  (** [a /@ b]

    Hadamard (element-wise) division of [a] by [b]. *)
  val ( /@ ) : t -> t -> t

  (** [t +$ s]

    Scalar addition of the vector [t] and scalar [s]. *)
  val ( +$ ) : t -> float -> t

  (** [t -$ s]

    Scalar subtraction of the scalar [s] from the vector [t]. *)
  val ( -$ ) : t -> float -> t

  (** [t *$ s]

    Scalar multiplication of the vector [t] by the scalar [s]. *)
  val ( *$ ) : t -> float -> t

  (** [t /$ s]

    Scalar division of the vector [t] by the scalar [s]. *)
  val ( /$ ) : t -> float -> t
end
