(** {1 Vectors}

    Vectors for describing transformations and points in
    2/3-dimensional space. *)

(** 4-dimensional vector *)
type v4 = V.v4 =
  { x : float
  ; y : float
  ; z : float
  ; w : float
  }

(** 3-dimensional vector *)
type v3 = V.v3 =
  { x : float
  ; y : float
  ; z : float
  }

(** 2-dimensional vector *)
type v2 = V.v2 =
  { x : float
  ; y : float
  }

(** [v2 x y]

    Construct a 2d vector from [x] and [y] coordinates. *)
let[@inline] v2 x y = { x; y }

(** [v3 x y z]

    Construct a 3d vector from [x], [y], and [z] coordinates. *)
let[@inline] v3 x y z = { x; y; z }

(** [v4 x y z]

    Construct a 4d vector from [x], [y], [z], and [w] coordinates. *)
let[@inline] v4 x y z w = { x; y; z; w }

(** 2-dimensional vector type, including basic mathematical/geometrical
    operations and transformations, allowing for points in 2d space, and higher
    level types composed of them ({i e.g.} {!Path2.t} and {!Poly2.t}) to be
    manipulated. *)
module V2 = struct
  include V2 (** @inline *)

  (** [lift p t]

      Lift the 2d vector/point [t] onto the plane [p]. On partial application of
      [p], a {!Affine3.t} is computed to perform the lift transform. Alias to
      {!Plane.lift}. *)
  let[@inline] lift plane t = Plane.lift plane t

  (** {1 Additional 2d transformations} *)

  (** [affine m t]

      Apply 2d affine transformation matrix [m] to the vector [t]. *)
  let[@inline] affine m t = Affine2.transform m t

  (** {1 2d to 3d transformations} *)

  (** [affine3 m t]

      Apply 3d affine transformation matrix [m] to the vector [t], taking it into
      the 3rd dimension. *)
  let[@inline] affine3 m { x; y } = Affine3.transform m (v3 x y 0.)

  (** [quaternion ?about q t]

      Rotate [t] with the quaternion [q] around the origin (or the point [about]
      if provided), taking it into the 3rd dimension. *)
  let[@inline] quaternion ?about q { x; y } = Quaternion.transform ?about q (v3 x y 0.)

  (** [axis_rotate ?about ax a t]

      Rotates the vector [t] around the axis [ax] through the origin (or the
      point [about] if provided) by the angle [a], taking it into the third
      dimension. *)
  let[@inline] axis_rotate ?about ax a { x; y } =
    Quaternion.(transform ?about (make ax a) (v3 x y 0.))
end

(** 3-dimensional vector type, including basic mathematical/geometrical
    operations and transformations, allowing for points in 3d space, and higher
    level types composed of them ({i e.g.} {!Path3.t}, {!Poly3.t}, and {!Mesh.t})
    to be manipulated. *)
module V3 = struct
  include V3 (** @inline *)

  (** [project p t]

      Project the 3d vector/point [t] onto the plane [p]. On partial application of
      [p], a {!Affine3.t} is computed to perform the projection transform. Alias to
      {!Plane.project}. *)
  let[@inline] project plane t = Plane.project plane t

  (** {1 Additional 3d transformations} *)

  (** [affine m t]

      Apply affine transformation matrix [m] to the vector [t]. *)
  let[@inline] affine m t = Affine3.transform m t

  (** [quaternion ?about q t]

      Rotate [t] with the quaternion [q] around the origin (or the point [about]
      if provided). *)
  let[@inline] quaternion ?about q t = Quaternion.transform ?about q t

  (** [axis_rotate ax a t]

      Rotates the vector [t] around the axis [ax] through the origin (or the
      point [about] if provided) by the angle [a]. *)
  let[@inline] axis_rotate ?about ax a = Quaternion.(transform ?about (make ax a))
end

(** {1 Transformations} *)

module Affine2 = Affine2

(** Affine transformation matrices for transforming 3d vectors ({!V3.t}), and 3d shapes. *)
module Affine3 = struct
  include Affine3 (** @inline *)

  (** {1 Conversions} *)

  (** [project t]

      Project [t] down into a 2d affine transformation matrix (z axis components
      dropped). *)
  let project (t : t) =
    Affine2.
      { r0c0 = t.r0c0
      ; r0c1 = t.r0c1
      ; r0c2 = t.r0c3
      ; r1c0 = t.r1c0
      ; r1c1 = t.r1c1
      ; r1c2 = t.r1c3
      ; r2c0 = 0.
      ; r2c1 = 0.
      ; r2c2 = t.r3c3
      }

  (** [of_quaternion q]

      Create an affine transformation matrix equivalent to the quaternion [q]. *)
  let of_quaternion ?trans q = Quaternion.to_affine ?trans q
end

module Quaternion = Quaternion
module Plane = Plane

(** {1 2-dimensional paths and polygons} *)

module Path2 = Path2
module Bezier2 = Bezier2
module CubicSpline = CubicSpline
module Poly2 = Poly2
module PolyText = PolyText

(** {1 3-dimensional paths, coplanar polygons, and meshes} *)

module Path3 = Path3
module Bezier3 = Bezier3
module Poly3 = Poly3
module Mesh = Mesh

(** {1 Utilities} *)

module Math = Math
module Easing = Easing
module BallTree2 = BallTree2
module BallTree3 = BallTree3
