(** Provides functions for the creation of and operations between
    {{:https://en.wikipedia.org/wiki/Quaternion} quaternions}. These can be used
    to create composable and interpolatable rotations to be applied to 3d vectors
    ({!V3.t}), and shapes. *)

type t

(** The identity quaternion: [{ x = 0.; y = 0.; z = 0.; w = 1. }] *)
val id : t

(** [make ax angle]

    Create a quaternion representing a rotation of [angle] (in radians) around
    the vector [ax]. *)
val make : V3.t -> float -> t

(** {1 Basic Arithmetic} *)

(** [add a b]

    Hadamard (element-wise) addition of quaternions [a] and [b]. *)
val add : t -> t -> t

(** [sub a b]

    Hadamard (element-wise) subtraction of quaternion [b] from [a]. *)
val sub : t -> t -> t

(** [mul a b]

    Quaternion multiplication of [a] and [b]. *)
val mul : t -> t -> t

(** [neg t]

    Negation of all elements of [t]. *)
val neg : t -> t

(** [sadd t s]

    Add [s] to the magnitude of [t], leaving the imaginary parts unchanged. *)
val sadd : t -> float -> t

(** [ssub t s]

    Subtract [s] from the magnitude of [t], leaving the imaginary parts
    unchanged. *)
val ssub : t -> float -> t

(** [ssub_neg t s]

    Negate the imaginary parts of [t], and subtract the magnitude from [s] to
    obtain the new magnitude. *)
val ssub_neg : t -> float -> t

(** [smul t s]

    Element-wise multiplication of [t] by [s]. *)
val smul : t -> float -> t

(** [div_scalar t s]

    Element-wise division of [t] by [s]. *)
val sdiv : t -> float -> t

(** {1 Vector Math} *)

(** [norm t]

    Calculate the vector norm (a.k.a. magnitude) of [t]. *)
val norm : t -> float

(** [normalize t]

    Normalize [t] to a quaternion for which the magnitude is equal to 1.
    e.g. [norm (normalize t) = 1.] *)
val normalize : t -> t

(** [dot a b]

    Vector dot product of [a] and [b]. *)
val dot : t -> t -> float

(** [conj t]

    Take the conjugate of the quaternion [t], negating the imaginary parts (x,
    y, and z) of [t], leaving the magnitude unchanged. *)
val conj : t -> t

(** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)
val distance : t -> t -> float

(** Conversions *)

(** [of_euler v]

    Create a quaternion equivalent to the Euler angle rotations represented by [v]. *)
val of_euler : V3.t -> t

(** [to_euler t]

    Convert the quaternion [t] to equivalent Euler angles. *)
val to_euler : t -> V3.t

(** [to_affine ?trans t]

    Convert quaternion [t] into an {!Affine3.t}, optionally providing a
    translation vector [trans] to tack on. *)
val to_affine : ?trans:V3.t -> t -> Affine3.t

(** {1 Vector Transformations} *)

(** [align a b]

    Calculate a quaternion that would bring [a] into alignment with [b]. *)
val align : V3.t -> V3.t -> t

(** [transform ?about t v]

    Rotate [v] with the quaternion [t] around the origin (or the point [about]
    if provided). *)
val transform : ?about:V3.t -> t -> V3.t -> V3.t

(** {1 Utilities} *)

(** [slerp a b step]

    Spherical linear interpotation. Adapted from
    {{:https://github.com/KieranWynn/pyquaternion} pyquaternion}. *)
val slerp : t -> t -> float -> t

val to_string : t -> string
