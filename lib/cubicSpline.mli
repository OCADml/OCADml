(** Cubic {{:https://en.wikipedia.org/wiki/Spline_interpolation} spline
    interpolation} of 2d paths.

    This is a port of a
   {{:https://github.com/Simsso/Online-Tools/blob/master/src/page/logic/cubic-spline-interpolation.js}
   javascript implementation} underlying a
   {{:https://tools.timodenk.com/cubic-spline-interpolation} spline
   visualization tool} by Timo Denk. *)

(** Boundary condition for cubic spline fitting. *)
type boundary =
  [ `Quadratic
  | `NotAKnot
  | `Periodic
  | `Natural
  ]

(** Cubic spline coefficients *)
type coef = private
  { a : float
  ; b : float
  ; c : float
  ; d : float
  }

(** Calculated coefficients along with the X ranges that they apply to.
    Abstracted to protect array access *)
type t

(** {1 Immutable access to fit results} *)

(** [len t]

    Length of the X range and coefficient arrays stored in [t]. *)
val len : t -> int

(** [xmins t]

    Minimum X values to which the coefficients at the same indices within [t] apply. *)
val xmins : t -> float list

(** [xmaxs t]

    Maximum X values to which the coefficients at the same indices within [t] apply. *)
val xmaxs : t -> float list

(** [coefs t]

    Coefficients describing a cubic spline, as calculated by {!fit}. *)
val coefs : t -> coef list

(** {2 Index getters} *)

val get_xmin : t -> int -> float option
val get_xmax : t -> int -> float option
val get_coef : t -> int -> coef option
val get_xmin_exn : t -> int -> float
val get_xmax_exn : t -> int -> float
val get_coef_exn : t -> int -> coef

(** {1 Fitting} *)

(** [fit ?boundary ps]

    Calculate cubic spline coefficients with the [boundary] condition (defaults
    to [`Natural]) for the 2-dimensional control points [ps]. *)
val fit : ?boundary:boundary -> V2.t list -> t

(** {1 Extrapolation} *)

(** [extrapolate t x]

    Calculate the corresponding dependent value (e.g. [Some y]) for the given
    independent value [x] using the cubic spline fit coefficient in [t]. If [x]
    does not fall within the range of the control values used to generate [t],
    [None] is returned. *)
val extrapolate : t -> float -> float option

(** [extrapolate_path t xs]

    Use [t] to extrapolate [xs] into a 2-dimensional cubic spline path. *)
val extrapolate_path : t -> float list -> V2.t list

(** [interpolate_path t n]

    Use [t] to interpolate 2-dimensional cubic spline path with [n] evently
    spaced points. *)
val interpolate_path : fn:int -> t -> V2.t list

(** {1 Utility} *)

(** [coef_to_string c]

    Show contents of [c] as a string. *)
val coef_to_string : coef -> string
