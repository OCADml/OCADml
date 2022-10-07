(** Various operations on floats ({i e.g.} approximate equality, linear
    interpolation), and basic arbitrary size 2d matrix operations. *)

(** {1 Float operations} *)

(** [sign v]

    Return the sign of [v] as a float ([-1.], [0.], or [1.]). *)
val sign : float -> float

(** [clamp ~min ~max v]

    Clamp the float [v] between [min] and [max]. *)
val clamp : min:float -> max:float -> float -> float

(** [lerp a b u]

    Linearly interpolate between floats [a] and [b]. *)
val lerp : float -> float -> float -> float

(** [lerpn ?endpoint a b n]

    Linearly interpolate [n] values between [a] and [b]. If [endpoint]
    is [true], the last value will be equal to [b], otherwise, it will be about
    [a + (b - a) * (1 - 1 / n)]. *)
val lerpn : ?endpoint:bool -> float -> float -> int -> float list

(** [quant ~q v]

    Quantize [v] to a multiple of the quantum size [q]. For example:
    {[
      quant ~q:0.2 1.5 = 1.6
    ]} *)
val quant : q:float -> float -> float

(** [quant_down ~q v]

    Quantize [v] to a multiple of the quantum size [q], always rounding down.
    For example:
    {[
      quant_down ~q:0.2 1.75 = 1.6
    ]} *)
val quant_down : q:float -> float -> float

(** [quant_up ~q v]

    Quantize [v] to a multiple of the quantum size [q], always rounding up.
    For example:
    {[
      quant_up ~q:0.2 1.51 = 1.6
    ]} *)
val quant_up : q:float -> float -> float

(** [approx ?eps a b]

    Return [true] if [a] is within the tolerance [eps] of [b]. *)
val approx : ?eps:float -> float -> float -> bool

(** [posmod a m]

    Compute the positive modulo [m] of [a]. The resulting value will be in the
    range of [0] to [m - 1]. *)
val posmod : int -> int -> int

(** {1 Angles and Trigonometry} *)

(** [deg_of_rad r]

    Convert [r] from radians to degrees. *)
val deg_of_rad : float -> float

(** [rad_of_deg d]

    Convert [d] from degrees to radians. *)
val rad_of_deg : float -> float

(** [law_of_cosines a b c]

    Apply the Law of Cosines for a triangle with side lengths [a], [b], and [c].
    The angle in radians of the corner opposite of the third side [c] is
    returned. *)
val law_of_cosines : float -> float -> float -> float

(** {1 2d matrix operations} *)

(** [mat_dims m]

    Return the dimensions ([(n_rows, n_cols)] of the 2d matrix [m]. An
    [Invalid_argument] exception is raised if the row lengths are inconsistent. *)
val mat_dims : 'a array array -> int * int

(** [matmul a b]

    Matrix multiplication of [a] and [b]. [Invalid_argument] is raised if the
    inner dimensions do not match, or if the rows of either matrix are ragged. *)
val matmul : float array array -> float array array -> float array array

(** [transpose m]

    Transpose rows and columns of the 2d matrix [m]. *)
val transpose : 'a array array -> 'a array array

(** {1 Polynomials} *)

(** [real_roots ?eps ?tol p]

    Compute the real roots of the real polynomial [p]. The polynomial is
    specified as [[|a_n; ...; a_1; a_0|]] where [a_n] is the [x ** n]
    coefficient.

    - [eps] is used to determine if the imaginary parts of the roots are zero
    - [tol] is the tolerance for the complex polynomial root finder

    Adapted from the [real_roots] function found in the
    {{:https://github.com/revarbat/BOSL2/blob/master/math.scad#L1361} BOSL2 math
    module}. *)
val real_roots : ?eps:float -> ?tol:float -> float array -> float array

(** {1 Search} *)

(** [bisection ?max_iter ?tolerance ~lower ~upper f]

    Perform a {{:https://en.wikipedia.org/wiki/Bisection_method} bisection}
    search for the value that minimizes the function [f] between the bounds
    [lower] and [upper]. [x] is returned either when [f x = 0.], or the bound
    range falls below [tolerance]. Raises [Failure] if the iteration count
    reaches [max_iter] (default = [100]). *)
val bisection
  :  ?max_iter:int
  -> ?tolerance:float
  -> lower:float
  -> upper:float
  -> (float -> float)
  -> float
