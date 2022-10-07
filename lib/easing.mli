(** Cubic bezier easing.

    Adapted from {{:https://github.com/jchavarri/rebez} rebez}, which itself is
    an adaptation of {{:https://github.com/gre/bezier-easing} bezier-easing}. *)

(** [make p1 p2]

    Compute an easing function with from a cubic bezier curve with handle points
    [p1] and [p2]. The resulting function takes an [x] value in the range of [0.]
    to [1.] (any values outside of this range are clamped) and returns the
    corresponding [y] value from the curve
    [Bezier2.make [{ x = 0.; y = 0.}; p1; p2; { x = 1.; y =1.}]].
    Raises [Invalid_argument] if the x values of [p1] and [p2] do not fall in
    the [0.]  to [1.] interval. *)
val make : V2.t -> V2.t -> float -> float
