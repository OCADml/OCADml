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

(** A pair of handle points, p1 and p2.

    Default handle pairs corresponding to the similarly named
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function} css
    keywords} are provided for reference and convenience.

    If none of the standard functions are quite what you're after, there is an
    interactive tool at {{:https://cubic-bezier.com} cubic-bezier.com}
    that may be helpful for finding the handle points that describe the easing
    that you desire. *)
type handles = V2.t * V2.t

(** Linear interpolation [= (v2 0. 0.) (v2 1. 1.)] *)
val linear : handles

(** Interpolation starting slowly, followed by a sharp acceleration and
    gradually slowing towards the end. [= (v2 0.25 0.1, v2 0.25 1.)] *)
val ease : handles

(** Interpolation starting slowly, progressively speeding up until the end.
    [= (v2 0.42 0., v2 1. 1.)] *)
val ease_in : handles

(** Interpolation starting slowly, progressively speeding up before slowing
    towards the end. [= (v2 0.42 0., v2 0.58 1.)] *)
val ease_in_out : handles

(** Interpolation starting abruptly, progressively slowing down towards the
    end. [= (v2 0. 0., v2 0.58 1.)] *)
val ease_out : handles

(** [of_handles (p1, p2)]

    Compute an easing function with a pair of handle points. (see {!make}). *)
val of_handles : handles -> float -> float
