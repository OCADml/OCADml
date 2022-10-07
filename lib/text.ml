type h_align =
  | Left
  | Center
  | Right

type direction =
  | LeftToRight
  | RightToLeft
  | TopToBottom
  | BottomToTop

type v_align =
  | Top
  | Center
  | Baseline

type t =
  { text : string
  ; size : float option
  ; font : string option
  ; halign : h_align option
  ; valign : v_align option
  ; spacing : float option
  ; direction : direction option
  ; language : string option
  ; script : string option
  ; fn : int option
  }

let h_align_to_string = function
  | Left   -> "left"
  | Center -> "center"
  | Right  -> "right"

let v_align_to_string = function
  | Top      -> "top"
  | Center   -> "center"
  | Baseline -> "baseline"

let direction_to_string = function
  | LeftToRight -> "ltr"
  | RightToLeft -> "rtl"
  | TopToBottom -> "ttb"
  | BottomToTop -> "btt"
