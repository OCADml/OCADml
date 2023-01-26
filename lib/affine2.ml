open V

type row = float * float * float
type t = Gg.m3

let id = Gg.M3.id

let[@inline] v e00 e01 e02 e10 e11 e12 e20 e21 e22 =
  Gg.M3.v e00 e01 e02 e10 e11 e12 e20 e21 e22

let[@inline] e00 t = Gg.M3.e00 t
let[@inline] e01 t = Gg.M3.e01 t
let[@inline] e02 t = Gg.M3.e02 t
let[@inline] e10 t = Gg.M3.e10 t
let[@inline] e11 t = Gg.M3.e11 t
let[@inline] e12 t = Gg.M3.e12 t
let[@inline] e20 t = Gg.M3.e20 t
let[@inline] e21 t = Gg.M3.e21 t
let[@inline] e22 t = Gg.M3.e22 t
let[@inline] trace t = Gg.M3.trace t
let get t r c = Gg.M3.el r c t

let of_row_matrix_exn rm =
  if Array.(length rm = 3 && for_all (fun c -> length c = 3) rm)
  then
    v
      rm.(0).(0)
      rm.(0).(1)
      rm.(0).(2)
      rm.(1).(0)
      rm.(1).(1)
      rm.(1).(2)
      rm.(2).(0)
      rm.(2).(1)
      rm.(2).(2)
  else invalid_arg "Input is not square matrix of size 3."

let of_row_matrix rm =
  try Ok (of_row_matrix_exn rm) with
  | Invalid_argument msg -> Error msg

let[@inline] mul a b = Gg.M3.mul a b
let[@inline] add a b = Gg.M3.add a b
let[@inline] sub a b = Gg.M3.sub a b
let[@inline] ediv a b = Gg.M3.ediv a b
let[@inline] emul a b = Gg.M3.emul a b
let[@inline] transpose t = Gg.M3.transpose t
let[@inline] map f t = Gg.M3.map f t
let[@inline] smul t s = map (( *. ) s) t
let[@inline] sdiv t s = map (( *. ) (1. /. s)) t
let[@inline] sadd t s = map (( +. ) s) t
let[@inline] ssub t s = map (( +. ) (-1. *. s)) t
let compose a b = mul b a
let[@inline] ( %> ) a b = compose a b
let[@inline] ( % ) a b = mul a b

let of_rows (scale_x, skew_x, tx) (skew_y, scale_y, ty) =
  v scale_x skew_x tx skew_y scale_y ty 0. 0. 1.

let[@inline] translate p = Gg.M3.move2 p
let[@inline] xtrans x = translate (v2 x 0.)
let[@inline] ytrans y = translate (v2 0. y)
let[@inline] rotate ?about r = Gg.M3.rot2 ?pt:about r
let[@inline] zrot ?about r = rotate ?about r

let align a b =
  let a = V2.normalize a
  and b = V2.normalize b in
  if V2.approx a b then id else zrot V2.(ccw_theta b -. ccw_theta a)

let[@inline] scale s = Gg.M3.scale2 s
let[@inline] xscale x = scale (v2 x 1.)
let[@inline] yscale y = scale (v2 1. y)

let mirror ax =
  let ax = V2.normalize ax in
  let x = V2.x ax
  and y = V2.y ax in
  let xx = 1. -. (2. *. x *. x)
  and xy = -2. *. x *. y
  and yy = 1. -. (2. *. y *. y) in
  v xx xy 0. xy yy 0. 0. 0. 1.

let skew xa ya =
  if not Float.(is_finite xa && is_finite ya)
  then invalid_arg "Skew angles must be finite."
  else v 1. (Float.tan xa) 0. (Float.tan ya) 1. 0. 0. 0. 1.

let transform t p =
  let x = (e00 t *. V2.x p) +. (e01 t *. V2.y p) +. e02 t
  and y = (e10 t *. V2.x p) +. (e11 t *. V2.y p) +. e12 t in
  V2.v x y

let lift (t : t) =
  Affine3.v
    (e00 t)
    (e01 t)
    0.
    (e02 t)
    (e10 t)
    (e11 t)
    0.
    (e12 t)
    0.
    0.
    1.
    0.
    (e20 t)
    (e21 t)
    0.
    (e22 t)

let to_string t =
  Printf.sprintf
    "[ [%f, %f, %f], [%f, %f, %f], [%f, %f, %f] ]"
    (e00 t)
    (e01 t)
    (e02 t)
    (e10 t)
    (e11 t)
    (e12 t)
    (e20 t)
    (e21 t)
    (e22 t)
