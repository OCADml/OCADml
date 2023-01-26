(* Float record implementation (replacing the private float array arrays used
    previously) inspired by dbuenzli's gg library [https://github.com/dbuenzli/gg]. *)

open V

type row = float * float * float * float
type t = Gg.m4

let[@inline] v e00 e01 e02 e03 e10 e11 e12 e13 e20 e21 e22 e23 e30 e31 e32 e33 =
  Gg.M4.v e00 e01 e02 e03 e10 e11 e12 e13 e20 e21 e22 e23 e30 e31 e32 e33

let[@inline] e00 t = Gg.M4.e00 t
let[@inline] e01 t = Gg.M4.e01 t
let[@inline] e02 t = Gg.M4.e02 t
let[@inline] e03 t = Gg.M4.e03 t
let[@inline] e10 t = Gg.M4.e10 t
let[@inline] e11 t = Gg.M4.e11 t
let[@inline] e12 t = Gg.M4.e12 t
let[@inline] e13 t = Gg.M4.e13 t
let[@inline] e20 t = Gg.M4.e20 t
let[@inline] e21 t = Gg.M4.e21 t
let[@inline] e22 t = Gg.M4.e22 t
let[@inline] e23 t = Gg.M4.e23 t
let[@inline] e30 t = Gg.M4.e30 t
let[@inline] e31 t = Gg.M4.e31 t
let[@inline] e32 t = Gg.M4.e32 t
let[@inline] e33 t = Gg.M4.e33 t
let id = Gg.M4.id
let[@inline] trace t = Gg.M4.trace t
let get t r c = Gg.M4.el r c t

let of_row_matrix_exn rm =
  if Array.(length rm = 4 && for_all (fun c -> length c = 4) rm)
  then
    v
      rm.(0).(0)
      rm.(0).(1)
      rm.(0).(2)
      rm.(0).(3)
      rm.(1).(0)
      rm.(1).(1)
      rm.(1).(2)
      rm.(1).(3)
      rm.(2).(0)
      rm.(2).(1)
      rm.(2).(2)
      rm.(2).(3)
      rm.(3).(0)
      rm.(3).(1)
      rm.(3).(2)
      rm.(3).(3)
  else invalid_arg "Input is not square matrix of size 3."

let of_row_matrix rm =
  try Ok (of_row_matrix_exn rm) with
  | Invalid_argument msg -> Error msg

let[@inline] mul a b = Gg.M4.mul a b
let[@inline] add a b = Gg.M4.add a b
let[@inline] sub a b = Gg.M4.sub a b
let[@inline] ediv a b = Gg.M4.ediv a b
let[@inline] emul a b = Gg.M4.emul a b
let[@inline] transpose t = Gg.M4.transpose t
let[@inline] map f t = Gg.M4.map f t
let[@inline] smul t s = map (( *. ) s) t
let[@inline] sdiv t s = map (( *. ) (1. /. s)) t
let[@inline] sadd t s = map (( +. ) s) t
let[@inline] ssub t s = map (( +. ) (-1. *. s)) t
let compose a b = mul b a
let[@inline] ( %> ) a b = compose a b
let[@inline] ( % ) a b = mul a b

let of_rows (sx, xy, xz, tx) (yx, sy, yz, ty) (zx, zy, sz, tz) =
  v sx xy xz tx yx sy yz ty zx zy sz tz 0. 0. 0. 1.

let[@inline] translate p = Gg.M4.move3 p
let[@inline] xtrans x = translate (v3 x 0. 0.)
let[@inline] ytrans y = translate (v3 0. y 0.)
let[@inline] ztrans z = translate (v3 0. 0. z)

let xrot ?about r =
  if not @@ Float.is_finite r
  then invalid_arg "X rotation angle must be finite."
  else (
    let rot =
      let c = Float.cos r
      and s = Float.sin r in
      v 1. 0. 0. 0. 0. c (-.s) 0. 0. s c 0. 0. 0. 0. 1.
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None -> rot )

let yrot ?about r =
  if not @@ Float.is_finite r
  then invalid_arg "Y rotation angle must be finite."
  else (
    let rot =
      let c = Float.cos r
      and s = Float.sin r in
      v c 0. s 0. 0. 1. 0. 0. (-.s) 0. c 0. 0. 0. 0. 1.
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None -> rot )

let zrot ?about r =
  if not @@ Float.is_finite r
  then invalid_arg "Z rotation angle must be finite."
  else (
    let rot =
      let c = Float.cos r
      and s = Float.sin r in
      v c (-.s) 0. 0. s c 0. 0. 0. 0. 1. 0. 0. 0. 0. 1.
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None -> rot )

let rotate ?about r =
  let rx = V3.x r
  and ry = V3.y r
  and rz = V3.z r in
  if not @@ Float.(is_finite rx && is_finite ry && is_finite rz)
  then invalid_arg "Euler rotation angles must be finite."
  else (
    let rot =
      let cx = Float.cos rx
      and cy = Float.cos ry
      and cz = Float.cos rz
      and sx = Float.sin rx
      and sy = Float.sin ry
      and sz = Float.sin rz in
      v
        (cy *. cz)
        ((sx *. sy *. cz) -. (cx *. sz))
        ((cx *. sy *. cz) +. (sx *. sz))
        0.
        (cy *. sz)
        ((sx *. sy *. sz) +. (cx *. cz))
        ((sy *. cx *. sz) -. (sx *. cz))
        0.
        (-.sy)
        (sx *. cy)
        (cx *. cy)
        0.
        0.
        0.
        0.
        1.
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None -> rot )

let axis_rotate ?about ax r =
  if Math.approx r 0.
  then id
  else (
    let rot =
      let ax = V3.normalize ax in
      let x = V3.x ax
      and y = V3.y ax
      and z = V3.z ax
      and c = Float.cos r
      and s = Float.sin r in
      let c2 = 1. -. c in
      let xx = x *. x *. c2
      and xy = x *. y *. c2
      and xz = x *. z *. c2
      and yy = y *. y *. c2
      and yz = y *. z *. c2
      and zz = z *. z *. c2
      and sx = s *. x
      and sy = s *. y
      and sz = s *. z in
      v
        (xx +. c)
        (xy -. sz)
        (xz +. sy)
        0.
        (xy +. sz)
        (yy +. c)
        (yz -. sx)
        0.
        (xz -. sy)
        (yz +. sx)
        (zz +. c)
        0.
        0.
        0.
        0.
        1.
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None -> rot )

let[@inline] scale s = Gg.M4.scale3 s
let[@inline] xscale x = scale (v3 x 1. 1.)
let[@inline] yscale y = scale (v3 1. y 1.)
let[@inline] zscale z = scale (v3 1. 1. z)

let mirror ax =
  let ax = V3.normalize ax in
  let x = V3.x ax
  and y = V3.y ax
  and z = V3.z ax in
  let xx = 1. -. (2. *. x *. x)
  and xy = -2. *. x *. y
  and xz = -2. *. x *. z
  and yy = 1. -. (2. *. y *. y)
  and yz = -2. *. y *. z
  and zz = 1. -. (2. *. z *. z) in
  v xx xy xz 0. xy yy yz 0. xz yz zz 0. 0. 0. 0. 1.

let skew ?(xy = 0.) ?(xz = 0.) ?(yx = 0.) ?(yz = 0.) ?(zx = 0.) ?(zy = 0.) () =
  v 1. xy xz 0. yx 1. yz 0. zx zy 1. 0. 0. 0. 0. 1.

let skew_xy xa ya =
  if not Float.(is_finite xa && is_finite ya)
  then invalid_arg "Skew angles must be finite."
  else v 1. (Float.tan xa) 0. 0. (Float.tan ya) 1. 0. 0. 0. 0. 1. 0. 0. 0. 0. 1.

let skew_xz xa za =
  if not Float.(is_finite xa && is_finite za)
  then invalid_arg "Skew angles must be finite."
  else v 1. 0. (Float.tan xa) 0. 0. 1. 0. 0. (Float.tan za) 0. 1. 0. 0. 0. 0. 1.

let skew_yz ya za =
  if not Float.(is_finite ya && is_finite za)
  then invalid_arg "Skew angles must be finite."
  else v 1. 0. 0. 0. 0. 1. (Float.tan ya) 0. 0. (Float.tan za) 1. 0. 0. 0. 0. 1.

let align a b =
  let a = V3.normalize a
  and b = V3.normalize b in
  if V3.approx a b
  then id
  else if V3.z a = 0. && V3.z b = 0.
  then zrot V2.(ccw_theta (V3.to_v2 b) -. ccw_theta (V3.to_v2 a))
  else (
    let ax = V3.vector_axis a b
    and r = V3.angle a b in
    axis_rotate ax r )

let transform t p =
  let x = V3.x p
  and y = V3.y p
  and z = V3.z p in
  let x = (e00 t *. x) +. (e01 t *. y) +. (e02 t *. z) +. e03 t
  and y = (e10 t *. x) +. (e11 t *. y) +. (e12 t *. z) +. e13 t
  and z = (e20 t *. x) +. (e21 t *. y) +. (e22 t *. z) +. e23 t in
  v3 x y z

let to_string t =
  Printf.sprintf
    "[ [%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f] ]"
    (e00 t)
    (e01 t)
    (e02 t)
    (e03 t)
    (e10 t)
    (e11 t)
    (e12 t)
    (e13 t)
    (e20 t)
    (e21 t)
    (e22 t)
    (e23 t)
    (e30 t)
    (e31 t)
    (e32 t)
    (e33 t)
