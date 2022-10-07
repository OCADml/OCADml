(* Float record implementation (replacing the private float array arrays used
    previously) inspired by dbuenzli's gg library [https://github.com/dbuenzli/gg]. *)

open V

type row = float * float * float * float

type t =
  { r0c0 : float
  ; r0c1 : float
  ; r0c2 : float
  ; r0c3 : float
  ; r1c0 : float
  ; r1c1 : float
  ; r1c2 : float
  ; r1c3 : float
  ; r2c0 : float
  ; r2c1 : float
  ; r2c2 : float
  ; r2c3 : float
  ; r3c0 : float
  ; r3c1 : float
  ; r3c2 : float
  ; r3c3 : float
  }

let id =
  { r0c0 = 1.
  ; r0c1 = 0.
  ; r0c2 = 0.
  ; r0c3 = 0.
  ; r1c0 = 0.
  ; r1c1 = 1.
  ; r1c2 = 0.
  ; r1c3 = 0.
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  ; r2c3 = 0.
  ; r3c0 = 0.
  ; r3c1 = 0.
  ; r3c2 = 0.
  ; r3c3 = 1.
  }

let trace t = t.r0c0 +. t.r1c1 +. t.r2c2 +. t.r3c3

let get t r c =
  match r, c with
  | 0, 0 -> t.r0c0
  | 0, 1 -> t.r0c1
  | 0, 2 -> t.r0c2
  | 0, 3 -> t.r0c3
  | 1, 0 -> t.r1c0
  | 1, 1 -> t.r1c1
  | 1, 2 -> t.r1c2
  | 1, 3 -> t.r1c3
  | 2, 0 -> t.r2c0
  | 2, 1 -> t.r2c1
  | 2, 2 -> t.r2c2
  | 2, 3 -> t.r2c3
  | 3, 0 -> t.r3c0
  | 3, 1 -> t.r3c1
  | 3, 2 -> t.r3c2
  | 3, 3 -> t.r3c3
  | _    -> invalid_arg "Index out of bounds for 4x4 3d affine matrix."

let of_row_matrix_exn rm =
  if Array.(length rm = 4 && for_all (fun c -> length c = 4) rm)
  then
    { r0c0 = rm.(0).(0)
    ; r0c1 = rm.(0).(1)
    ; r0c2 = rm.(0).(2)
    ; r0c3 = rm.(0).(3)
    ; r1c0 = rm.(1).(0)
    ; r1c1 = rm.(1).(1)
    ; r1c2 = rm.(1).(2)
    ; r1c3 = rm.(1).(3)
    ; r2c0 = rm.(2).(0)
    ; r2c1 = rm.(2).(1)
    ; r2c2 = rm.(2).(2)
    ; r2c3 = rm.(2).(3)
    ; r3c0 = rm.(3).(0)
    ; r3c1 = rm.(3).(1)
    ; r3c2 = rm.(3).(2)
    ; r3c3 = rm.(3).(3)
    }
  else invalid_arg "Input is not square matrix of size 3."

let of_row_matrix rm =
  try Ok (of_row_matrix_exn rm) with
  | Invalid_argument msg -> Error msg

let mul a b =
  if a == id
  then b
  else if b == id
  then a
  else
    { r0c0 =
        (a.r0c0 *. b.r0c0)
        +. (a.r0c1 *. b.r1c0)
        +. (a.r0c2 *. b.r2c0)
        +. (a.r0c3 *. b.r3c0)
    ; r0c1 =
        (a.r0c0 *. b.r0c1)
        +. (a.r0c1 *. b.r1c1)
        +. (a.r0c2 *. b.r2c1)
        +. (a.r0c3 *. b.r3c1)
    ; r0c2 =
        (a.r0c0 *. b.r0c2)
        +. (a.r0c1 *. b.r1c2)
        +. (a.r0c2 *. b.r2c2)
        +. (a.r0c3 *. b.r3c2)
    ; r0c3 =
        (a.r0c0 *. b.r0c3)
        +. (a.r0c1 *. b.r1c3)
        +. (a.r0c2 *. b.r2c3)
        +. (a.r0c3 *. b.r3c3)
    ; r1c0 =
        (a.r1c0 *. b.r0c0)
        +. (a.r1c1 *. b.r1c0)
        +. (a.r1c2 *. b.r2c0)
        +. (a.r1c3 *. b.r3c0)
    ; r1c1 =
        (a.r1c0 *. b.r0c1)
        +. (a.r1c1 *. b.r1c1)
        +. (a.r1c2 *. b.r2c1)
        +. (a.r1c3 *. b.r3c1)
    ; r1c2 =
        (a.r1c0 *. b.r0c2)
        +. (a.r1c1 *. b.r1c2)
        +. (a.r1c2 *. b.r2c2)
        +. (a.r1c3 *. b.r3c2)
    ; r1c3 =
        (a.r1c0 *. b.r0c3)
        +. (a.r1c1 *. b.r1c3)
        +. (a.r1c2 *. b.r2c3)
        +. (a.r1c3 *. b.r3c3)
    ; r2c0 =
        (a.r2c0 *. b.r0c0)
        +. (a.r2c1 *. b.r1c0)
        +. (a.r2c2 *. b.r2c0)
        +. (a.r2c3 *. b.r3c0)
    ; r2c1 =
        (a.r2c0 *. b.r0c1)
        +. (a.r2c1 *. b.r1c1)
        +. (a.r2c2 *. b.r2c1)
        +. (a.r2c3 *. b.r3c1)
    ; r2c2 =
        (a.r2c0 *. b.r0c2)
        +. (a.r2c1 *. b.r1c2)
        +. (a.r2c2 *. b.r2c2)
        +. (a.r2c3 *. b.r3c2)
    ; r2c3 =
        (a.r2c0 *. b.r0c3)
        +. (a.r2c1 *. b.r1c3)
        +. (a.r2c2 *. b.r2c3)
        +. (a.r2c3 *. b.r3c3)
    ; r3c0 =
        (a.r3c0 *. b.r0c0)
        +. (a.r3c1 *. b.r1c0)
        +. (a.r3c2 *. b.r2c0)
        +. (a.r3c3 *. b.r3c0)
    ; r3c1 =
        (a.r3c0 *. b.r0c1)
        +. (a.r3c1 *. b.r1c1)
        +. (a.r3c2 *. b.r2c1)
        +. (a.r3c3 *. b.r3c1)
    ; r3c2 =
        (a.r3c0 *. b.r0c2)
        +. (a.r3c1 *. b.r1c2)
        +. (a.r3c2 *. b.r2c2)
        +. (a.r3c3 *. b.r3c2)
    ; r3c3 =
        (a.r3c0 *. b.r0c3)
        +. (a.r3c1 *. b.r1c3)
        +. (a.r3c2 *. b.r2c3)
        +. (a.r3c3 *. b.r3c3)
    }

let[@inline] element_wise op a b =
  { r0c0 = op a.r0c0 b.r0c0
  ; r0c1 = op a.r0c1 b.r0c1
  ; r0c2 = op a.r0c2 b.r0c2
  ; r0c3 = op a.r0c3 b.r0c3
  ; r1c0 = op a.r1c0 b.r1c0
  ; r1c1 = op a.r1c1 b.r1c1
  ; r1c2 = op a.r1c2 b.r1c2
  ; r1c3 = op a.r1c3 b.r1c3
  ; r2c0 = op a.r2c0 b.r2c0
  ; r2c1 = op a.r2c1 b.r2c1
  ; r2c2 = op a.r2c2 b.r2c2
  ; r2c3 = op a.r2c3 b.r2c3
  ; r3c0 = op a.r3c0 b.r3c0
  ; r3c1 = op a.r3c1 b.r3c1
  ; r3c2 = op a.r3c2 b.r3c2
  ; r3c3 = op a.r3c3 b.r3c3
  }

let[@inline] add a b = element_wise ( +. ) a b
let[@inline] sub a b = element_wise ( -. ) a b
let[@inline] ediv a b = element_wise ( /. ) a b
let[@inline] emul a b = element_wise ( *. ) a b

let transpose t =
  { r0c0 = t.r0c0
  ; r0c1 = t.r1c0
  ; r0c2 = t.r2c0
  ; r0c3 = t.r3c0
  ; r1c0 = t.r0c1
  ; r1c1 = t.r1c1
  ; r1c2 = t.r2c1
  ; r1c3 = t.r3c1
  ; r2c0 = t.r0c2
  ; r2c1 = t.r1c2
  ; r2c2 = t.r2c2
  ; r2c3 = t.r3c2
  ; r3c0 = t.r0c3
  ; r3c1 = t.r1c3
  ; r3c2 = t.r2c3
  ; r3c3 = t.r3c3
  }

let[@inline] map f t =
  { r0c0 = f t.r0c0
  ; r0c1 = f t.r0c1
  ; r0c2 = f t.r0c2
  ; r0c3 = f t.r0c3
  ; r1c0 = f t.r1c0
  ; r1c1 = f t.r1c1
  ; r1c2 = f t.r1c2
  ; r1c3 = f t.r1c3
  ; r2c0 = f t.r2c0
  ; r2c1 = f t.r2c1
  ; r2c2 = f t.r2c2
  ; r2c3 = f t.r2c3
  ; r3c0 = f t.r3c0
  ; r3c1 = f t.r3c1
  ; r3c2 = f t.r3c2
  ; r3c3 = f t.r3c3
  }

let[@inline] smul t s = map (( *. ) s) t
let[@inline] sdiv t s = map (( *. ) (1. /. s)) t
let[@inline] sadd t s = map (( +. ) s) t
let[@inline] ssub t s = map (( +. ) (-1. *. s)) t
let compose a b = mul b a
let[@inline] ( %> ) a b = compose a b
let[@inline] ( % ) a b = mul a b

let of_rows (sx, xy, xz, tx) (yx, sy, yz, ty) (zx, zy, sz, tz) =
  { r0c0 = sx
  ; r0c1 = xy
  ; r0c2 = xz
  ; r0c3 = tx
  ; r1c0 = yx
  ; r1c1 = sy
  ; r1c2 = yz
  ; r1c3 = ty
  ; r2c0 = zx
  ; r2c1 = zy
  ; r2c2 = sz
  ; r2c3 = tz
  ; r3c0 = 0.
  ; r3c1 = 0.
  ; r3c2 = 0.
  ; r3c3 = 1.
  }

let translate { x; y; z } =
  { r0c0 = 1.
  ; r0c1 = 0.
  ; r0c2 = 0.
  ; r0c3 = x
  ; r1c0 = 0.
  ; r1c1 = 1.
  ; r1c2 = 0.
  ; r1c3 = y
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  ; r2c3 = z
  ; r3c0 = 0.
  ; r3c1 = 0.
  ; r3c2 = 0.
  ; r3c3 = 1.
  }

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
      { r0c0 = 1.
      ; r0c1 = 0.
      ; r0c2 = 0.
      ; r0c3 = 0.
      ; r1c0 = 0.
      ; r1c1 = c
      ; r1c2 = -.s
      ; r1c3 = 0.
      ; r2c0 = 0.
      ; r2c1 = s
      ; r2c2 = c
      ; r2c3 = 0.
      ; r3c0 = 0.
      ; r3c1 = 0.
      ; r3c2 = 0.
      ; r3c3 = 1.
      }
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None   -> rot )

let yrot ?about r =
  if not @@ Float.is_finite r
  then invalid_arg "Y rotation angle must be finite."
  else (
    let rot =
      let c = Float.cos r
      and s = Float.sin r in
      { r0c0 = c
      ; r0c1 = 0.
      ; r0c2 = s
      ; r0c3 = 0.
      ; r1c0 = 0.
      ; r1c1 = 1.
      ; r1c2 = 0.
      ; r1c3 = 0.
      ; r2c0 = -.s
      ; r2c1 = 0.
      ; r2c2 = c
      ; r2c3 = 0.
      ; r3c0 = 0.
      ; r3c1 = 0.
      ; r3c2 = 0.
      ; r3c3 = 1.
      }
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None   -> rot )

let zrot ?about r =
  if not @@ Float.is_finite r
  then invalid_arg "Z rotation angle must be finite."
  else (
    let rot =
      let c = Float.cos r
      and s = Float.sin r in
      { r0c0 = c
      ; r0c1 = -.s
      ; r0c2 = 0.
      ; r0c3 = 0.
      ; r1c0 = s
      ; r1c1 = c
      ; r1c2 = 0.
      ; r1c3 = 0.
      ; r2c0 = 0.
      ; r2c1 = 0.
      ; r2c2 = 1.
      ; r2c3 = 0.
      ; r3c0 = 0.
      ; r3c1 = 0.
      ; r3c2 = 0.
      ; r3c3 = 1.
      }
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None   -> rot )

let rotate ?about { x; y; z } =
  if not @@ Float.(is_finite x && is_finite y && is_finite z)
  then invalid_arg "Euler rotation angles must be finite."
  else (
    let rot =
      let cx = Float.cos x
      and cy = Float.cos y
      and cz = Float.cos z
      and sx = Float.sin x
      and sy = Float.sin y
      and sz = Float.sin z in
      { r0c0 = cy *. cz
      ; r0c1 = (sx *. sy *. cz) -. (cx *. sz)
      ; r0c2 = (cx *. sy *. cz) +. (sx *. sz)
      ; r0c3 = 0.
      ; r1c0 = cy *. sz
      ; r1c1 = (sx *. sy *. sz) +. (cx *. cz)
      ; r1c2 = (sy *. cx *. sz) -. (sx *. cz)
      ; r1c3 = 0.
      ; r2c0 = -.sy
      ; r2c1 = sx *. cy
      ; r2c2 = cx *. cy
      ; r2c3 = 0.
      ; r3c0 = 0.
      ; r3c1 = 0.
      ; r3c2 = 0.
      ; r3c3 = 1.
      }
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None   -> rot )

let axis_rotate ?about ax r =
  if Math.approx r 0.
  then id
  else (
    let rot =
      let { x; y; z } = V3.normalize ax
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
      { r0c0 = xx +. c
      ; r0c1 = xy -. sz
      ; r0c2 = xz +. sy
      ; r0c3 = 0.
      ; r1c0 = xy +. sz
      ; r1c1 = yy +. c
      ; r1c2 = yz -. sx
      ; r1c3 = 0.
      ; r2c0 = xz -. sy
      ; r2c1 = yz +. sx
      ; r2c2 = zz +. c
      ; r2c3 = 0.
      ; r3c0 = 0.
      ; r3c1 = 0.
      ; r3c2 = 0.
      ; r3c3 = 1.
      }
    in
    match about with
    | Some p -> translate (V3.neg p) %> rot %> translate p
    | None   -> rot )

let scale { x; y; z } =
  { r0c0 = x
  ; r0c1 = 0.
  ; r0c2 = 0.
  ; r0c3 = 0.
  ; r1c0 = 0.
  ; r1c1 = y
  ; r1c2 = 0.
  ; r1c3 = 0.
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = z
  ; r2c3 = 0.
  ; r3c0 = 0.
  ; r3c1 = 0.
  ; r3c2 = 0.
  ; r3c3 = 1.
  }

let mirror ax =
  let { x; y; z } = V3.normalize ax in
  let xx = 1. -. (2. *. x *. x)
  and xy = -2. *. x *. y
  and xz = -2. *. x *. z
  and yy = 1. -. (2. *. y *. y)
  and yz = -2. *. y *. z
  and zz = 1. -. (2. *. z *. z) in
  { r0c0 = xx
  ; r0c1 = xy
  ; r0c2 = xz
  ; r0c3 = 0.
  ; r1c0 = xy
  ; r1c1 = yy
  ; r1c2 = yz
  ; r1c3 = 0.
  ; r2c0 = xz
  ; r2c1 = yz
  ; r2c2 = zz
  ; r2c3 = 0.
  ; r3c0 = 0.
  ; r3c1 = 0.
  ; r3c2 = 0.
  ; r3c3 = 1.
  }

let skew ?(xy = 0.) ?(xz = 0.) ?(yx = 0.) ?(yz = 0.) ?(zx = 0.) ?(zy = 0.) () =
  { r0c0 = 1.
  ; r0c1 = xy
  ; r0c2 = xz
  ; r0c3 = 0.
  ; r1c0 = yx
  ; r1c1 = 1.
  ; r1c2 = yz
  ; r1c3 = 0.
  ; r2c0 = zx
  ; r2c1 = zy
  ; r2c2 = 1.
  ; r2c3 = 0.
  ; r3c0 = 0.
  ; r3c1 = 0.
  ; r3c2 = 0.
  ; r3c3 = 1.
  }

let skew_xy xa ya =
  if not Float.(is_finite xa && is_finite ya)
  then invalid_arg "Skew angles must be finite."
  else
    { r0c0 = 1.
    ; r0c1 = Float.tan xa
    ; r0c2 = 0.
    ; r0c3 = 0.
    ; r1c0 = Float.tan ya
    ; r1c1 = 1.
    ; r1c2 = 0.
    ; r1c3 = 0.
    ; r2c0 = 0.
    ; r2c1 = 0.
    ; r2c2 = 1.
    ; r2c3 = 0.
    ; r3c0 = 0.
    ; r3c1 = 0.
    ; r3c2 = 0.
    ; r3c3 = 1.
    }

let skew_xz xa za =
  if not Float.(is_finite xa && is_finite za)
  then invalid_arg "Skew angles must be finite."
  else
    { r0c0 = 1.
    ; r0c1 = 0.
    ; r0c2 = Float.tan xa
    ; r0c3 = 0.
    ; r1c0 = 0.
    ; r1c1 = 1.
    ; r1c2 = 0.
    ; r1c3 = 0.
    ; r2c0 = Float.tan za
    ; r2c1 = 0.
    ; r2c2 = 1.
    ; r2c3 = 0.
    ; r3c0 = 0.
    ; r3c1 = 0.
    ; r3c2 = 0.
    ; r3c3 = 1.
    }

let skew_yz ya za =
  if not Float.(is_finite ya && is_finite za)
  then invalid_arg "Skew angles must be finite."
  else
    { r0c0 = 1.
    ; r0c1 = 0.
    ; r0c2 = 0.
    ; r0c3 = 0.
    ; r1c0 = 0.
    ; r1c1 = 1.
    ; r1c2 = Float.tan ya
    ; r1c3 = 0.
    ; r2c0 = 0.
    ; r2c1 = Float.tan za
    ; r2c2 = 1.
    ; r2c3 = 0.
    ; r3c0 = 0.
    ; r3c1 = 0.
    ; r3c2 = 0.
    ; r3c3 = 1.
    }

let align a b =
  let a = V3.normalize a
  and b = V3.normalize b in
  if V3.approx a b
  then id
  else if a.z = 0. && b.z = 0.
  then zrot V2.(ccw_theta (V3.to_v2 b) -. ccw_theta (V3.to_v2 a))
  else (
    let ax = V3.vector_axis a b
    and r = V3.angle a b in
    axis_rotate ax r )

let transform t { x; y; z } =
  { x = (t.r0c0 *. x) +. (t.r0c1 *. y) +. (t.r0c2 *. z) +. t.r0c3
  ; y = (t.r1c0 *. x) +. (t.r1c1 *. y) +. (t.r1c2 *. z) +. t.r1c3
  ; z = (t.r2c0 *. x) +. (t.r2c1 *. y) +. (t.r2c2 *. z) +. t.r2c3
  }

let to_string t =
  Printf.sprintf
    "[ [%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f] ]"
    t.r0c0
    t.r0c1
    t.r0c2
    t.r0c3
    t.r1c0
    t.r1c1
    t.r1c2
    t.r1c3
    t.r2c0
    t.r2c1
    t.r2c2
    t.r2c3
    t.r3c0
    t.r3c1
    t.r3c2
    t.r3c3
