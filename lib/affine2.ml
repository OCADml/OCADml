open V

type row = float * float * float

type t =
  { r0c0 : float
  ; r0c1 : float
  ; r0c2 : float
  ; r1c0 : float
  ; r1c1 : float
  ; r1c2 : float
  ; r2c0 : float
  ; r2c1 : float
  ; r2c2 : float
  }

let id =
  { r0c0 = 1.
  ; r0c1 = 0.
  ; r0c2 = 0.
  ; r1c0 = 0.
  ; r1c1 = 1.
  ; r1c2 = 0.
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  }

let trace t = t.r0c0 +. t.r1c1 +. t.r2c2

let get t r c =
  match r, c with
  | 0, 0 -> t.r0c0
  | 0, 1 -> t.r0c1
  | 0, 2 -> t.r0c2
  | 1, 0 -> t.r1c0
  | 1, 1 -> t.r1c1
  | 1, 2 -> t.r1c2
  | 2, 0 -> t.r2c0
  | 2, 1 -> t.r2c1
  | 2, 2 -> t.r2c2
  | _    -> invalid_arg "Index out of bounds for 3x3 2d affine matrix."

let of_row_matrix_exn rm =
  if Array.(length rm = 3 && for_all (fun c -> length c = 3) rm)
  then
    { r0c0 = rm.(0).(0)
    ; r0c1 = rm.(0).(1)
    ; r0c2 = rm.(0).(2)
    ; r1c0 = rm.(1).(0)
    ; r1c1 = rm.(1).(1)
    ; r1c2 = rm.(1).(2)
    ; r2c0 = rm.(2).(0)
    ; r2c1 = rm.(2).(1)
    ; r2c2 = rm.(2).(2)
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
    { r0c0 = (a.r0c0 *. b.r0c0) +. (a.r0c1 *. b.r1c0) +. (a.r0c2 *. b.r2c0)
    ; r0c1 = (a.r0c0 *. b.r0c1) +. (a.r0c1 *. b.r1c1) +. (a.r0c2 *. b.r2c1)
    ; r0c2 = (a.r0c0 *. b.r0c2) +. (a.r0c1 *. b.r1c2) +. (a.r0c2 *. b.r2c2)
    ; r1c0 = (a.r1c0 *. b.r0c0) +. (a.r1c1 *. b.r1c0) +. (a.r1c2 *. b.r2c0)
    ; r1c1 = (a.r1c0 *. b.r0c1) +. (a.r1c1 *. b.r1c1) +. (a.r1c2 *. b.r2c1)
    ; r1c2 = (a.r1c0 *. b.r0c2) +. (a.r1c1 *. b.r1c2) +. (a.r1c2 *. b.r2c2)
    ; r2c0 = (a.r2c0 *. b.r0c0) +. (a.r2c1 *. b.r1c0) +. (a.r2c2 *. b.r2c0)
    ; r2c1 = (a.r2c0 *. b.r0c1) +. (a.r2c1 *. b.r1c1) +. (a.r2c2 *. b.r2c1)
    ; r2c2 = (a.r2c0 *. b.r0c2) +. (a.r2c1 *. b.r1c2) +. (a.r2c2 *. b.r2c2)
    }

let[@inline] element_wise op a b =
  { r0c0 = op a.r0c0 b.r0c0
  ; r0c1 = op a.r0c1 b.r0c1
  ; r0c2 = op a.r0c2 b.r0c2
  ; r1c0 = op a.r1c0 b.r1c0
  ; r1c1 = op a.r1c1 b.r1c1
  ; r1c2 = op a.r1c2 b.r1c2
  ; r2c0 = op a.r2c0 b.r2c0
  ; r2c1 = op a.r2c1 b.r2c1
  ; r2c2 = op a.r2c2 b.r2c2
  }

let[@inline] add a b = element_wise ( +. ) a b
let[@inline] sub a b = element_wise ( -. ) a b
let[@inline] ediv a b = element_wise ( /. ) a b
let[@inline] emul a b = element_wise ( *. ) a b

let transpose t =
  { r0c0 = t.r0c0
  ; r0c1 = t.r1c0
  ; r0c2 = t.r2c0
  ; r1c0 = t.r0c1
  ; r1c1 = t.r1c1
  ; r1c2 = t.r2c1
  ; r2c0 = t.r0c2
  ; r2c1 = t.r1c2
  ; r2c2 = t.r2c2
  }

let[@inline] map f t =
  { r0c0 = f t.r0c0
  ; r0c1 = f t.r0c1
  ; r0c2 = f t.r0c2
  ; r1c0 = f t.r1c0
  ; r1c1 = f t.r1c1
  ; r1c2 = f t.r1c2
  ; r2c0 = f t.r2c0
  ; r2c1 = f t.r2c1
  ; r2c2 = f t.r2c2
  }

let[@inline] smul t s = map (( *. ) s) t
let[@inline] sdiv t s = map (( *. ) (1. /. s)) t
let[@inline] sadd t s = map (( +. ) s) t
let[@inline] ssub t s = map (( +. ) (-1. *. s)) t
let compose a b = mul b a
let[@inline] ( %> ) a b = compose a b
let[@inline] ( % ) a b = mul a b

let of_rows (scale_x, skew_x, tx) (skew_y, scale_y, ty) =
  { r0c0 = scale_x
  ; r0c1 = skew_x
  ; r0c2 = tx
  ; r1c0 = skew_y
  ; r1c1 = scale_y
  ; r1c2 = ty
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  }

let translate { x; y } =
  { r0c0 = 1.
  ; r0c1 = 0.
  ; r0c2 = x
  ; r1c0 = 0.
  ; r1c1 = 1.
  ; r1c2 = y
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  }

let[@inline] xtrans x = translate (v2 x 0.)
let[@inline] ytrans y = translate (v2 0. y)

let rotate ?about r =
  if not @@ Float.is_finite r
  then invalid_arg "Rotation angle must be finite."
  else (
    let rot =
      let c = Float.cos r
      and s = Float.sin r in
      { r0c0 = c
      ; r0c1 = -.s
      ; r0c2 = 0.
      ; r1c0 = s
      ; r1c1 = c
      ; r1c2 = 0.
      ; r2c0 = 0.
      ; r2c1 = 0.
      ; r2c2 = 1.
      }
    in
    match about with
    | Some p -> translate (V2.neg p) %> rot %> translate p
    | None   -> rot )

let[@inline] zrot ?about r = rotate ?about r

let align a b =
  let a = V2.normalize a
  and b = V2.normalize b in
  if V2.approx a b then id else zrot V2.(ccw_theta b -. ccw_theta a)

let scale { x; y } =
  { r0c0 = x
  ; r0c1 = 0.
  ; r0c2 = 0.
  ; r1c0 = 0.
  ; r1c1 = y
  ; r1c2 = 0.
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  }

let[@inline] xscale x = scale (v2 x 1.)
let[@inline] yscale y = scale (v2 1. y)

let mirror ax =
  let { x; y } = V2.normalize ax in
  let xx = 1. -. (2. *. x *. x)
  and xy = -2. *. x *. y
  and yy = 1. -. (2. *. y *. y) in
  { r0c0 = xx
  ; r0c1 = xy
  ; r0c2 = 0.
  ; r1c0 = xy
  ; r1c1 = yy
  ; r1c2 = 0.
  ; r2c0 = 0.
  ; r2c1 = 0.
  ; r2c2 = 1.
  }

let skew xa ya =
  if not Float.(is_finite xa && is_finite ya)
  then invalid_arg "Skew angles must be finite."
  else
    { r0c0 = 1.
    ; r0c1 = Float.tan xa
    ; r0c2 = 0.
    ; r1c0 = Float.tan ya
    ; r1c1 = 1.
    ; r1c2 = 0.
    ; r2c0 = 0.
    ; r2c1 = 0.
    ; r2c2 = 1.
    }

let transform t { x; y } =
  { x = (t.r0c0 *. x) +. (t.r0c1 *. y) +. t.r0c2
  ; y = (t.r1c0 *. x) +. (t.r1c1 *. y) +. t.r1c2
  }

let lift (t : t) =
  Affine3.
    { r0c0 = t.r0c0
    ; r0c1 = t.r0c1
    ; r0c2 = 0.
    ; r0c3 = t.r0c2
    ; r1c0 = t.r1c0
    ; r1c1 = t.r1c1
    ; r1c2 = 0.
    ; r1c3 = t.r1c2
    ; r2c0 = 0.
    ; r2c1 = 0.
    ; r2c2 = 1.
    ; r2c3 = 0.
    ; r3c0 = t.r2c0
    ; r3c1 = t.r2c1
    ; r3c2 = 0.
    ; r3c3 = t.r2c2
    }

let to_string t =
  Printf.sprintf
    "[ [%f, %f, %f], [%f, %f, %f], [%f, %f, %f] ]"
    t.r0c0
    t.r0c1
    t.r0c2
    t.r1c0
    t.r1c1
    t.r1c2
    t.r2c0
    t.r2c1
    t.r2c2
