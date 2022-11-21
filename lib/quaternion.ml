type t = V.v4 =
  { x : float
  ; y : float
  ; z : float
  ; w : float
  }

let id = { x = 0.; y = 0.; z = 0.; w = 1. }

let make ax angle =
  let V.{ x; y; z } = V3.normalize ax in
  let s = Float.sin (angle /. 2.) in
  { x = x *. s; y = y *. s; z = z *. s; w = Float.cos (angle /. 2.) }

let basic_op op a b = { x = op a.x b.x; y = op a.y b.y; z = op a.z b.z; w = op a.w b.w }
let add = basic_op ( +. )
let sub = basic_op ( -. )
let sadd t s = { t with w = t.w +. s }
let ssub t s = { t with w = t.w -. s }
let ssub_neg t s = { x = -.t.x; y = -.t.y; z = -.t.z; w = s -. t.w }

let mul a b =
  let x = (a.y *. b.z) -. (a.z *. b.y) +. (b.w *. a.x) +. (a.w *. b.x)
  and y = (a.z *. b.x) -. (a.x *. b.z) +. (b.w *. a.y) +. (a.w *. b.y)
  and z = (a.x *. b.y) -. (a.y *. b.x) +. (b.w *. a.z) +. (b.z *. a.w)
  and w = (a.w *. b.w) -. (a.x *. b.x) -. (a.y *. b.y) -. (a.z *. b.z) in
  { x; y; z; w }

let smul t s = { x = t.x *. s; y = t.y *. s; z = t.z *. s; w = t.w *. s }
let sdiv t s = { x = t.x /. s; y = t.y /. s; z = t.z /. s; w = t.w /. s }
let neg q = smul q (-1.)
let norm { x; y; z; w } = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w))

let normalize t =
  let n = norm t in
  if n > 0. then sdiv t n else t

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z) +. (a.w *. b.w)
let conj t = { x = -.t.x; y = -.t.y; z = -.t.z; w = t.w }
let distance a b = norm (sub a b)

let of_euler V.{ x = roll; y = pitch; z = yaw } =
  let open Float in
  let cy = cos (yaw *. 0.5)
  and sy = sin (yaw *. 0.5)
  and cp = cos (pitch *. 0.5)
  and sp = sin (pitch *. 0.5)
  and cr = cos (roll *. 0.5)
  and sr = sin (roll *. 0.5) in
  let w = (cr *. cp *. cy) +. (sr *. sp *. sy)
  and x = (sr *. cp *. cy) -. (cr *. sp *. sy)
  and y = (cr *. sp *. cy) +. (sr *. cp *. sy)
  and z = (cr *. cp *. sy) -. (sr *. sp *. cy) in
  { x; y; z; w }

let to_euler { x; y; z; w } =
  let xx = x *. x
  and yy = y *. y
  and zz = z *. z
  and ww = w *. w
  and wx = w *. x
  and wy = w *. y
  and wz = w *. z
  and zx = z *. x
  and zy = z *. y
  and xy = x *. y in
  V3.
    { x = Float.atan2 (2. *. (zy +. wx)) (ww -. xx -. yy +. zz)
    ; y = Float.asin (-2. *. (zx -. wy))
    ; z = Float.atan2 (2. *. (xy +. wz)) (ww +. xx -. yy -. zz)
    }

let to_affine ?(trans = V3.zero) { x; y; z; w } =
  let s =
    let len_sqr = (x *. x) +. (y *. y) +. (z *. z) +. (w *. w) in
    if len_sqr != 0. then 2. /. len_sqr else 0.
  in
  let V.({ z = zs; _ } as xyzs) = V3.(smul (v x y z) s) in
  let V.{ x = xsw; y = ysw; z = zsw } = V3.smul xyzs w in
  let V.{ x = xsx; y = ysx; z = zsx } = V3.smul xyzs x
  and V.{ y = ysy; z = zsy; _ } = V3.smul xyzs y
  and zsz = z *. zs in
  Affine3.
    { r0c0 = 1. -. ysy -. zsz
    ; r0c1 = ysx -. zsw
    ; r0c2 = zsx +. ysw
    ; r0c3 = trans.x
    ; r1c0 = ysx +. zsw
    ; r1c1 = 1. -. xsx -. zsz
    ; r1c2 = zsy -. xsw
    ; r1c3 = trans.y
    ; r2c0 = zsx -. ysw
    ; r2c1 = zsy +. xsw
    ; r2c2 = 1. -. xsx -. ysy
    ; r2c3 = trans.z
    ; r3c0 = 0.
    ; r3c1 = 0.
    ; r3c2 = 0.
    ; r3c3 = 1.
    }

let slerp a b =
  let a = normalize a
  and b = normalize b in
  fun v ->
    let v = if v < 0. then 0. else if v > 1. then 1. else v in
    let compute a' b' d =
      let theta_0 = Float.acos d in
      let sin_theta_0 = Float.sin theta_0 in
      let theta = theta_0 *. v in
      let sin_theta = Float.sin theta in
      let s0 = Float.cos theta -. (d *. sin_theta /. sin_theta_0)
      and s1 = sin_theta /. sin_theta_0 in
      add (smul a' s0) (smul b' s1) |> normalize
    in
    (* If dot is negative, slerp won't take shorter path. Fix by reversing one quat.
     *  Dot is constrained for cases using compute, so acos is safe. *)
    match dot a b with
    | d when d < 0. -> compute (neg a) b (-.d)
    | d when d > 0.9995 -> add a (smul (sub b a) v) |> normalize
    | d -> compute a b d

let transform ?about t v =
  let aux V.{ x; y; z } =
    let r = { x; y; z; w = 0. } in
    let { x; y; z; _ } = mul (mul t r) (conj t) in
    V.{ x; y; z }
  in
  match about with
  | Some p -> V3.sub v p |> aux |> V3.add p
  | None -> aux v

let align v1 v2 =
  let dp = V3.dot v1 v2 in
  if dp > 0.999999 (* already parallel *)
  then id
  else if dp < -0.999999 (* opposite *)
  then (
    let x_cross = V3.(cross (v 1. 0. 0.) v1) in
    let axis =
      V3.normalize
      @@ if V3.norm x_cross < 0.000001 then V3.(cross (v 0. 1. 0.) v1) else x_cross
    in
    make axis Float.pi )
  else (
    let V.{ x; y; z } = V3.(cross v1 v2) in
    let w = V3.((norm v1 *. norm v2) +. dot v1 v2) in
    normalize { x; y; z; w } )

let to_string { x; y; z; w } = Printf.sprintf "[%f, %f, %f, %f]" x y z w
