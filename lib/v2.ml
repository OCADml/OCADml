type t = V.v2

type line =
  { a : t
  ; b : t
  }

let[@inline] v x y = Gg.V2.v x y
let zero = v 0. 0.
let[@inline] x t = Gg.V2.x t
let[@inline] y t = Gg.V2.y t
let[@inline] z _ = 0.
let[@inline] of_tup (x, y) = v x y
let[@inline] to_tup t = x t, y t
let[@inline] horizontal_op op a b = v (op (x a) (x b)) (op (y a) (y b))
let[@inline] add a b = Gg.V2.add a b
let[@inline] sub a b = Gg.V2.sub a b
let[@inline] mul a b = Gg.V2.mul a b
let[@inline] div a b = Gg.V2.div a b
let[@inline] neg t = Gg.V2.neg t
let[@inline] sadd t s = v (x t +. s) (y t +. s)
let[@inline] ssub t s = v (x t -. s) (y t -. s)
let[@inline] smul t s = Gg.V2.smul s t
let[@inline] sdiv t s = v (x t /. s) (y t /. s)
let[@inline] map f t = Gg.V2.map f t
let[@inline] equal a b = Float.equal (x a) (x b) && Float.equal (y a) (y b)

let compare a b =
  let x = Float.compare (x a) (x b) in
  if x = 0 then Float.compare (y a) (y b) else x

let norm t =
  let x = x t
  and y = y t in
  Float.sqrt ((x *. x) +. (y *. y))

let distance a b = norm (sub a b)
let approx ?(eps = Util.epsilon) a b = Float.(compare (distance a b) eps) < 1
let abs t = v (Float.abs (x t)) (Float.abs (y t))

let normalize t =
  let n = norm t in
  if n > 0. then sdiv t n else t

let ortho t = v (-.y t) (x t)
let dot a b = Gg.V2.dot a b
let cross a b = V.v3 0. 0. ((x a *. y b) -. (y a *. x b))
let mid a b = v ((x a +. x b) /. 2.) ((y a +. y b) /. 2.)

let mean l =
  let n, sum = List.fold_left (fun (i, s) t -> i + 1, add t s) (0, zero) l in
  sdiv sum (Int.to_float n)

let mean' a =
  let sum = ref zero
  and len = Array.length a in
  for i = 0 to len - 1 do
    sum := add !sum a.(i)
  done;
  sdiv !sum (Int.to_float len)

let lerp a b u =
  if u = 0. then a else if u = 1. then b else add (smul a (1. -. u)) (smul b u)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then Int.max 1 (n - 1) else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let angle a b = Float.acos (Math.clamp ~min:(-1.) ~max:1. (dot a b /. (norm a *. norm b)))
let ccw_theta t = Float.atan2 (y t) (x t)
let vector_axis a b = cross a b
let angle_points a b c = angle (sub a b) (sub c b)
let lower_bounds a b = v (Float.min (x a) (x b)) (Float.min (y a) (y b))
let upper_bounds a b = v (Float.max (x a) (x b)) (Float.max (y a) (y b))

let clockwise_sign ?(eps = Util.epsilon) a b c =
  let ba = sub b a
  and cb = sub c b in
  let crx_z = Gg.V3.z @@ cross ba cb in
  if Float.abs crx_z <= eps *. norm ba *. norm cb then 0. else Math.sign crx_z

let collinear p1 p2 p3 =
  let a = distance p1 p2
  and b = distance p2 p3
  and c = distance p3 p1 in
  a +. b < c || b +. c < a || c +. a < b

let distance_to_vector p v = norm (sub p (smul v (dot p v)))
let left_of_line ?eps ~line t = clockwise_sign ?eps t line.b line.a

let closest_simplex1 ?(eps = Util.epsilon) p1 p2 =
  if norm (sub p2 p1) <= eps *. (norm p1 +. norm p2) /. 2.
  then p1, [ p1 ]
  else (
    let c = sub p2 p1 in
    let t = -1. *. dot p1 c /. dot c c in
    if t < 0.
    then p1, [ p1 ]
    else if t > 1.
    then p2, [ p2 ]
    else add p1 (smul c t), [ p1; p2 ] )

let line_closest_point ?(bounds = false, false) ~line t =
  match bounds with
  | false, false ->
    let n = normalize (sub line.a line.b) in
    add line.b (smul n (dot (sub t line.b) n))
  | true, true -> add t (fst @@ closest_simplex1 (sub line.a t) (sub line.b t))
  | b1, b2 ->
    let line = if b1 && not b2 then line else { a = line.b; b = line.a } in
    let seg_vec = normalize (sub line.b line.a) in
    let projection = dot (sub t line.a) seg_vec in
    if projection <= 0. then line.a else add line.a (smul seg_vec projection)

let distance_to_line ?(bounds = false, false) ~line t =
  match bounds with
  | false, false -> distance_to_vector (sub t line.a) (normalize (sub line.b line.a))
  | bounds -> norm (sub t (line_closest_point ~bounds ~line t))

let point_on_line ?(eps = Util.epsilon) ?bounds ~line t =
  distance_to_line ?bounds ~line t < eps

let line_intersection
    ?(eps = Util.epsilon)
    ?(bounds1 = false, false)
    ?(bounds2 = false, false)
    l1
    l2
  =
  let d1 = sub l1.a l1.b
  and d2 = sub l2.a l2.b in
  let denominator = Gg.V3.z @@ cross d1 d2 in
  if Math.approx ~eps denominator 0.
  then None
  else (
    let v = sub l1.a l2.a in
    let a_frac =
      let num = Gg.V3.z @@ cross v d2 in
      num /. denominator
    and b_frac =
      let num = Gg.V3.z @@ cross v d1 in
      num /. denominator
    in
    let good =
      let bn_a1, bn_a2 = bounds1
      and bn_b1, bn_b2 = bounds2 in
      ((not bn_a1) || a_frac >= 0. -. eps)
      && ((not bn_a2) || a_frac <= 1. +. eps)
      && ((not bn_b1) || b_frac >= 0. -. eps)
      && ((not bn_b2) || b_frac <= 1. +. eps)
    in
    if good then Some (add l1.a (smul (sub l1.b l1.a) a_frac)) else None )

let line_normal a b = normalize (v (y a -. y b) (x b -. x a))
let to_v2 t = t
let to_string t = Printf.sprintf "[%f, %f]" (x t) (y t)
let deg_of_rad t = map Math.deg_of_rad t
let rad_of_deg t = map Math.rad_of_deg t
let[@inline] ( +@ ) a b = add a b
let[@inline] ( -@ ) a b = sub a b
let[@inline] ( *@ ) a b = mul a b
let[@inline] ( /@ ) a b = div a b
let[@inline] ( +$ ) a b = sadd a b
let[@inline] ( -$ ) a b = ssub a b
let[@inline] ( *$ ) a b = smul a b
let[@inline] ( /$ ) a b = sdiv a b
let[@inline] of_v3 p = v (Gg.V3.x p) (Gg.V3.y p)
let[@inline] to_v3 ?(z = 0.) t = V.v3 (x t) (y t) z
let[@inline] translate a b = add a b
let[@inline] xtrans d t = v (x t +. d) (y t)
let[@inline] ytrans d t = v (x t) (y t +. d)

let rotate ?about theta t =
  let s = Float.sin theta
  and c = Float.cos theta in
  let rot r =
    let rx = x r
    and ry = y r in
    let x = (rx *. c) -. (ry *. s)
    and y = (ry *. c) +. (rx *. s) in
    v x y
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let[@inline] zrot ?about theta t = rotate ?about theta t
let[@inline] scale a b = mul a b
let[@inline] xscale s t = v (x t *. s) (y t)
let[@inline] yscale s t = v (x t) (y t *. s)
let mirror ax t = sub t (smul ax (2. *. (dot t ax /. dot ax ax)))
