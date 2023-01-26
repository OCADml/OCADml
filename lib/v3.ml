type t = V.v3

type line =
  { a : t
  ; b : t
  }

let[@inline] v x y z = Gg.V3.v x y z
let[@inline] x t = Gg.V3.x t
let[@inline] y t = Gg.V3.y t
let[@inline] z t = Gg.V3.z t
let zero = v 0. 0. 0.
let[@inline] of_tup tup = Gg.V3.of_tuple tup
let[@inline] to_tup t = Gg.V3.to_tuple t
let[@inline] horizontal_op op a b = v (op (x a) (x b)) (op (y a) (y b)) (op (z a) (z b))
let[@inline] add a b = Gg.V3.add a b
let[@inline] sub a b = Gg.V3.sub a b
let[@inline] mul a b = Gg.V3.mul a b
let[@inline] div a b = Gg.V3.div a b
let[@inline] neg t = Gg.V3.neg t
let[@inline] sadd t s = v (x t +. s) (y t +. s) (z t +. s)
let[@inline] ssub t s = v (x t -. s) (y t -. s) (z t -. s)
let[@inline] smul t s = Gg.V3.smul s t
let[@inline] sdiv t s = v (x t /. s) (y t /. s) (z t /. s)
let[@inline] map f t = Gg.V3.map f t

let equal a b =
  Float.equal (x a) (x b) && Float.equal (y a) (y b) && Float.equal (z a) (z b)

let compare a b =
  let x = Float.compare (x a) (x b) in
  if x = 0
  then (
    let y = Float.compare (y a) (y b) in
    if y = 0 then Float.compare (z a) (z b) else y )
  else x

let norm t =
  let x = x t
  and y = y t
  and z = z t in
  Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))

let distance a b = norm (sub a b)

let approx ?(eps = Util.epsilon) a b =
  not (Int.equal Float.(compare (distance a b) eps) 1)

let[@inline] abs t = map Float.abs t

let normalize t =
  let n = norm t in
  if n > 0. then sdiv t n else t

let[@inline] dot a b = Gg.V3.dot a b

let cross a b =
  let x = (y a *. z b) -. (z a *. y b)
  and y = (z a *. x b) -. (x a *. z b)
  and z = (x a *. y b) -. (y a *. x b) in
  v x y z

let mid a b = v ((x a +. x b) /. 2.) ((y a +. y b) /. 2.) ((z a +. z b) /. 2.)

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
let angle_points a b c = angle (sub a b) (sub c b)
let ccw_theta t = Float.atan2 (y t) (x t)

let lower_bounds a b =
  v (Float.min (x a) (x b)) (Float.min (y a) (y b)) (Float.min (z a) (z b))

let upper_bounds a b =
  v (Float.max (x a) (x b)) (Float.max (y a) (y b)) (Float.max (z a) (z b))

let clockwise_sign ?(eps = Util.epsilon) a b c =
  let ba = sub b a
  and cb = sub c b in
  let crx_z = z @@ cross ba cb in
  if Float.abs crx_z <= eps *. norm ba *. norm cb then 0. else Math.sign crx_z

let collinear p1 p2 p3 =
  let a = distance p1 p2
  and b = distance p2 p3
  and c = distance p3 p1 in
  a +. b < c || b +. c < a || c +. a < b

let vector_axis a b =
  let eps = 1e-6
  and a = normalize a
  and b = normalize b in
  let c =
    if norm (sub a b) > eps && norm (add a b) > eps
    then b
    else if norm (abs b) > eps
    then v 0. 0. 1.
    else v 1. 0. 0.
  in
  normalize (cross a c)

let distance_to_vector p v = norm (sub p (smul v (dot p v)))

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

let to_string t = Printf.sprintf "[%f, %f, %f]" (x t) (y t) (z t)
let deg_of_rad t = map (fun r -> 180.0 *. r /. Float.pi) t
let rad_of_deg t = map (fun d -> d *. Float.pi /. 180.) t
let[@inline] ( +@ ) a b = add a b
let[@inline] ( -@ ) a b = sub a b
let[@inline] ( *@ ) a b = mul a b
let[@inline] ( /@ ) a b = div a b
let[@inline] ( +$ ) a b = sadd a b
let[@inline] ( -$ ) a b = ssub a b
let[@inline] ( *$ ) a b = smul a b
let[@inline] ( /$ ) a b = sdiv a b
let[@inline] to_v2 t = Gg.V2.v (x t) (y t)
let[@inline] of_v2 ?(z = 0.) p = v (Gg.V2.x p) (Gg.V2.y p) z

let xrot ?about theta t =
  let rot t =
    let y = y t
    and z = z t in
    let s = Float.sin theta
    and c = Float.cos theta in
    let y' = (y *. c) -. (z *. s)
    and z' = (z *. c) +. (y *. s) in
    v (x t) y' z'
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let yrot ?about theta t =
  let rot t =
    let x = x t
    and z = z t in
    let s = Float.sin theta
    and c = Float.cos theta in
    let x' = (x *. c) +. (z *. s)
    and z' = (z *. c) -. (x *. s) in
    v x' (y t) z'
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let zrot ?about theta t =
  let rot t =
    let x = x t
    and y = y t in
    let s = Float.sin theta
    and c = Float.cos theta in
    let x' = (x *. c) -. (y *. s)
    and y' = (y *. c) +. (x *. s) in
    v x' y' (z t)
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let rotate ?about r t =
  match about with
  | Some p -> sub t p |> xrot (x r) |> yrot (y r) |> zrot (z r) |> add p
  | None -> xrot (x r) t |> yrot (y r) |> zrot (z r)

let[@inline] translate a b = add a b
let[@inline] xtrans d t = v (x t +. d) (y t) (z t)
let[@inline] ytrans d t = v (x t) (y t +. d) (z t)
let[@inline] ztrans d t = v (x t) (y t) (z t +. d)
let[@inline] scale a b = mul a b
let[@inline] xscale s t = v (x t *. s) (y t) (z t)
let[@inline] yscale s t = v (x t) (y t *. s) (z t)
let[@inline] zscale s t = v (x t) (y t) (z t *. s)
let mirror ax t = sub t (smul ax (2. *. (dot t ax /. dot ax ax)))
let projection t = v (x t) (y t) 0.
