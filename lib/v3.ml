type t = V.v3 =
  { x : float
  ; y : float
  ; z : float
  }

type line =
  { a : t
  ; b : t
  }

type bbox =
  { min : t
  ; max : t
  }

let zero = { x = 0.; y = 0.; z = 0. }
let[@inline] v x y z = { x; y; z }
let of_tup (x, y, z) = { x; y; z }
let to_tup { x; y; z } = x, y, z
let[@inline] horizontal_op op a b = v (op a.x b.x) (op a.y b.y) (op a.z b.z)
let[@inline] add a b = v (a.x +. b.x) (a.y +. b.y) (a.z +. b.z)
let[@inline] sub a b = v (a.x -. b.x) (a.y -. b.y) (a.z -. b.z)
let[@inline] mul a b = v (a.x *. b.x) (a.y *. b.y) (a.z *. b.z)
let[@inline] div a b = v (a.x /. b.x) (a.y /. b.y) (a.z /. b.z)
let[@inline] neg t = v (t.x *. -1.) (t.y *. -1.) (t.z *. -1.)
let[@inline] sadd t s = v (t.x +. s) (t.y +. s) (t.z +. s)
let[@inline] ssub t s = v (t.x -. s) (t.y -. s) (t.z -. s)
let[@inline] smul t s = v (t.x *. s) (t.y *. s) (t.z *. s)
let[@inline] sdiv t s = v (t.x /. s) (t.y /. s) (t.z /. s)
let map f { x; y; z } = v (f x) (f y) (f z)
let equal a b = Float.equal a.x b.x && Float.equal a.y b.y && Float.equal a.z b.z

let compare a b =
  let x = Float.compare a.x b.x in
  if x = 0
  then (
    let y = Float.compare a.y b.y in
    if y = 0 then Float.compare a.z b.z else y )
  else x

let norm { x; y; z } = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))
let distance a b = norm (sub a b)

let approx ?(eps = Util.epsilon) a b =
  not (Int.equal Float.(compare (distance a b) eps) 1)

let abs { x; y; z } = v (Float.abs x) (Float.abs y) (Float.abs z)

let normalize t =
  let n = norm t in
  if n > 0. then sdiv t n else t

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let cross a b =
  let x = (a.y *. b.z) -. (a.z *. b.y)
  and y = (a.z *. b.x) -. (a.x *. b.z)
  and z = (a.x *. b.y) -. (a.y *. b.x) in
  { x; y; z }

let mid a b = v ((a.x +. b.x) /. 2.) ((a.y +. b.y) /. 2.) ((a.z +. b.z) /. 2.)

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
let ccw_theta { x; y; _ } = Float.atan2 y x
let lower_bounds a b = v (Float.min a.x b.x) (Float.min a.y b.y) (Float.min a.z b.z)
let upper_bounds a b = v (Float.max a.x b.x) (Float.max a.y b.y) (Float.max a.z b.z)
let bbox a b = { min = lower_bounds a b; max = upper_bounds a b }
let bbox_centroid bb = mid bb.min bb.max

let bbox_area bb =
  let x = bb.max.x -. bb.min.x
  and y = bb.max.y -. bb.min.y
  and z = bb.max.z -. bb.min.z in
  2. *. ((x *. y) +. (x *. z) +. (y *. z))

let bbox_intersect a b =
  let open Float in
  let min = v (max a.min.x b.min.x) (max a.min.y b.min.y) (max a.min.z a.min.z)
  and max = v (min a.max.x b.max.x) (min a.max.y b.max.y) (min a.max.z a.max.z) in
  if max.x < min.x || max.y < min.y || max.z < min.z then None else Some { min; max }

let bbox_volume bb =
  (bb.max.x -. bb.min.x) *. (bb.max.y -. bb.min.y) *. (bb.max.z -. bb.min.z)

let clockwise_sign ?(eps = Util.epsilon) a b c =
  let ba = sub b a
  and cb = sub c b in
  let { z; _ } = cross ba cb in
  if Float.abs z <= eps *. norm ba *. norm cb then 0. else Math.sign z

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
    else if norm Float.{ x = abs b.x; y = abs b.y; z = abs b.z } > eps
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

let get_x { x; _ } = x
let get_y { y; _ } = y
let get_z { z; _ } = z
let to_string { x; y; z } = Printf.sprintf "[%f, %f, %f]" x y z
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
let to_v2 { x; y; _ } = V.v2 x y
let of_v2 ?(z = 0.) ({ x; y } : V.v2) = { x; y; z }

let xrot ?about theta t =
  let rot { x; y; z } =
    let s = Float.sin theta
    and c = Float.cos theta in
    let y' = (y *. c) -. (z *. s)
    and z' = (z *. c) +. (y *. s) in
    v x y' z'
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let yrot ?about theta t =
  let rot { x; y; z } =
    let s = Float.sin theta
    and c = Float.cos theta in
    let x' = (x *. c) +. (z *. s)
    and z' = (z *. c) -. (x *. s) in
    v x' y z'
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let zrot ?about theta t =
  let rot { x; y; z } =
    let s = Float.sin theta
    and c = Float.cos theta in
    let x' = (x *. c) -. (y *. s)
    and y' = (y *. c) +. (x *. s) in
    v x' y' z
  in
  match about with
  | Some p -> sub t p |> rot |> add p
  | None -> rot t

let rotate ?about { x; y; z } t =
  match about with
  | Some p -> sub t p |> xrot x |> yrot y |> zrot z |> add p
  | None -> xrot x t |> yrot y |> zrot z

let[@inline] translate a b = add a b
let[@inline] xtrans d { x; y; z } = v (x +. d) y z
let[@inline] ytrans d { x; y; z } = v x (y +. d) z
let[@inline] ztrans d { x; y; z } = v x y (z +. d)
let[@inline] scale a b = mul a b
let[@inline] xscale s { x; y; z } = v (x *. s) y z
let[@inline] yscale s { x; y; z } = v x (y *. s) z
let[@inline] zscale s { x; y; z } = v x y (z *. s)
let mirror ax t = sub t (smul ax (2. *. (dot t ax /. dot ax ax)))
let projection { x; y; _ } = { x; y; z = 0. }
