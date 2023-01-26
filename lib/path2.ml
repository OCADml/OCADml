open V
include Path.Make (V2)
include PathSearch.Make (V2) (BallTree2) (PathSearch.TangentSign2)
include Arc2
include Rounding.Make (V2) (Arc2)

let of_tups = List.map V2.of_tup
let of_path3 ?(plane = Plane.xy) = List.map (Plane.project plane)
let to_path3 ?(plane = Plane.xy) = List.map (Plane.lift plane)

let clockwise_sign ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Path/polygon must have more than two points."
  | p0 :: p1 :: t ->
    let f (sum, last) p = (sum +. V2.((x last -. x p) *. (y last +. y p))), p in
    let sum, _ = List.fold_left f (f (0., p0) p1) t in
    if Math.approx ~eps sum 0. then 0. else Float.(of_int @@ compare sum 0.)

let is_clockwise ps = Float.equal 1. (clockwise_sign ps)

let self_intersections ?eps ?closed path =
  APath2.self_intersections ?eps ?closed (Array.of_list path)

let is_simple ?eps ?closed path = APath2.is_simple ?eps ?closed (Array.of_list path)

let bbox = function
  | [] -> Gg.Box2.empty
  | hd :: tl ->
    let f (min, max) p =
      let min = V2.lower_bounds min p
      and max = V2.upper_bounds max p in
      min, max
    in
    let min, max = List.fold_left f (hd, hd) tl in
    Gg.Box2.v min max

let centroid ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Polygon must have more than two points."
  | p0 :: p1 :: tl ->
    let f (area_sum, p_sum, p1) p2 =
      let area = V3.z V2.(cross (sub p2 p0) (sub p1 p0)) in
      area +. area_sum, V2.(add p_sum (smul (p0 +@ p1 +@ p2) area)), p2
    in
    let area_sum, p_sum, _ = List.fold_left f (0., V2.zero, p1) tl in
    if Math.approx ~eps area_sum 0.
    then invalid_arg "The polygon is self-intersecting, or its points are collinear.";
    V2.(sdiv p_sum (area_sum *. 3.))

let area ?(signed = false) = function
  | [] | [ _ ] | [ _; _ ] -> 0.
  | p0 :: p1 :: tl ->
    let f (area, p1) p2 = (area +. V2.(V3.z (cross (sub p1 p0) (sub p2 p0)))), p2 in
    let area, _ = List.fold_left f (0., p1) tl in
    (if signed then area else Float.abs area) /. 2.

let point_inside ?(eps = Util.epsilon) ?(nonzero = false) t p =
  if Gg.Box2.mem p (bbox t)
  then `Outside
  else (
    let segs = segment ~closed:true t in
    let exception OnBorder in
    if try
         let f (s : V2.line) =
           if V2.distance s.a s.b > eps
              && V2.point_on_line ~eps ~bounds:(true, true) ~line:s p
           then raise OnBorder
         in
         List.iter f segs;
         false
       with
       | OnBorder -> true
    then `OnBorder
    else if nonzero
    then (
      let f sum (s : V2.line) =
        let p0 = V2.sub s.a p
        and p1 = V2.sub s.b p in
        let w =
          if V2.distance p0 p1 > eps
          then (
            let c = V3.z (V2.cross p0 (V2.sub p1 p0)) in
            if V2.y p0 <= 0.
            then if V2.y p1 > 0. && c > 0. then 1 else 0
            else if V2.y p1 <= 0. && c < 0.
            then -1
            else 0 )
          else 0
        in
        w + sum
      in
      if List.fold_left f 0 segs <> 0 then `Inside else `Outside )
    else (
      let f crossings (s : V2.line) =
        let p0 = V2.sub s.a p
        and p1 = V2.sub s.b p in
        if V2.(
             ((y p1 > eps && y p0 <= eps) || (y p1 <= eps && y p0 > eps))
             && -.eps < x p0 -. (y p0 *. (x p1 -. x p0) /. (y p1 -. y p0)))
        then crossings + 1
        else crossings
      in
      if (2 * (List.fold_left f 0 segs mod 2)) - 1 > 0 then `Inside else `Outside ) )

include
  PathMatch.Make
    (V2)
    (struct
      let centroid = centroid
      let closest_tangent = closest_tangent
    end)

let offset = Offset.offset
let lift plane = to_path3 ~plane
let translate p = List.map (V2.translate p)
let xtrans x = List.map (V2.xtrans x)
let ytrans y = List.map (V2.ytrans y)
let rotate ?about r = List.map (V2.rotate ?about r)
let[@inline] zrot ?about r t = rotate ?about r t
let scale s = List.map (V2.scale s)
let xscale x = List.map (V2.xscale x)
let yscale y = List.map (V2.yscale y)
let mirror ax = List.map (V2.mirror ax)
let affine a = List.map (Affine2.transform a)
let affine3 m = List.map (fun v -> Affine3.transform m (V3.of_v2 v))
let quaternion ?about q = List.map (fun v -> Quaternion.transform ?about q (V3.of_v2 v))
let axis_rotate ?about ax r = quaternion ?about (Quaternion.make ax r)

let circle ?fn ?fa ?fs r =
  let fn = Util.helical_fragments ?fn ?fa ?fs r in
  let step = -2. *. Float.pi /. Float.of_int fn in
  let f i =
    let a = step *. Float.of_int i in
    v2 (r *. Float.cos a) (r *. Float.sin a)
  in
  List.init fn f

let square ?(center = false) dims =
  if center
  then (
    let w = V2.x dims /. 2.
    and h = V2.y dims /. 2. in
    V2.[ v w (-.h); v (-.w) (-.h); v (-.w) h; v w h ] )
  else V2.[ v 0. (y dims); v (x dims) (y dims); v (x dims) 0.; v 0. 0. ]

let ellipse ?fn ?fa ?fs radii =
  let x = V2.x radii
  and y = V2.y radii in
  let fn = Util.helical_fragments ?fn ?fa ?fs (Float.max x y) in
  let step = -2. *. Float.pi /. Float.of_int fn in
  let f i =
    let a = step *. Float.of_int i in
    v2 (x *. Float.cos a) (y *. Float.sin a)
  in
  List.init fn f

let star ~r1 ~r2 n =
  if n < 2 then invalid_arg "Cannot draw star path with less than 2 points.";
  let step = Float.(2. *. pi /. of_int n)
  and start_a = Float.pi /. -2.
  and pt r a = Float.(v2 (r *. cos a) (r *. sin a)) in
  let f i acc =
    let i = Float.of_int i in
    let a = start_a +. (step *. i) in
    pt r2 (a +. (step /. 2.)) :: pt r1 a :: acc
  in
  Util.fold_init n f []

let cubic_spline ?boundary ~fn ps = CubicSpline.(interpolate_path ~fn (fit ?boundary ps))

(* Adapted from BOSL2's hull2d_path in the geometry module
(https://github.com/revarbat/BOSL2/blob/46e15e50053e986f1370e8688b5c5b078ccf818e/geometry.scad#L2347)
which is based on the method described here:
https://www.hackerearth.com/practice/math/geometry/line-sweep-technique/tutorial/ *)

let hull ?(all = false) ps =
  let ps = Array.of_list ps in
  let len = Array.length ps in
  Array.fast_sort V2.compare ps;
  let is_cw =
    let lhs a b c = V3.z V2.(cross (a -@ c) (b -@ c))
    and rhs a b c = V2.(Util.epsilon *. distance a c *. distance b c) in
    if all
    then fun a b c -> lhs a b c <= rhs a b c
    else fun a b c -> lhs a b c < -.rhs a b c
  in
  let rec backtrack idx h stop i =
    match h with
    | a :: (b :: _ as rest) ->
      if i = stop || is_cw ps.(idx) ps.(a) ps.(b)
      then i, h
      else backtrack idx rest stop (i + 1)
    | _ -> i, h
  in
  if len < 2
  then []
  else (
    let h = ref [ 1; 0 ]
    and n = ref 2 in
    for i = 2 to len - 1 do
      let removed, h' = backtrack i !h (!n - 1) 0 in
      h := i :: h';
      n := !n - removed + 1
    done;
    let n_lower = !n in
    for i = 0 to len - 2 do
      let idx = len - 2 - i in
      let removed, h' = backtrack idx !h (!n - n_lower) 0 in
      h := if idx > 0 then idx :: h' else h';
      n := !n - removed + 1
    done;
    List.rev_map (Array.get ps) !h )

let triangulate ?eps poly =
  let poly = Array.of_list poly in
  Triangulate.triangulate ?eps poly |> List.map (List.map (fun i -> poly.(i)))
