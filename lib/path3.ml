open V
include Path.Make (V3)
include PathSearch.Make (V3) (BallTree3) (PathSearch.TangentSign3)
include Arc3
include Rounding.Make (V3) (Arc3)
module Bez2 = Bezier.Make (V2)

let of_tups = List.map V3.of_tup
let of_path2 ?(plane = Plane.xy) = Path2.lift plane
let to_path2 ?(plane = Plane.xy) = List.map (Plane.project plane)

let bbox = function
  | [] -> invalid_arg "Cannot calculate bbox for empty path."
  | hd :: tl ->
    let f (bb : V3.bbox) p =
      let min = V3.lower_bounds bb.min p
      and max = V3.upper_bounds bb.max p in
      V3.{ min; max }
    in
    List.fold_left f V3.{ min = hd; max = hd } tl

let circle ?fn ?fa ?fs ?(plane = Plane.xy) r =
  Path2.lift plane (Path2.circle ?fn ?fa ?fs r)

let square ?center ?(plane = Plane.xy) dims = Path2.lift plane (Path2.square ?center dims)

let ellipse ?fn ?fa ?fs ?(plane = Plane.xy) radii =
  Path2.lift plane (Path2.ellipse ?fn ?fa ?fs radii)

let star ?(plane = Plane.xy) ~r1 ~r2 n = Path2.lift plane (Path2.star ~r1 ~r2 n)

let helix ?fn ?fa ?fs ?(left = true) ~n_turns ~pitch ?r2 r1 =
  let r2 = Option.value ~default:r1 r2 in
  let n_frags = Util.helical_fragments ?fn ?fa ?fs (Float.max r1 r2) in
  let r_step = (r2 -. r1) /. Float.of_int (n_turns * n_frags)
  and h_step = pitch /. Float.of_int n_frags
  and a_step = 2. *. Float.pi /. Float.of_int n_frags *. if left then -1. else 1. in
  let f i =
    let i = Float.of_int i in
    let r = r1 +. (r_step *. i)
    and a = a_step *. i in
    Float.(v3 (r *. cos a) (r *. sin a) (h_step *. i))
  in
  List.init ((n_frags * n_turns) + 1) f

let scaler ?ez dims =
  let f =
    match ez with
    | Some (p1, p2) ->
      let ez = Easing.make p1 p2 in
      fun u -> V2.lerp (v2 1. 1.) dims (ez u)
    | None -> V2.lerp (v2 1. 1.) dims
  in
  fun u -> Affine3.scale @@ V3.of_v2 ~z:1. @@ f u

let twister ?ez rot =
  let f =
    match ez with
    | Some (p1, p2) ->
      let ez = Easing.make p1 p2 in
      fun u -> ez u *. rot
    | None -> ( *. ) rot
  in
  fun u -> Quaternion.(to_affine @@ make (v3 0. 0. 1.) (f u))

let to_transforms ?(mode = `Auto) ?scale_ez ?twist_ez ?scale ?twist path =
  let p = Array.of_list path in
  let len = Array.length p
  and id _ = Affine3.id in
  let rel_pos =
    if Option.(is_some scale || is_some twist)
    then (
      let a = Array.of_list @@ cummulative_length path in
      for i = 0 to len - 1 do
        a.(i) <- a.(i) /. a.(len - 1)
      done;
      Array.get a )
    else Fun.const 0.
  in
  if len < 2 then invalid_arg "Invalid path (too few points).";
  let scaler = Util.value_map_opt ~default:id (scaler ?ez:scale_ez) scale
  and twister = Util.value_map_opt ~default:id (twister ?ez:twist_ez) twist
  and transformer =
    match mode with
    | `Euler ->
      let m = Quaternion.(to_affine @@ of_euler Float.(v3 (pi /. 2.) 0. (pi /. 2.))) in
      fun i ->
        let { x = dx; y = dy; z = dz } =
          if i = 0
          then V3.(p.(1) -@ p.(0))
          else if i = len - 1
          then V3.(p.(i) -@ p.(i - 1))
          else V3.(p.(i + 1) -@ p.(i - 1))
        in
        let ay = Float.atan2 dz (Float.sqrt ((dx *. dx) +. (dy *. dy)))
        and az = Float.atan2 dy dx in
        let q = Quaternion.of_euler (v3 0. (-.ay) az) in
        Affine3.(m %> Quaternion.(to_affine ~trans:p.(i) q))
    | _ ->
      let accum_qs =
        let local i =
          let p1 = p.(i)
          and p2 = p.(i + 1)
          and p3 = p.(i + 2) in
          Quaternion.align V3.(normalize (p2 -@ p1)) V3.(normalize (p3 -@ p2))
        in
        match List.init (len - 2) local with
        | [] -> [| Quaternion.id |]
        | [ q ] -> [| q; Quaternion.id |]
        | hd :: tl ->
          let f (acc, qs) m =
            let q = Quaternion.mul m acc in
            q, q :: qs
          in
          let _, qs = List.fold_left f (hd, [ hd; Quaternion.id ]) tl in
          Util.array_of_list_rev qs
      in
      let init =
        match mode with
        | `Auto ->
          let cardinal =
            (* Determine an appropriate axis to pre-align the 2d shape with
                 (from normal of {x = 0.; y = 0.; z = 1.}), BEFORE alignment
                 with the initial tangent of the path. Adjust for sign of major
                 axes to prevent inconsistent flipping. *)
            let similarity a b = V3.dot a b /. V3.(norm a *. norm b)
            and n = V3.(normalize (p.(1) -@ p.(0))) in
            let z = similarity n (v3 0. 0. 1.)
            and x = similarity n (v3 1. 0. 0.)
            and y = similarity n (v3 0. 1. 0.) in
            let abs_x = Float.abs x
            and abs_y = Float.abs y
            and abs_z = Float.abs z
            and sgn_x = Math.sign x
            and sgn_y = Math.sign y
            and sgn_z = Math.sign z in
            let comp a b =
              if Float.compare (Float.abs (a -. b)) 0.01 = 1 then Float.compare a b else 0
            in
            match comp abs_x abs_y, comp abs_x abs_z, comp abs_y abs_z with
            | 1, 1, _ -> v3 sgn_x 0. 0. (* x-axis *)
            | -1, _, 1 -> v3 0. sgn_y 0. (* y-axis *)
            | 0, -1, -1 -> v3 0. 0. sgn_z (* xy equal, but less than z *)
            | 0, _, _ -> v3 0. sgn_y 0. (* xy equal, roughly following plane *)
            | _ -> v3 0. 0. sgn_z
          in
          let d = V3.normalize V3.(p.(1) -@ p.(0)) in
          Quaternion.(to_affine @@ mul (align cardinal d) (align (v3 0. 0. 1.) cardinal))
        | `Align initial -> Affine3.align initial (v3 0. 0. 1.)
        | _ -> Affine3.id
      in
      fun i ->
        if i = 0
        then Affine3.(init %> translate p.(0))
        else Affine3.(init %> Quaternion.(to_affine ~trans:p.(i) accum_qs.(i - 1)))
  in
  let f i = Affine3.(scaler (rel_pos i) %> twister (rel_pos i) %> transformer i) in
  List.init len f

let helical_transforms
    ?fn
    ?fa
    ?fs
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?(left = true)
    ~n_turns
    ~pitch
    ?r2
    r1
  =
  let r2 = Option.value ~default:r1 r2 in
  let n_frags = Util.helical_fragments ?fn ?fa ?fs (Float.max r1 r2) in
  let rot_sign = if left then -1. else 1. in
  let a_step = 2. *. Float.pi /. Float.of_int n_frags *. rot_sign
  and ax =
    let a = Float.(atan2 (pitch /. of_int n_frags) (pi *. 2. *. r1 /. of_int n_frags)) in
    (a *. rot_sign) +. (Float.pi /. 2.)
  in
  let path = helix ?fn ?fa ?fs ~left ~n_turns ~pitch ~r2 r1 in
  let len = List.length path
  and id _ = Affine3.id in
  let rel_pos =
    if Option.(is_some scale || is_some twist)
    then (
      let a = Array.of_list @@ cummulative_length path in
      for i = 0 to len - 1 do
        a.(i) <- a.(i) /. a.(len - 1)
      done;
      Array.get a )
    else Fun.const 0.
  in
  let scaler = Util.value_map_opt ~default:id (scaler ?ez:scale_ez) scale
  and twister = Util.value_map_opt ~default:id (twister ?ez:twist_ez) twist in
  let f i trans =
    let eul = v3 ax 0. (a_step *. Float.of_int i) in
    Affine3.(
      scaler (rel_pos i)
      %> twister (rel_pos i)
      %> Quaternion.(to_affine ~trans (of_euler eul)))
  in
  List.mapi f path

let prune_transforms ?(min_dist = 0.05) ~shape = function
  | [] -> []
  | [ m ] -> [ 0, m ]
  | m0 :: transforms ->
    let f (acc, i, plane) m =
      let s' = Path3.affine m (shape i) in
      let valid = List.for_all (Plane.is_point_above ~eps:min_dist plane) s' in
      if valid then (i, m) :: acc, i + 1, Path3.to_plane s' else acc, i + 1, plane
    and plane = Path3.to_plane @@ Path3.affine m0 (shape 0) in
    let transforms, _, _ = List.fold_left f ([ 0, m0 ], 1, plane) transforms in
    List.rev transforms

let normal = function
  | p0 :: p1 :: p2 :: poly ->
    let area_vec =
      let f (sum, last) p =
        let c = V3.(cross (sub last p0) (sub p last)) in
        V3.add c sum, p
      in
      fst @@ List.fold_left f (f (V3.zero, p1) p2) poly
    in
    V3.(normalize @@ neg area_vec)
  | _ -> invalid_arg "Too few points to calculate path normal."

let coplanar ?eps t =
  try Plane.are_points_on ?eps (Plane.of_normal @@ normal t) t with
  (* too few points, or co-linear *)
  | Invalid_argument _ -> false

let to_plane ?eps = function
  | [ p0; p1; p2 ] -> Plane.make p0 p1 p2
  | point :: _ as t ->
    let plane = Plane.of_normal ~point (normal t) in
    if Plane.are_points_on ?eps plane t
    then plane
    else invalid_arg "Path is not coplanar."
  | _ -> invalid_arg "Path must contain at least 3 points to define a plane."

let project plane = to_path2 ~plane

let centroid ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Polygon must have more than two points."
  | p0 :: p1 :: tl as t ->
    let plane = to_plane t in
    if not @@ Plane.are_points_on ~eps plane t
    then invalid_arg "Polygon must be coplanar.";
    let n = Plane.normal plane in
    let f (area_sum, p_sum, p1) p2 =
      let area = V3.(dot (cross (sub p2 p0) (sub p1 p0)) n) in
      area +. area_sum, V3.(add p_sum (smul (p0 +@ p1 +@ p2) area)), p2
    in
    let area_sum, p_sum, _ = List.fold_left f (0., V3.zero, p1) tl in
    if Math.approx ~eps area_sum 0.
    then invalid_arg "The polygon is self-intersecting, or its points are collinear.";
    V3.(sdiv p_sum (area_sum *. 3.))

let area ?(signed = false) = function
  | [] | [ _ ] | [ _; _ ] -> 0.
  | p0 :: p1 :: tl as t ->
    let plane = to_plane t in
    if not @@ Plane.are_points_on plane t then invalid_arg "Polygon must be coplanar.";
    let n = Plane.normal plane in
    let f (area, p1) p2 = (area +. V3.(dot (cross (sub p1 p0) (sub p2 p0)) n)), p2 in
    let area, _ = List.fold_left f (0., p1) tl in
    if signed then area else Float.abs area

include
  PathMatch.Make
    (V3)
    (struct
      let centroid = centroid
      let closest_tangent = closest_tangent
    end)

let translate p = List.map (V3.translate p)
let xtrans x = List.map (V3.xtrans x)
let ytrans y = List.map (V3.ytrans y)
let ztrans z = List.map (V3.ztrans z)
let rotate ?about r = List.map (V3.rotate ?about r)
let xrot ?about r = List.map (V3.xrot ?about r)
let yrot ?about r = List.map (V3.yrot ?about r)
let zrot ?about r = List.map (V3.zrot ?about r)
let quaternion ?about q = List.map (Quaternion.transform ?about q)
let axis_rotate ?about ax r = quaternion ?about (Quaternion.make ax r)
let affine m = List.map (Affine3.transform m)
let scale s = List.map (V3.scale s)
let xscale x = List.map (V3.xscale x)
let yscale y = List.map (V3.yscale y)
let zscale z = List.map (V3.zscale z)
let mirror ax = List.map (V3.mirror ax)
