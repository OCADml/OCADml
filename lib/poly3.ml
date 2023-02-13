type t =
  { outer : V3.t list
  ; holes : V3.t list list
  }

let of_poly2 ?(plane = Plane.xy) Poly2.{ outer; holes } =
  let f = Path2.lift plane in
  { outer = f outer; holes = List.map f holes }

let to_poly2 ?(validate = false) ?(plane = Plane.xy) { outer; holes } =
  let f = Path3.project plane in
  Poly2.make ~validate ~holes:(List.map f holes) (f outer)

let make ?(validate = true) ?(holes = []) outer =
  let plane = Path3.to_plane outer in
  if not validate
  then { outer; holes }
  else if let coplanar = Plane.are_points_on plane in
          coplanar outer && List.for_all coplanar holes
  then of_poly2 ~plane @@ to_poly2 ~validate:true ~plane { outer; holes }
  else invalid_arg "Polygon contains non-coplanar points."

let of_paths ?validate = function
  | [ outer ] | ([] as outer) -> make ?validate outer
  | outer :: holes -> make ?validate ~holes outer

let[@inline] of_list l = of_paths ~validate:false l
let[@inline] to_list t = t.outer :: t.holes

let add_holes ?validate ~holes t =
  make ?validate ~holes:(List.rev_append t.holes holes) t.outer

let circle ?fn ?fa ?fs ?plane r = of_poly2 ?plane @@ Poly2.circle ?fn ?fa ?fs r

let wedge ?fn ?fa ?fs ?plane ~centre ~radius ~start angle =
  make @@ Path3.arc ?fn ?fa ?fs ?plane ~wedge:true ~centre ~radius ~start angle

let square ?center ?plane dims = of_poly2 ?plane @@ Poly2.square ?center dims
let ellipse ?fn ?fa ?fs ?plane radii = of_poly2 ?plane @@ Poly2.ellipse ?fn ?fa ?fs radii
let star ?plane ~r1 ~r2 n = of_poly2 ?plane @@ Poly2.star ~r1 ~r2 n

let ring ?fn ?fa ?fs ?plane ~thickness r =
  of_poly2 ?plane @@ Poly2.ring ?fn ?fa ?fs ~thickness r

let box ?center ?plane ~thickness dims =
  of_poly2 ?plane @@ Poly2.box ?center ~thickness dims

let bbox { outer; _ } = Path3.bbox outer
let centroid ?eps { outer; _ } = Path3.centroid ?eps outer

let area ?signed { outer; holes } =
  Path3.area ?signed outer
  -. List.fold_left (fun sum h -> Path3.area ?signed h +. sum) 0. holes

let map f { outer; holes } = { outer = f outer; holes = List.map f holes }

let offset ?fn ?fs ?fa ?check_valid ?mode d t =
  let plane = Path3.to_plane t.outer in
  let f p =
    Offset.offset ?fn ?fs ?fa ~closed:true ?check_valid ?mode d (Path2.of_path3 ~plane p)
    |> Path2.lift plane
  in
  map f t

let translate p = map (Path3.translate p)
let xtrans x = map (Path3.xtrans x)
let ytrans y = map (Path3.ytrans y)
let ztrans z = map (Path3.ztrans z)
let rotate ?about r = map (Path3.rotate ?about r)
let xrot ?about r = map (Path3.xrot ?about r)
let yrot ?about r = map (Path3.yrot ?about r)
let zrot ?about r = map (Path3.zrot ?about r)
let affine m = map (Path3.affine m)
let quaternion ?about q = map (Path3.quaternion ?about q)
let axis_rotate ?about ax r = quaternion ?about (Quaternion.make ax r)
let scale s = map (Path3.scale s)
let xscale x = map (Path3.xscale x)
let yscale y = map (Path3.yscale y)
let zscale z = map (Path3.zscale z)
let mirror ax = map (Path3.mirror ax)
