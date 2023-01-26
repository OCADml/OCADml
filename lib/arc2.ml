open V

let arc
    ?(rev = false)
    ?fn
    ?fa
    ?fs
    ?(wedge = false)
    ~centre:(c : V2.t)
    ~radius
    ~start
    angle
  =
  let fn = Util.helical_fragments ?fn ?fa ?fs radius in
  let a_step = angle /. Float.of_int fn *. if rev then 1. else -1. in
  let f _ (acc, a) =
    let p = v2 ((Float.cos a *. radius) +. V2.x c) ((Float.sin a *. radius) +. V2.y c) in
    p :: acc, a +. a_step
  and init = if wedge then [ c ] else [] in
  fst @@ Util.fold_init (fn + 1) f (init, if rev then start else start +. angle)

let arc_about_centre ?rev ?fn ?fa ?fs ?dir ?wedge ~centre p1 p2 =
  let radius = V2.distance centre p1
  and start =
    let d = V2.sub p1 centre in
    Float.atan2 (V2.y d) (V2.x d)
  and angle =
    let a = V2.angle_points p1 centre p2
    and d = V2.clockwise_sign p1 p2 centre in
    match d, dir with
    | 0., None ->
      invalid_arg "Co-linear points don't define unique arc. Must specify dir."
    | 0., Some `CW -> (2. *. Float.pi) -. a
    | 0., Some `CCW -> ((2. *. Float.pi) -. a) *. -1.
    | -1., Some `CW | 1., Some `CCW -> ((2. *. Float.pi) -. a) *. Float.neg d
    | _ -> d *. a
  in
  arc ?rev ?fn ?fa ?fs ?wedge ~centre ~radius ~start angle

let arc_through ?rev ?fn ?fa ?fs ?wedge p1 p2 p3 =
  if V2.collinear p1 p2 p3 then invalid_arg "Arc points must form a valid triangle.";
  let centre =
    let x1 = V2.x p1
    and y1 = V2.y p1
    and x2 = V2.x p2
    and y2 = V2.y p2
    and x3 = V2.x p3
    and y3 = V2.y p3 in
    let d = (2. *. (x1 -. x3) *. (y3 -. y2)) +. (2. *. (x2 -. x3) *. (y1 -. y3))
    and m1 = V2.dot p1 p1 -. V2.dot p3 p3
    and m2 = V2.dot p3 p3 -. V2.dot p2 p2 in
    let nx = (m1 *. (y3 -. y2)) +. (m2 *. (y3 -. y1))
    and ny = (m1 *. (x2 -. x3)) +. (m2 *. (x1 -. x3)) in
    v2 (nx /. d) (ny /. d)
  and dir = if Float.equal (V2.clockwise_sign p1 p2 p3) 1. then `CW else `CCW in
  arc_about_centre ?rev ?fn ?fa ?fs ?wedge ~dir ~centre p1 p3
