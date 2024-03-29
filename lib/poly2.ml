type invalid =
  [ `SelfIntersection of int
  | `CrossIntersection of int * int
  | `DuplicatePoints
  ]

exception InvalidPoly of invalid

let invalid_printer exn =
  let path idx =
    if idx = 0 then "outer path" else Printf.sprintf "hole path at index %i" (idx - 1)
  and prelude = "Poly2 validation failed due to" in
  match exn with
  | InvalidPoly (`SelfIntersection i) ->
    Some (Printf.sprintf "%s self-intersection within the %s." prelude (path i))
  | InvalidPoly (`CrossIntersection (i, j)) ->
    Some
      (Printf.sprintf
         "%s cross-intersection between the %s and the %s."
         prelude
         (path i)
         (path j) )
  | InvalidPoly `DuplicatePoints ->
    Some (Printf.sprintf "%s presence of duplicate points (across all paths)." prelude)
  | _ -> None

let () = Printexc.register_printer invalid_printer

type t =
  { outer : V2.t list
  ; holes : V2.t list list
  }

let validation ?(eps = Util.epsilon) = function
  | { outer = [] | [ _ ] | [ _; _ ]; _ } -> invalid_arg "Outer path has too few points."
  | { outer; holes } ->
    let paths = Array.map Array.of_list (Array.of_list (outer :: holes)) in
    Array.iteri
      (fun i p ->
        if not @@ APath2.is_simple ~eps ~closed:true p
        then raise (InvalidPoly (`SelfIntersection i)) )
      paths;
    (* check for intersections *)
    let n = Array.length paths
    and p1_idx = ref 0 in
    while !p1_idx < n - 1 do
      let p1 = paths.(!p1_idx) in
      let len_p1 = Array.length p1
      and i = ref 0 in
      while !i < len_p1 - 1 do
        let a = p1.(!i)
        and b = p1.(Util.index_wrap ~len:len_p1 (!i + 1)) in
        let diff = V2.sub b a in
        let dist = V2.norm diff in
        if dist > eps
        then (
          let s1 = V2.{ a; b } in
          let s1_normal = V2.(v (-.y diff /. dist) (x diff /. dist)) in
          let ref_v = V2.dot s1.a s1_normal
          and p2_idx = ref (!p1_idx + 1) in
          while !p2_idx < n do
            let last_signal = ref 0
            and p2 = paths.(!p2_idx) in
            let len_p2 = Array.length p2 in
            for j = 0 to len_p2 - 1 do
              let v = V2.dot p2.(j) s1_normal -. ref_v in
              if Float.abs v >= eps
              then (
                let signal = Int.of_float @@ Math.sign v in
                if signal * !last_signal <= 0
                   && V2.line_intersection
                        ~eps
                        ~bounds1:(true, true)
                        ~bounds2:(true, true)
                        s1
                        V2.{ a = p2.(j); b = p2.(Util.index_wrap ~len:len_p2 (j + 1)) }
                      |> Option.is_some
                then raise (InvalidPoly (`CrossIntersection (!p1_idx, !p2_idx)));
                last_signal := signal )
            done;
            incr p2_idx
          done;
          incr i )
      done;
      incr p1_idx
    done;
    (* check for duplicate points *)
    let pts = Util.flatten_array paths in
    let len = Array.length pts in
    if len < 400
    then
      for i = 0 to len - 2 do
        for j = i + 1 to len - 1 do
          if V2.approx ~eps pts.(i) pts.(j) then raise (InvalidPoly `DuplicatePoints)
        done
      done
    else (
      let tree = BallTree2.make' pts in
      for i = 1 to len - 1 do
        match BallTree2.search_idxs ~radius:eps tree pts.(i) with
        | [] | [ _ ] -> () (* single result will be self *)
        | _ -> raise (InvalidPoly `DuplicatePoints)
      done )

let is_simple ?eps t =
  try
    validation ?eps t;
    true
  with
  | _ -> false

let make ?(validate = true) ?(holes = []) outer =
  let rewind =
    match holes with
    | [] -> Fun.id
    | _ ->
      let outer_sign = Path2.clockwise_sign outer in
      fun p -> if Path2.clockwise_sign p = outer_sign then List.rev p else p
  in
  let t = { outer; holes = List.map rewind holes } in
  if validate
  then (
    validation t;
    t )
  else t

let of_paths ?validate = function
  | [ outer ] | ([] as outer) -> make ?validate outer
  | outer :: holes -> make ?validate ~holes outer

let[@inline] of_list l = of_paths ~validate:false l
let[@inline] to_list t = t.outer :: t.holes

let of_seq s =
  let[@tail_mod_cons] rec loop s =
    match Seq.uncons s with
    | Some (hd, tl) -> List.of_seq hd :: loop tl
    | None -> []
  in
  match Seq.uncons s with
  | Some (outer, holes) ->
    let outer = List.of_seq outer in
    make ~validate:false ~holes:(loop holes) outer
  | None -> make ~validate:false []

let to_seq t =
  let holes = Seq.map List.to_seq @@ List.to_seq t.holes in
  Seq.cons (List.to_seq t.outer) holes

let of_array a = of_list @@ Array.fold_right (fun h acc -> Array.to_list h :: acc) a []

let to_array t =
  let n_holes = List.length t.holes in
  let a = Array.make (n_holes + 1) [||] in
  a.(0) <- Array.of_list t.outer;
  List.iteri (fun i h -> a.(i + 1) <- Array.of_list h) t.holes;
  a

let add_holes ?validate ~holes t =
  make ?validate ~holes:(List.rev_append t.holes holes) t.outer

let circle ?fn ?fa ?fs r = make @@ Path2.circle ?fn ?fa ?fs r

let wedge ?fn ?fa ?fs ~centre ~radius ~start angle =
  if Math.approx 0. angle || Float.abs angle >= 2. *. Float.pi
  then invalid_arg "Wedge angle must not be 0, or greater than +/- 2π.";
  make @@ Path2.arc ?fn ?fa ?fs ~wedge:true ~centre ~radius ~start angle

let square ?center dims = make (Path2.square ?center dims)
let ellipse ?fn ?fa ?fs radii = make @@ Path2.ellipse ?fn ?fa ?fs radii
let star ~r1 ~r2 n = make (Path2.star ~r1 ~r2 n)

let ring ?fn ?fa ?fs ~thickness radii =
  if V2.(
       x thickness < x radii
       && y thickness < y radii
       && x thickness > 0.
       && y thickness > 0. )
  then
    make
      ~holes:[ List.rev @@ Path2.ellipse ?fn ?fa ?fs (V2.sub radii thickness) ]
      (Path2.ellipse ?fn ?fa ?fs radii)
  else invalid_arg "Ring thickness must be less than the outer radius and above zero."

let box ?center ~thickness dims =
  if V2.(
       x thickness < x dims
       && y thickness < y dims
       && x thickness > 0.
       && y thickness > 0. )
  then (
    let holes = [ List.rev @@ Path2.square ?center (V2.sub dims thickness) ] in
    make ~holes (Path2.square ?center dims) )
  else
    invalid_arg "Box thicknesses must be less than the outer dimensions and above zero."

let bbox { outer; _ } = Path2.bbox outer
let centroid ?eps { outer; _ } = Path2.centroid ?eps outer

let area ?signed { outer; holes } =
  let outside = Path2.area ?signed outer
  and inside = List.fold_left (fun sum h -> Path2.area ?signed h +. sum) 0. holes in
  match signed with
  | Some true -> outside +. inside
  | _ -> outside -. inside

let map f { outer; holes } = { outer = f outer; holes = List.map f holes }

let offset ?fn ?fs ?fa ?check_valid ?mode d =
  map (Offset.offset ?fn ?fs ?fa ~closed:true ?check_valid ?mode d)

let translate p = map (Path2.translate p)
let xtrans x = map (Path2.xtrans x)
let ytrans y = map (Path2.ytrans y)
let rotate ?about r = map (Path2.rotate ?about r)
let[@inline] zrot ?about r t = rotate ?about r t
let scale s = map (Path2.scale s)
let xscale x = map (Path2.xscale x)
let yscale y = map (Path2.yscale y)
let mirror ax = map (Path2.mirror ax)
let affine m = map (Path2.affine m)
let outer t = t.outer
let holes t = t.holes
