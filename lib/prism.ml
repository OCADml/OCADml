open Util
module Bez = Bezier.Make (V3)

module Prism = struct
  type spec =
    { k : float
    ; k_bot : float option
    ; k_top : float option
    ; k_sides : [ `Flat of float | `Mix of float list ] option
    ; joint_bot : float * float
    ; joint_top : float * float
    ; joint_sides : [ `Flat of float * float | `Mix of (float * float) list ]
    }

  type holes =
    [ `Same
    | `Flip
    | `Spec of spec
    | `Mix of [ `Same | `Flip | `Spec of spec ] list
    ]

  let flip ({ joint_bot = b_in, b_down; joint_top = t_in, t_down; _ } as spec) =
    { spec with joint_bot = b_in *. -1., b_down; joint_top = t_in *. -1., t_down }

  let spec
      ?(k = 0.5)
      ?k_bot
      ?k_top
      ?k_sides
      ?(joint_bot = 0., 0.)
      ?(joint_top = 0., 0.)
      ?(joint_sides = `Flat (0., 0.))
      ()
    =
    { k; k_bot; k_top; k_sides; joint_bot; joint_top; joint_sides }
end

open Prism

type patch_edges =
  { left : Path3.t
  ; right : Path3.t
  ; top : Path3.t
  ; bot : Path3.t
  }

let degenerate_patch ?(fn = 16) ?(rev = false) bezpatch =
  let trans_bezpatch = Math.transpose bezpatch
  and n_rows, n_cols = Math.mat_dims bezpatch in
  let row_degen = Array.map (array_all_equal @@ V3.approx ~eps:0.) bezpatch
  and col_degen = Array.map (array_all_equal @@ V3.approx ~eps:0.) trans_bezpatch in
  let top_degen = row_degen.(0)
  and bot_degen = row_degen.(n_rows - 1)
  and left_degen = col_degen.(0)
  and right_degen = col_degen.(n_cols - 1)
  and all_rows_degen = Array.for_all Fun.id row_degen
  and all_cols_degen = Array.for_all Fun.id col_degen
  and top_degen_case ~rev bp =
    let row_max =
      let full_degen =
        let row_degen = Array.map (array_all_equal @@ V3.approx ~eps:0.) bp in
        let r = Float.(to_int @@ ceil ((of_int n_rows /. 2.) -. 1.)) in
        n_rows >= 4 && Array.for_all Fun.id (Array.sub row_degen 1 r)
      in
      let f = if full_degen then Fun.id else fun i -> if i <= fn / 2 then 2 * i else fn in
      Array.init (fn + 1) f
    and bezpatch =
      Array.map (fun row -> Bez.(curve' @@ make' row)) bp |> Math.transpose
    in
    let pts =
      [ bezpatch.(0).(0) ]
      :: List.init fn (fun i ->
             let fn = row_max.(i) + 2 in
             Bez.(curve ~fn @@ make' bezpatch.(i + 1)) )
    in
    let left = List.map List.hd pts
    and right = List.map last_element pts
    and mesh = Mesh0.of_ragged ~rev:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = last_element pts }
  in
  match all_rows_degen, all_cols_degen, top_degen, bot_degen, left_degen, right_degen with
  | true, true, _, _, _, _ ->
    let p = [ bezpatch.(0).(0) ] in
    Mesh0.empty, { left = p; right = p; top = p; bot = p }
  | true, false, _, _, _, _ ->
    let col = Bez.(curve ~fn @@ make' trans_bezpatch.(0)) in
    let bot = [ last_element col ] in
    Mesh0.empty, { left = col; right = col; top = [ List.hd col ]; bot }
  | false, true, _, _, _, _ ->
    let row = Bez.(curve ~fn @@ make' bezpatch.(0)) in
    let right = [ last_element row ] in
    Mesh0.empty, { left = [ List.hd row ]; right; top = row; bot = row }
  | false, false, false, false, false, false ->
    let pts = Bez.(patch_curve ~fn @@ patch' bezpatch) in
    let left = List.map List.hd pts
    and right = List.map last_element pts
    and mesh = Mesh0.of_ragged ~rev:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = last_element pts }
  | false, false, true, true, _, _ ->
    let row_count =
      let steps = ((fn - 3) / 2) + 1 in
      let even = fn mod 2 = 0 in
      let mid_start = if even then steps + 1 else steps in
      let a = Array.make ((steps * 2) + if even then 1 else 0) 0 in
      if even then a.(steps) <- fn + 1;
      for i = 0 to steps - 1 do
        a.(i) <- 3 + (i * 2);
        a.(mid_start + i) <- 3 + ((steps - 1 - i) * 2)
      done;
      a
    in
    let bezpatch =
      Array.map (fun row -> Bez.(curve' @@ make' row)) trans_bezpatch |> Math.transpose
    in
    let pts =
      [ bezpatch.(0).(0) ]
      :: fold_init
           (fn - 1)
           (fun j acc ->
             let i = fn - 2 - j in
             Bez.(curve ~fn:row_count.(i) @@ make' bezpatch.(i + 1)) :: acc )
           [ [ bezpatch.(0).(Array.length bezpatch - 1) ] ]
    in
    let left = List.map List.hd pts
    and right = List.map last_element pts
    and mesh = Mesh0.of_ragged ~rev:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = last_element pts }
  | false, false, true, false, false, false -> top_degen_case ~rev trans_bezpatch
  | false, false, false, true, false, false ->
    let poly, { left; right; top; bot } =
      top_degen_case
        ~rev:(not rev)
        (Array.init n_rows (fun i -> bezpatch.(n_rows - 1 - i)))
    in
    poly, { left; right; top = bot; bot = top }
  | _ ->
    let poly, { left; right; top; bot } =
      top_degen_case ~rev:(not rev) (Math.transpose bezpatch)
    in
    poly, { left = top; right = bot; top = left; bot = right }

let compute_patches ~r_top:(rt_in, rt_down) ~r_sides ~k_top ~k_sides ~concave top bot =
  let len = Array.length top
  and plane = Plane.make top.(0) top.(1) top.(2)
  and rt_in_sign = if rt_in >= 0. then 1. else -1.
  and abs_rt_in = Float.abs rt_in in
  let f i =
    let rside_prev, rside_next = r_sides.(i)
    and concave_sign = rt_in_sign *. if concave.(i) then -1. else 1.
    and prev = V3.sub top.(index_wrap ~len (i - 1)) top.(i)
    and next = V3.sub top.(index_wrap ~len (i + 1)) top.(i)
    and edge = V3.sub bot.(i) top.(i) in
    let prev_offset =
      let s = V3.(smul (normalize prev) (rside_prev /. Float.sin (angle prev edge))) in
      V3.add top.(i) s
    and next_offset =
      let s = V3.(smul (normalize next) (rside_next /. Float.sin (angle next edge))) in
      V3.add top.(i) s
    and down =
      let edge_angle =
        let edge = V3.{ a = bot.(i); b = top.(i) } in
        rt_down /. Float.sin (Float.abs (Plane.line_angle plane edge))
      in
      V3.(smul (normalize edge) edge_angle)
    and fill_row p1 p2 p3 =
      [| p1; V3.lerp p2 p1 k_sides.(i); p2; V3.lerp p2 p3 k_sides.(i); p3 |]
    in
    let row0 =
      let in_prev =
        let a = V3.(sub next (smul prev (dot next prev /. dot prev prev))) in
        V3.(smul (normalize a) concave_sign)
      and in_next =
        let a = V3.(sub prev (smul next (dot prev next /. dot next next))) in
        V3.(smul (normalize a) concave_sign)
      and far_corner =
        let num =
          let s = concave_sign *. abs_rt_in in
          V3.(smul (normalize (add (normalize prev) (normalize next))) s)
        in
        V3.(add top.(i) @@ sdiv num (Float.sin (V3.angle prev next /. 2.)))
      in
      let prev_corner = V3.(add prev_offset (smul in_prev abs_rt_in))
      and next_corner = V3.(add next_offset (smul in_next abs_rt_in)) in
      if concave_sign < 0.
      then fill_row prev_corner far_corner next_corner
      else (
        let fc2 = V2.of_v3 far_corner in
        let prev_degen =
          let po2 = V2.of_v3 prev_offset in
          V2.(
            line_intersection
              ~bounds1:(true, false)
              ~bounds2:(true, false)
              { a = fc2; b = add fc2 (of_v3 prev) }
              { a = po2; b = add po2 (of_v3 in_prev) })
          |> Option.is_none
        and next_degen =
          let no2 = V2.of_v3 next_offset in
          V2.(
            line_intersection
              ~bounds1:(true, false)
              ~bounds2:(true, false)
              { a = fc2; b = add fc2 (of_v3 next) }
              { a = no2; b = add no2 (of_v3 in_next) })
          |> Option.is_none
        in
        fill_row
          (if prev_degen then far_corner else prev_corner)
          far_corner
          (if next_degen then far_corner else next_corner) )
    and row2 = fill_row prev_offset top.(i) next_offset
    and row4 =
      V3.(fill_row (add prev_offset down) (add top.(i) down) (add next_offset down))
    in
    let row1 = Array.map2 (fun a b -> V3.lerp b a k_top) row0 row2
    and row3 = Array.map2 (fun a b -> V3.lerp a b k_top) row2 row4 in
    [| row0; row1; row2; row3; row4 |]
  in
  Array.init len f

let curvature_continuity ~len ~bot_patch:bp ~top_patch:tp =
  let check line =
    if not (Path3.is_collinear line) then failwith "Curvature continuity failure."
  and w = index_wrap ~len in
  let horiz p i j =
    [ p.(i).(j).(2)
    ; p.(i).(j).(3)
    ; p.(i).(j).(4)
    ; p.(w (i + 1)).(j).(0)
    ; p.(w (i + 1)).(j).(1)
    ; p.(w (i + 1)).(j).(2)
    ]
  in
  ignore w;
  for i = 0 to len - 1 do
    for j = 0 to 4 do
      (* verify vertical edges *)
      check
        [ tp.(i).(2).(j)
        ; tp.(i).(3).(j)
        ; tp.(i).(4).(j)
        ; bp.(i).(2).(j)
        ; bp.(i).(3).(j)
        ; bp.(i).(4).(j)
        ];
      (* verify horizontal edges *)
      check (horiz tp i j);
      check (horiz bp i j)
    done
  done

let bad_patches ~len ~bot_patch:bp ~top_patch:tp bot top =
  let open V3 in
  let w = index_wrap ~len in
  let vert_bad i acc =
    if distance top.(i) tp.(i).(4).(2) +. distance bot.(i) bp.(i).(4).(2)
       > distance bot.(i) top.(i)
    then i :: acc
    else acc
  and patch_bad p i acc =
    if distance p.(i).(2).(4) p.(i).(2).(2)
       +. distance p.(w (i + 1)).(2).(0) p.(w (i + 1)).(2).(2)
       > distance p.(i).(2).(2) p.(w (i + 1)).(2).(2)
    then (i, (i + 1) mod len) :: acc
    else acc
  and patch_in_bad p i acc =
    if distance p.(i).(0).(2) p.(i).(0).(4)
       +. distance p.(w (i + 1)).(0).(0) p.(w (i + 1)).(0).(2)
       > distance p.(i).(0).(2) p.(w (i + 1)).(0).(2)
    then (i, (i + 1) mod len) :: acc
    else acc
  and show (a, b) = Printf.sprintf "(%i, %i)" a b in
  let check ~show ~msg f =
    match fold_init len f [] with
    | []  -> ()
    | bad ->
      let f acc a = Printf.sprintf "%s; %s" acc (show a) in
      failwith @@ List.fold_left f (Printf.sprintf "%s: [" msg) bad ^ "]"
  in
  check
    ~show:Int.to_string
    ~msg:"Top and bottom joint lengths are too large; they interfere with eachother"
    vert_bad;
  check ~show ~msg:"Joint lengths too large at top edges" (patch_bad tp);
  check ~show ~msg:"Joint lengths too large at bottom edges" (patch_bad bp);
  check ~show ~msg:"Joint length too large on the top face at edges" (patch_in_bad tp);
  check ~show ~msg:"Joint length too large on the bottom face at edges" (patch_in_bad bp)

let roundover_interference label face =
  let proj = Path3.(project (to_plane face) face) in
  if not (Path2.is_simple proj)
  then (
    let msg =
      Printf.sprintf
        "Roundovers interfere with eachother on the %s face: either the shape is self \
         intersecting or the %s joint length is too large."
        label
        label
    in
    failwith msg )

let prism'
    ?(debug = false)
    ?(fn = 16)
    ~spec:{ k; k_bot; k_top; k_sides; joint_bot; joint_top; joint_sides }
    bottom
    top
  =
  let bottom = Array.of_list bottom
  and top = Array.of_list top in
  let len = Array.length bottom in
  let wrap = index_wrap ~len
  and unpack_sides ~name = function
    | `Flat s -> Array.make len s
    | `Mix ss ->
      let ss = Array.of_list ss in
      if Array.length ss = len
      then ss
      else
        invalid_arg
        @@ Printf.sprintf "`Mix %s must be the same length as the top/bottom polys." name
  in
  if len <> Array.length top
  then invalid_arg "Top and bottom shapes must have the same length.";
  let k_bot = Option.value ~default:k k_bot
  and k_top = Option.value ~default:k k_top
  and k_sides = unpack_sides ~name:"k_sides" (Option.value ~default:(`Flat k) k_sides)
  and r_sides = unpack_sides ~name:"joint_sides" joint_sides in
  let bot_proj =
    let plane = Plane.make bottom.(0) bottom.(1) bottom.(2) in
    Array.map (Plane.project plane) bottom
  in
  let bottom_sign = APath2.clockwise_sign bot_proj in
  let concave =
    let f i =
      let line = V2.{ a = bot_proj.(wrap (i - 1)); b = bot_proj.(i) } in
      bottom_sign *. V2.left_of_line ~line bot_proj.(wrap (i + 1)) > 0.
    in
    Array.init len f
  in
  let top_patch =
    compute_patches ~r_top:joint_top ~r_sides ~k_top ~k_sides ~concave top bottom
  and bot_patch =
    compute_patches ~r_top:joint_bot ~r_sides ~k_top:k_bot ~k_sides ~concave bottom top
  in
  if not debug then bad_patches ~len ~bot_patch ~top_patch bottom top;
  let top_samples, top_edges =
    unzip_array @@ Array.map (degenerate_patch ~fn ~rev:false) top_patch
  and bot_samples, bot_edges =
    unzip_array @@ Array.map (degenerate_patch ~fn ~rev:true) bot_patch
  in
  let top_face = fold_init len (fun i acc -> List.rev_append top_edges.(i).top acc) []
  and bot_face = fold_init len (fun i acc -> List.rev_append bot_edges.(i).top acc) [] in
  let edge_points =
    let f i acc =
      let top_edge = [ top_edges.(i).right; top_edges.(wrap (i + 1)).left ]
      and bot_edge = [ bot_edges.(wrap (i + 1)).left; bot_edges.(i).right ]
      and vert_edge = [ bot_edges.(i).bot; top_edges.(i).bot ] in
      vert_edge :: bot_edge :: top_edge :: acc
    in
    fold_init len f []
  and patches =
    List.init len (fun i ->
        [ bot_patch.(i).(4).(4)
        ; bot_patch.(wrap (i + 1)).(4).(0)
        ; top_patch.(wrap (i + 1)).(4).(0)
        ; top_patch.(i).(4).(4)
        ] )
  in
  if not debug then curvature_continuity ~len ~bot_patch ~top_patch;
  if not debug then roundover_interference "top" top_face;
  if not debug then roundover_interference "bottom" bot_face;
  let mesh =
    List.fold_left
      (fun acc pts -> Mesh0.of_ragged pts :: acc)
      [ Mesh0.of_polygons patches ]
      edge_points
    |> fold_init len (fun i acc -> bot_samples.(i) :: acc)
    |> fold_init len (fun i acc -> top_samples.(i) :: acc)
    |> Mesh0.join
  in
  bot_face, top_face, mesh

let prism ?debug ?fn ?(holes = `Flip) ?(outer = spec ()) bottom top =
  let n_holes = List.length bottom.Poly3.holes
  and validate = Option.map not debug in
  if List.length top.Poly3.holes <> n_holes
  then invalid_arg "Polys must have same number of holes.";
  let hole_spec =
    match holes with
    | `Same      -> fun _ -> outer
    | `Flip      ->
      let flipped = flip outer in
      fun _ -> flipped
    | `Spec spec -> fun _ -> spec
    | `Mix specs ->
      let specs = Array.of_list specs in
      if Array.length specs = n_holes
      then
        fun i ->
        match Array.get specs i with
        | `Same      -> outer
        | `Flip      -> flip outer
        | `Spec spec -> spec
      else invalid_arg "Mixed hole specs must match the number of holes."
  in
  let _, tunnel_bots, tunnel_tops, tunnels =
    let f (i, bots, tops, tuns) bot_hole top_hole =
      let bot, top, tunnel = prism' ?debug ?fn ~spec:(hole_spec i) bot_hole top_hole in
      i + 1, bot :: bots, top :: tops, tunnel :: tuns
    in
    List.fold_left2 f (0, [], [], []) bottom.holes top.holes
  in
  let bot', top', outer = prism' ?debug ?fn ~spec:outer bottom.outer top.outer in
  let bot_lid = Mesh0.of_poly3 (Poly3.make ?validate ~holes:tunnel_bots bot')
  and top_lid = Mesh0.of_poly3 ~rev:true (Poly3.make ?validate ~holes:tunnel_tops top') in
  Mesh0.join (bot_lid :: top_lid :: outer :: tunnels)

let linear_prism ?debug ?fn ?holes ?outer ?(center = false) ~height bottom =
  let bottom =
    let b = Poly3.of_poly2 bottom in
    if center then Poly3.translate { x = 0.; y = 0.; z = height /. -2. } b else b
  in
  let top = Poly3.translate { x = 0.; y = 0.; z = height } bottom in
  prism ?debug ?fn ?holes ?outer bottom top
