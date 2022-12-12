open Util
open V
module R2 = Rounding.Make (V2) (Arc2)

module Cap = struct
  type offset =
    { d : float
    ; z : float
    }

  type offset_mode =
    | Delta
    | Chamfer
    | Radius of
        { fn : int option
        ; fs : float option
        ; fa : float option
        }

  type offsets = Offsets : offset list -> offsets

  type holes =
    [ `Same
    | `Flip
    | `Spec of offsets
    | `Mix of [ `Same | `Flip | `Spec of offsets ] list
    ]

  type poly =
    { outer : offsets
    ; holes : holes
    ; mode : offset_mode
    }

  type poly_spec =
    [ `Empty
    | `Flat
    | `Round of poly
    ]

  type path_spec =
    [ `Empty
    | `Flat
    | `Round of offsets
    ]

  let poly_to_path_spec = function
    | `Flat -> `Flat
    | `Empty -> `Empty
    | `Round { outer; _ } -> `Round outer

  type caps =
    { top : poly_spec
    ; bot : poly_spec
    }

  type loop = Loop

  type 'c t =
    | Looped : loop t
    | Caps : caps -> caps t

  let delta_mode = Delta
  let chamfer_mode = Chamfer
  let radius_mode ?fn ?fs ?fa () = Radius { fn; fs; fa }
  let round ?(mode = Delta) ?(holes = `Flip) outer = `Round { outer; holes; mode }
  let looped = Looped
  let capped ~top ~bot = Caps { top; bot }
  let flat_caps = Caps { top = `Flat; bot = `Flat }
  let open_caps = Caps { top = `Empty; bot = `Empty }
  let quantize = Math.quant ~q:(1. /. 1024.)

  let radius_of_roundover = function
    | `Radius r -> r
    | `Cut c -> c /. (Float.sqrt 2. -. 1.)

  let chamf ?(angle = Float.pi /. 4.) ?cut ?width ?height () =
    let a = Float.(min (pi /. 2.) (abs angle)) in
    let width, height =
      match width, height, cut with
      | _, _, Some c -> c /. Float.cos a, c /. Float.sin a
      | Some w, Some h, None -> w, h
      | Some w, None, None -> w, w /. Float.tan a
      | None, Some h, None -> h *. Float.tan a, h
      | None, None, None ->
        invalid_arg "At least one of cut, width, or height must be specified for chamfer."
    in
    Offsets [ { d = -.width; z = Float.abs height } ]

  let circ ?(fn = 16) roundover =
    let radius = radius_of_roundover roundover in
    let abs_radius = Float.abs radius
    and step = Float.pi /. 2. /. Float.of_int (Int.max 3 fn) in
    let f i (acc, last_z) =
      let i = Float.of_int (i + 1) in
      let z = quantize (abs_radius *. Float.sin (i *. step)) in
      if Math.approx last_z z
      then acc, last_z
      else { d = quantize (radius *. (Float.cos (i *. step) -. 1.)); z } :: acc, z
    in
    Offsets (List.rev @@ fst @@ Util.fold_init (fn + 1) f ([], Float.min_float))

  let tear ?(fn = 16) roundover =
    let radius = radius_of_roundover roundover in
    let abs_radius = Float.abs radius
    and step = Float.pi /. 4. /. Float.of_int (Int.max 3 (fn - 1)) in
    let f i (acc, last_z) =
      if i < fn
      then (
        let i = Float.of_int (i + 1) in
        let z = quantize (abs_radius *. Float.sin (i *. step)) in
        if Math.approx last_z z
        then acc, last_z
        else { d = quantize (radius *. (Float.cos (i *. step) -. 1.)); z } :: acc, z )
      else
        ( { d = -2. *. radius *. (1. -. (Float.sqrt 2. /. 2.)); z = abs_radius } :: acc
        , abs_radius )
    in
    Offsets (List.rev @@ fst @@ Util.fold_init (fn + 1) f ([], Float.min_float))

  let bez ?(curv = 0.5) ?(fn = 16) spec =
    let joint =
      match spec with
      | `Joint j -> j
      | `Cut c -> 16. *. c /. Float.sqrt 2. /. (1. +. (4. *. curv))
    in
    let f (acc, last_z) { x = d; y = z } =
      let z = quantize z in
      if Math.approx last_z z then acc, last_z else { d = quantize d; z } :: acc, z
    in
    Offsets
      ( R2.bez_corner
          ~fn:(Int.max 1 fn + 2)
          ~curv
          ~d:joint
          V2.zero
          (v2 0. (Float.abs joint))
          (v2 (-.joint) (Float.abs joint))
      |> List.tl
      |> List.fold_left f ([], Float.min_float)
      |> fst
      |> List.rev )

  let offsets offsets =
    let f (last_z, acc) { d; z } =
      let z = quantize @@ Float.abs z in
      if Float.compare z last_z <> 1
      then invalid_arg "Z offsets must increase monotonically."
      else z, { d = quantize d *. -1.; z } :: acc
    in
    let _, offsets = List.fold_left f (Float.min_float, []) offsets in
    Offsets (List.rev offsets)

  let unwrap_offsets (Offsets l) = l
  let flip_d (Offsets l) = Offsets (List.map (fun { d; z } -> { d = d *. -1.; z }) l)

  let unpack_poly_spec ?(n_holes = 0) spec =
    let hole_spec outer_offsets = function
      | `Same -> fun _ -> `Round outer_offsets
      | `Flip ->
        let flipped = flip_d outer_offsets in
        fun _ -> `Round flipped
      | `Spec offs -> fun _ -> `Round offs
      | `Mix specs ->
        let specs = Array.of_list specs in
        if Array.length specs = n_holes
        then
          fun i ->
          match Array.get specs i with
          | `Same -> `Round outer_offsets
          | `Flip -> `Round (flip_d outer_offsets)
          | `Spec offs -> `Round offs
        else invalid_arg "Mixed hole specs must match the number of holes."
    in
    match spec with
    | `Flat -> `Flat, (fun _ -> `Flat), None
    | `Empty -> `Empty, (fun _ -> `Empty), None
    | `Round { outer; holes; mode } -> `Round outer, hole_spec outer holes, Some mode
end

open Cap

let cap ?check_valid ?len ~flip ~close ~offset_mode ~m ~offsets shape =
  let len =
    match len with
    | Some l -> l
    | None -> List.length shape
  and z_dir = if flip then -1. else 1.
  and lift ~z m = List.map (fun p -> Affine3.transform m @@ V2.to_v3 ~z p) in
  let f (pts, faces, start_idx, last_shape, last_len, last_d) { d; z } =
    let mode, fn, fs, fa =
      match offset_mode with
      | Radius { fn; fs; fa } -> `Radius, fn, fs, fa
      | Delta -> `Delta, None, None, None
      | Chamfer -> `Chamfer, None, None, None
    and z = z *. z_dir in
    let n, ps, fs =
      Offset.offset_with_faces
        ?check_valid
        ?fn
        ?fs
        ?fa
        ~flip_faces:flip
        ~start_idx
        ~mode
        (d -. last_d)
        last_shape
    in
    lift ~z m ps :: pts, fs :: faces, start_idx + last_len, ps, n, d
  in
  let points, faces, idx, last_shape, last_len, _ =
    List.fold_left f ([ lift ~z:0. m shape ], [], 0, shape, len, 0.) offsets
  in
  let faces =
    if close
    then (
      let close =
        if flip then fun i _ -> last_len + idx - i - 1 else fun i _ -> i + idx
      in
      List.mapi close last_shape :: List.concat faces )
    else List.concat faces
  in
  List.hd points, Mesh0.make ~points:List.(concat (rev points)) ~faces

type poly_morph =
  | Fixed of Poly2.t
  | Morph of
      { outer_map : Skin.mapping
      ; hole_map : [ `Same | `Flat of Skin.mapping | `Mix of Skin.mapping list ]
      ; refine : int option
      ; ez : (V2.t * V2.t) option
      ; a : Poly2.t
      ; b : Poly2.t
      }

let sweep'
    (type c)
    ?style
    ?check_valid
    ?(merge = true)
    ?(winding = `CCW)
    ?(caps : c Cap.t option)
    ?progress
    ~transforms
    shape
  =
  let outer_wind, hole_wind =
    match winding with
    | `CCW -> `CCW, `CW
    | `CW -> `CW, `CCW
    | `NoCheck -> `NoCheck, `NoCheck
  and outer, holes, refine, ez =
    match shape with
    | Fixed Poly2.{ outer; holes } ->
      `Fixed outer, List.map (fun h -> `Fixed h) holes, None, None
    | Morph
        { outer_map
        ; hole_map
        ; refine
        ; ez
        ; a = { outer = oa; holes = ha }
        ; b = { outer = ob; holes = hb }
        } ->
      let wrap m a b =
        let same =
          try List.for_all2 V2.approx a b with
          | Invalid_argument _ -> false
        in
        if same then `Fixed a else `Morph (m, a, b)
      in
      let outer = wrap outer_map oa ob
      and holes =
        let map =
          match hole_map with
          | `Flat m -> List.map2 (wrap m)
          | `Same -> List.map2 (wrap outer_map)
          | `Mix ms ->
            fun ha hb ->
              let zipped =
                try List.map2 (fun m h -> m, h) ms ha with
                | Invalid_argument _ ->
                  invalid_arg "`Mix hole morph mappings must match number of holes."
              in
              List.map2 (fun (m, a) b -> wrap m a b) zipped hb
        in
        try map ha hb with
        | Invalid_argument _ ->
          invalid_arg "Polygon pair to be morphed must have same number of holes."
      and refine = Option.bind refine (fun n -> if n > 1 then Some n else None) in
      outer, holes, refine, ez
  in
  let morph
      ?(sealed = true)
      ?(top_mode = radius_mode ())
      ?(bot_mode = radius_mode ())
      ~winding
      ~top
      ~bot
      shape
    =
    let n_trans = List.length transforms in
    let get_shape, bot_shape, len_bot, top_shape, len_top =
      match shape with
      | `Fixed shape ->
        let shape = Mesh0.enforce_winding winding shape in
        let len = List.length shape in
        Fun.const shape, shape, len, shape, len
      | `Morph (mapping, bot, top) ->
        if n_trans < 2 then invalid_arg "More than one transform is required for morphs.";
        let a = Mesh0.enforce_winding winding bot
        and b = Mesh0.enforce_winding winding top in
        let len_a = List.length a
        and len_b = List.length b
        and refine = Option.value ~default:1 refine in
        let resample =
          let n = Int.max len_a len_b * refine
          and samp =
            match mapping with
            | `Direct samp | `Reindex samp -> samp
            | _ -> `BySeg
          in
          let f len path =
            if len <> n
            then Path2.subdivide ~closed:true ~freq:(`N (n, samp)) path
            else path
          in
          fun (a, b) -> f len_a a, f len_b b
        in
        let a, b =
          match mapping with
          | `Direct _ | `Reindex _ ->
            let a, b = resample (a, b) in
            if Skin.is_direct mapping then a, b else a, Path2.reindex_polygon a b
          | `Distance -> resample @@ Path2.distance_match a b
          | `FastDistance -> resample @@ Path2.aligned_distance_match a b
          | `Tangent -> resample @@ Path2.tangent_match a b
        in
        let prog =
          match progress with
          | Some (`RelDist prog) ->
            let prog = Array.of_list prog in
            if Array.length prog <> n_trans
            then
              invalid_arg
              @@ Printf.sprintf
                   "`RelDist progress (%i) must be the same length as transforms (%i)"
                   (Array.length prog)
                   n_trans;
            Array.get prog
          | Some `AutoDist | None ->
            let pts = List.map (fun m -> Affine3.transform m V3.zero) transforms in
            let dists = Path3.cummulative_length pts |> Array.of_list in
            for i = 0 to n_trans - 1 do
              dists.(i) <- dists.(i) /. dists.(n_trans - 1)
            done;
            Array.get dists
          | Some `AutoPoints ->
            let n = List.length transforms in
            (* less than 2 transforms are not allowed for morphs *)
            let step = 1. /. Float.of_int (n - 1) in
            let prog = Array.init n (fun i -> Float.of_int i *. step) in
            Array.get prog
        in
        let transition =
          let ez =
            Util.value_map_opt ~default:Fun.id (fun (p1, p2) -> Easing.make p1 p2) ez
          in
          fun i -> List.map2 (fun a b -> V2.lerp a b (ez @@ prog i)) a b
        and bot, len_bot, top, len_top =
          (* use the original shapes for caps if there has been point
                 duplication as repeated points will cause offset to fail (rounded caps) *)
          if Skin.is_duplicator mapping
          then
            if refine > 1
            then (
              let sd = Path2.subdivide ~closed:true ~freq:(`Refine (refine, `BySeg)) in
              sd bot, len_a * refine, sd top, len_b * refine )
            else bot, len_a, top, len_b
          else (
            let len = Int.max len_a len_b * refine in
            a, len, b, len )
        in
        transition, bot, len_bot, top, len_top
    in
    let unpack_cap = function
      | `Flat -> sealed, []
      | `Empty -> false, []
      | `Round (Offsets offsets) -> sealed, offsets
    in
    let close_top, top_offsets = unpack_cap top
    and close_bot, bot_offsets = unpack_cap bot in
    let cap = function
      | `Top ->
        cap
          ?check_valid
          ~len:len_top
          ~flip:false
          ~close:close_top
          ~offset_mode:top_mode
          ~offsets:top_offsets
      | `Bot ->
        cap
          ?check_valid
          ~len:len_bot
          ~flip:true
          ~close:close_bot
          ~offset_mode:bot_mode
          ~offsets:bot_offsets
    in
    match transforms with
    | [] ->
      let bot_lid, bot = cap `Bot ~m:Affine3.id bot_shape
      and top_lid, top = cap `Top ~m:Affine3.id top_shape in
      bot_lid, top_lid, Mesh0.join [ bot; top ]
    | [ m ] ->
      let bot_lid, bot = cap `Bot ~m bot_shape
      and top_lid, top = cap `Top ~m top_shape in
      bot_lid, top_lid, Mesh0.join [ bot; top ]
    | hd :: tl ->
      let _, mid, last_transform =
        let f (i, acc, _last) m =
          ( i + 1
          , List.map (fun { x; y } -> Affine3.transform m { x; y; z = 0. }) (get_shape i)
            :: acc
          , m )
        in
        List.fold_left f (f (0, [], hd) hd) tl
      in
      let mid = Mesh0.of_rows ?style ~endcaps:`None (List.rev mid)
      and bot_lid, bot = cap `Bot ~m:hd bot_shape
      and top_lid, top = cap `Top ~m:last_transform top_shape in
      bot_lid, top_lid, Mesh0.join [ bot; mid; top ]
  in
  let no_holes top bot =
    let top = poly_to_path_spec top
    and bot = poly_to_path_spec bot in
    let _, _, poly = morph ~winding ~top ~bot outer in
    poly
  and with_holes top bot holes =
    let n_holes = List.length holes in
    let top, top_holes, top_mode = unpack_poly_spec ~n_holes top
    and bot, bot_holes, bot_mode = unpack_poly_spec ~n_holes bot in
    let _, tunnel_bots, tunnel_tops, tunnels =
      let f (i, bots, tops, tuns) hole =
        let bot, top, tunnel =
          morph
            ~winding:hole_wind
            ~sealed:false
            ?top_mode
            ?bot_mode
            ~top:(top_holes i)
            ~bot:(bot_holes i)
            hole
        in
        i + 1, bot :: bots, top :: tops, tunnel :: tuns
      in
      List.fold_left f (0, [], [], []) holes
    in
    let validate =
      match check_valid with
      | Some `No -> false
      | _ -> true
    and outer_bot, outer_top, outer =
      morph ~winding:outer_wind ~sealed:false ?top_mode ?bot_mode ~top ~bot outer
    in
    let bot_lid =
      Mesh0.of_poly3 ~rev:true (Poly3.make ~validate ~holes:tunnel_bots outer_bot)
    and top_lid = Mesh0.of_poly3 (Poly3.make ~validate ~holes:tunnel_tops outer_top) in
    Mesh0.join (bot_lid :: top_lid :: outer :: tunnels)
  in
  let mesh =
    match caps, holes with
    | Some (Caps { top; bot }), [] -> no_holes top bot
    | None, [] -> no_holes `Flat `Flat
    | Some Looped, _ ->
      ( match shape with
      | Fixed Poly2.{ outer; holes } ->
        let f ~winding path =
          let path = Mesh0.enforce_winding winding path in
          List.map (fun m -> Path2.affine3 m path) transforms
          |> Mesh0.of_rows ?style ~endcaps:`Loop
        in
        Mesh0.join (f ~winding:outer_wind outer :: List.map (f ~winding:hole_wind) holes)
      | Morph _ -> invalid_arg "Cannot loop a morph (see skin)." )
    | Some (Caps { top; bot }), holes -> with_holes top bot holes
    | None, holes -> with_holes `Flat `Flat holes
  in
  if merge then Mesh0.merge_points mesh else mesh

let sweep ?style ?check_valid ?merge ?winding ?caps ~transforms shape =
  sweep' ?style ?check_valid ?merge ?winding ?caps ~transforms (Fixed shape)

let morphing_sweep
    ?(style = `MinEdge)
    ?check_valid
    ?merge
    ?winding
    ?(caps = flat_caps)
    ?(outer_map = `Direct `ByLen)
    ?(hole_map = `Same)
    ?refine
    ?ez
    ~transforms
    a
    b
  =
  sweep'
    ?check_valid
    ~style
    ?merge
    ?winding
    ~caps
    ~progress:`AutoDist
    ~transforms
    (Morph { outer_map; hole_map; refine; ez; a; b })

let linear'
    ?style
    ?check_valid
    ?merge
    ?winding
    ?fa
    ?slices
    ?scale_ez
    ?twist_ez
    ?scale
    ?(twist = 0.)
    ?(center = false)
    ?(caps = flat_caps)
    ~height
    shape
  =
  let cap_height = function
    | `Flat | `Empty -> 0.
    | `Round { outer = Offsets l; _ } -> List.fold_left (fun _ { z; _ } -> z) 0. l
  in
  let (Caps { bot; top }) = caps in
  let bot_height = cap_height bot
  and top_height = cap_height top in
  let transforms =
    let h = height -. bot_height -. top_height
    and z = if center then height /. -2. else bot_height in
    if h < 0.
    then [ Affine3.ztrans z ]
    else (
      let slices =
        match slices, twist with
        | Some s, tw when Float.(abs tw /. (2. *. pi) < 1.) -> s
        | fn, tw -> helical_slices ?fa ?fn tw
      in
      let s = height /. Float.of_int slices
      and twist = if Float.abs twist > 0. then Some twist else None in
      List.init (slices + 1) (fun i -> v3 0. 0. ((Float.of_int i *. s) +. z))
      |> Path3.to_transforms ?scale_ez ?twist_ez ?scale ?twist )
  in
  sweep' ?style ?check_valid ?merge ?winding ~caps ~progress:`AutoPoints ~transforms shape

let extrude
    ?style
    ?check_valid
    ?merge
    ?winding
    ?fa
    ?slices
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?center
    ?caps
    ~height
    shape
  =
  linear'
    ?style
    ?check_valid
    ?merge
    ?winding
    ?fa
    ?slices
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?center
    ?caps
    ~height
    (Fixed shape)

let morph
    ?(style = `MinEdge)
    ?check_valid
    ?merge
    ?winding
    ?fa
    ?(slices = 10)
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?center
    ?caps
    ?(outer_map = `Direct `ByLen)
    ?(hole_map = `Same)
    ?refine
    ?ez
    ~height
    a
    b
  =
  linear'
    ~style
    ?check_valid
    ?merge
    ?winding
    ?fa
    ~slices
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?center
    ?caps
    ~height
    (Morph { outer_map; hole_map; refine; ez; a; b })

let helix'
    ?style
    ?check_valid
    ?merge
    ?fn
    ?fa
    ?fs
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?(caps = flat_caps)
    ?(left = true)
    ~n_turns
    ~pitch
    ?r2
    r1
    shape
  =
  let transforms =
    Path3.helical_transforms
      ?fn
      ?fa
      ?fs
      ?scale_ez
      ?twist_ez
      ?scale
      ?twist
      ~n_turns
      ~pitch
      ?r2
      r1
  in
  sweep'
    ?style
    ?check_valid
    ?merge
    ~winding:(if left then `CCW else `CW)
    ~caps
    ~progress:`AutoPoints
    ~transforms
    shape

let helix_extrude
    ?style
    ?check_valid
    ?merge
    ?fn
    ?fa
    ?fs
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?caps
    ?left
    ~n_turns
    ~pitch
    ?r2
    r1
    shape
  =
  helix'
    ?style
    ?check_valid
    ?merge
    ?fn
    ?fa
    ?fs
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?caps
    ?left
    ~n_turns
    ~pitch
    ?r2
    r1
    (Fixed shape)

let helix_morph
    ?(style = `MinEdge)
    ?check_valid
    ?merge
    ?fn
    ?fa
    ?fs
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?caps
    ?(outer_map = `Direct `ByLen)
    ?(hole_map = `Same)
    ?refine
    ?ez
    ?left
    ~n_turns
    ~pitch
    ?r2
    r1
    a
    b
  =
  helix'
    ~style
    ?check_valid
    ?merge
    ?fn
    ?fa
    ?fs
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ?caps
    ?left
    ~n_turns
    ~pitch
    ?r2
    r1
    (Morph { outer_map; hole_map; refine; ez; a; b })

let path'
    ?style
    ?check_valid
    ?merge
    ?winding
    ?caps
    ?(euler = false)
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ~path
    shape
  =
  let mode = if euler then `Euler else `Auto in
  let transforms = Path3.to_transforms ~mode ?scale_ez ?twist_ez ?scale ?twist path in
  let progress =
    match shape with
    | Fixed _ -> None
    | Morph _ ->
      let dists = Path3.cummulative_length path in
      let f d = function
        | (Some t as total), acc -> total, (d /. t) :: acc
        | None, acc -> Some d, 1. :: acc
      in
      Some (`RelDist (snd @@ List.fold_right f dists (None, [])))
  in
  sweep' ?style ?check_valid ?merge ?winding ?caps ?progress ~transforms shape

let path_extrude
    ?style
    ?check_valid
    ?merge
    ?winding
    ?caps
    ?euler
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ~path
    shape
  =
  path'
    ?style
    ?check_valid
    ?merge
    ?winding
    ?caps
    ?euler
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ~path
    (Fixed shape)

let path_morph
    ?(style = `MinEdge)
    ?check_valid
    ?merge
    ?winding
    ?(caps = flat_caps)
    ?(outer_map = `Direct `ByLen)
    ?(hole_map = `Same)
    ?refine
    ?ez
    ?euler
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ~path
    a
    b
  =
  path'
    ~style
    ?check_valid
    ?merge
    ?winding
    ~caps
    ?euler
    ?scale_ez
    ?twist_ez
    ?scale
    ?twist
    ~path
    (Morph { outer_map; hole_map; refine; ez; a; b })
