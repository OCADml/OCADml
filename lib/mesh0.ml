module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end)

module IntSet = Set.Make (Int)

type tri = int * int * int

type t =
  { points : V3.t array
  ; faces : tri list
  }

type endcaps =
  [ `Loop
  | `Both
  | `None
  | `Top
  | `Bot
  ]

type style =
  [ `Default
  | `Alt
  | `MinEdge
  | `Quincunx
  | `Convex
  | `Concave
  ]

let empty = { points = [||]; faces = [] }
let size t = Array.length t.points
let points t = Array.to_list t.points
let e t i = t.points.(i)
let faces t = t.faces
let make ~points ~faces = { points = Array.of_list points; faces }

let of_polyhedron ?eps points faces =
  let pts = Array.of_list points in
  let f faces = function
    | [ a; b; c ] -> (a, b, c) :: faces
    | face ->
      let face = Array.of_list face in
      let poly = Array.init (Array.length face) (fun i -> pts.(face.(i))) in
      let norm = APath3.normal poly in
      let proj = Plane.(project (of_normal ~point:pts.(face.(0)) norm)) in
      Triangulate.triangulate ?eps (Array.map proj poly)
      |> List.map (fun (a, b, c) -> face.(a), face.(b), face.(c))
      |> Fun.flip List.rev_append faces
  in
  { points = pts; faces = List.fold_left f [] faces }

let prune_rows ?(min_dist = 0.05) = function
  | [] -> [], []
  | [ _ ] as rows -> [], rows
  | hd :: rows ->
    let f (drop, keep, i, plane) row =
      let valid = List.for_all (Plane.is_point_above ~eps:min_dist plane) row in
      if valid
      then drop, row :: keep, i + 1, Path3.to_plane row
      else i :: drop, keep, i + 1, plane
    in
    let plane = Path3.to_plane hd in
    let drop, keep, _, _ = List.fold_left f ([], [ hd ], 1, plane) rows in
    List.rev drop, List.rev keep

let of_rows
  ?(rev = false)
  ?(endcaps = `Both)
  ?(col_wrap = true)
  ?(style = `Default)
  layers
  =
  let looped =
    match endcaps with
    | `Loop -> true
    | _ -> false
  in
  match layers with
  | [] -> empty
  | [ _ ] -> invalid_arg "Only one layer provided."
  | hd :: tl ->
    let n_layers = List.length layers
    and n_facets = List.length hd in
    let n_rows = n_layers - if looped then 0 else 1
    and n_cols = n_facets - if col_wrap then 0 else 1 in
    if not (List.for_all (fun l -> List.length l = n_facets) tl)
    then invalid_arg "Inconsistent layer length.";
    let idxs r c =
      let i1 = (r mod n_layers * n_facets) + (c mod n_facets)
      and i2 = ((r + 1) mod n_layers * n_facets) + (c mod n_facets)
      and i3 = ((r + 1) mod n_layers * n_facets) + ((c + 1) mod n_facets)
      and i4 = (r mod n_layers * n_facets) + ((c + 1) mod n_facets) in
      i1, i2, i3, i4
    in
    let ps =
      let points = List.concat layers in
      match style with
      | `Quincunx ->
        let ps' = Array.of_list points in
        let rec loop acc r c =
          let acc =
            let i1, i2, i3, i4 = idxs r c in
            V3.mean' [| ps'.(i1); ps'.(i2); ps'.(i3); ps'.(i4) |] :: acc
          in
          if c = n_cols - 1
          then if r = n_rows - 1 then acc else loop acc (r + 1) 0
          else loop acc r (c + 1)
        in
        let qps = List.rev @@ loop [] 0 0
        and sz = n_layers * n_facets in
        let a = Array.make (sz + List.length qps) V3.zero in
        List.iteri (fun i p -> a.(i) <- p) points;
        List.iteri (fun i p -> a.(sz + i) <- p) qps;
        a
      | _ ->
        let a = Array.make (n_layers * n_facets) V3.zero in
        List.iteri (fun i p -> a.(i) <- p) points;
        a
    in
    let faces =
      let add_face a b c acc =
        (* drop degenerate faces *)
        if V3.(
             distance ps.(a) ps.(b) > Util.epsilon
             && distance ps.(b) ps.(c) > Util.epsilon
             && distance ps.(c) ps.(a) > Util.epsilon )
        then if rev then (c, b, a) :: acc else (a, b, c) :: acc
        else acc
      in
      let add_faces default acc i1 i2 i3 i4 =
        if default
        then add_face i1 i3 i2 acc |> add_face i1 i4 i3
        else add_face i1 i4 i2 acc |> add_face i2 i4 i3
      in
      let f =
        match style with
        | `Quincunx ->
          let n = n_layers * n_facets in
          fun acc r c ->
            let i1, i2, i3, i4 = idxs r c
            and i5 = n + (r * n_cols) + c in
            add_face i1 i5 i2 acc
            |> add_face i2 i5 i3
            |> add_face i3 i5 i4
            |> add_face i4 i5 i1
        | `Alt ->
          fun acc r c ->
            let i1, i2, i3, i4 = idxs r c in
            add_faces false acc i1 i2 i3 i4
        | `MinEdge ->
          fun acc r c ->
            let i1, i2, i3, i4 = idxs r c in
            let default = V3.(distance ps.(i4) ps.(i2) <= distance ps.(i1) ps.(i3)) in
            add_faces default acc i1 i2 i3 i4
        | (`Convex | `Concave) as con ->
          (* find normal for 3 of the points, is the other point above or below? *)
          let side =
            match con with
            | `Convex -> Fun.id
            | _ -> not
          in
          fun acc r c ->
            let i1, i2, i3, i4 = idxs r c in
            let n =
              V3.(cross (sub ps.(i2) ps.(i1)) (sub ps.(i3) ps.(i1)))
              |> if rev then V3.neg else Fun.id
            in
            if Math.approx (V3.z n) 0.
            then add_face i1 i4 i3 acc
            else add_faces (side V3.(dot n ps.(i4) > dot n ps.(i1))) acc i1 i2 i3 i4
        | `Default ->
          fun acc r c ->
            let i1, i2, i3, i4 = idxs r c in
            add_faces true acc i1 i2 i3 i4
      in
      let rec loop acc seg face =
        let acc = f acc seg face in
        if face = n_cols - 1
        then if seg = n_rows - 1 then acc else loop acc (seg + 1) 0
        else loop acc seg (face + 1)
      in
      let faces = loop [] 0 0
      and bottom_cap () =
        let bot = List.hd layers in
        let hd = List.hd bot in
        if List.for_all (V3.equal hd) bot
        then []
        else (
          let proj = Plane.project (Path3.to_plane ~no_check:true bot) in
          let a = Util.array_of_list_map proj bot in
          Triangulate.triangulate ~rev:(not rev) a )
      and top_cap () =
        let offset = n_facets * (n_layers - 1)
        and top = List.nth layers (n_layers - 1) in
        let hd = List.hd top in
        if List.for_all (V3.equal hd) top
        then []
        else (
          let proj = Plane.project (Path3.to_plane ~no_check:true top) in
          let a = Util.array_of_list_map proj top in
          let f = if rev then Util.offset_tri_rev else Util.offset_tri in
          List.map (f offset) (Triangulate.triangulate a) )
      in
      match endcaps with
      | `Both -> List.concat [ top_cap (); bottom_cap (); faces ]
      | `None | `Loop -> faces
      | `Top -> List.rev_append (top_cap ()) faces
      | `Bot -> List.rev_append (bottom_cap ()) faces
    in
    { points = ps; faces }

let of_ragged ?(looped = false) ?(rev = false) rows =
  let starts_lenghts, points =
    let f (start, starts_lengths, points) row =
      let g (i, ps) p = i + 1, p :: ps in
      let len, points = List.fold_left g (0, points) row in
      start + len, (start, len) :: starts_lengths, points
    in
    let _, starts_lengths, points = List.fold_left f (0, [], []) rows in
    List.rev starts_lengths, List.rev points
  in
  match starts_lenghts with
  | [] | [ _ ] -> empty
  | hd :: tl ->
    let f ((start, len), faces) ((next_start, next_len) as next) =
      let faces =
        match next_len - len with
        | 0 ->
          let a i = i + start, i + start + 1, i + next_start
          and b i = i + start + 1, i + next_start + 1, i + next_start in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init (len - 1) b
        | 1 ->
          let a i = i + start, i + start + 1, i + next_start + 1
          and b i = i + start, i + next_start + 1, i + next_start in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init len b
        | -1 ->
          let a i = i + start + 1, i + next_start + 1, i + next_start
          and b i = i + start, i + start + 1, i + next_start in
          Util.prepend_init (len - 2) a faces |> Util.prepend_init (len - 1) b
        | 2 ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = i + start, i + start + 1, i + next_start + 1
          and b i =
            let i = i + count in
            i + start, i + start + 1, i + next_start + 2
          and c i = i + start, i + next_start + 1, i + next_start
          and d i =
            let i = i + count + 1 in
            i + start - 1, i + next_start + 1, i + next_start
          in
          Util.prepend_init count a faces
          |> Util.prepend_init (len - count - 1) b
          |> Util.prepend_init (count + 1) c
          |> Util.prepend_init (next_len - 2 - count) d
        | -2 ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = i + next_start, i + start + 1, i + next_start + 1
          and b i =
            let i = i + count - 1 in
            i + next_start, i + start + 2, i + next_start + 1
          and c i = i + next_start, i + start, i + start + 1
          and d i =
            let i = i + count in
            i + next_start - 1, i + start, i + start + 1
          in
          Util.prepend_init (count - 1) a faces
          |> Util.prepend_init (len - count - 2) b
          |> Util.prepend_init count c
          |> Util.prepend_init (next_len + 1 - count) d
        | delta ->
          let s = Printf.sprintf "Unsupported layer length difference of %i" delta in
          invalid_arg s
      in
      next, faces
    in
    let _, all_faces = List.fold_left f (hd, []) (if looped then tl @ [ hd ] else tl)
    and verts = Array.of_list points in
    let faces =
      let cull_degenerate =
        let v = Array.unsafe_get verts in
        let not_degen a b = Float.compare (V3.distance (v a) (v b)) Util.epsilon = 1 in
        fun ((i0, i1, i2) as face) ->
          if not_degen i0 i1 && not_degen i1 i2 && not_degen i2 i0
          then if rev then Some (i2, i1, i0) else Some face
          else None
      in
      List.filter_map cull_degenerate all_faces
    in
    { points = verts; faces }

let of_path2 ?rev layer =
  let ps = Array.of_list layer in
  let faces = Triangulate.triangulate ?rev ps in
  { points = Array.map V3.of_v2 ps; faces }

let of_path3 ?rev layer =
  let points = Array.of_list layer in
  let faces = Triangulate.triangulate ?rev (Array.map V2.of_v3 points) in
  { points; faces }

let of_poly2 ?rev p2 =
  match Poly2.holes p2 with
  | [] -> of_path2 ?rev (Poly2.outer p2)
  | holes ->
    let points, faces = PolyHoles.partition ?rev ~holes (Poly2.outer p2) in
    { points; faces }

let of_poly3 ?rev = function
  | Poly3.{ outer; holes = [] } -> of_path3 ?rev outer
  | Poly3.{ outer; holes } ->
    let plane = Plane.of_normal ~point:(List.hd outer) @@ Path3.normal outer in
    let project = Path3.project plane
    and lift = Plane.lift plane in
    let holes = List.map project holes in
    let points, faces = PolyHoles.partition ?rev ~lift ~holes (project outer) in
    { points; faces }

let of_polygons polys =
  let lengths = Util.array_of_list_map List.length polys in
  let n = Array.length lengths in
  let offsets =
    let a = Array.make (n + 1) 0 in
    for i = 1 to n do
      a.(i) <- a.(i - 1) + lengths.(i - 1)
    done;
    a
  in
  let faces = List.init n (fun i -> List.init lengths.(i) (fun j -> j + offsets.(i)))
  and points = List.concat polys in
  of_polyhedron points faces

let join = function
  | [] -> empty
  | [ t ] -> t
  | { points; faces } :: ts ->
    let f (n, ps, fs) t =
      let offset = List.map (Util.offset_tri n) t.faces in
      n + Array.length t.points, t.points :: ps, offset :: fs
    in
    let _, ps, fs = List.fold_left f (Array.length points, [ points ], [ faces ]) ts in
    { points = Array.concat (List.rev ps); faces = List.concat (List.rev fs) }

let merge_points ?(eps = Util.epsilon) { points = pts; faces } =
  let drop = IntTbl.create 100 in
  let len = Array.length pts in
  let () =
    (* naive search if the mesh is small (avoid building search tree) *)
    if len < 400
    then
      for i = 0 to len - 2 do
        for j = i + 1 to len - 1 do
          if (not (IntTbl.mem drop j)) && V3.approx ~eps pts.(i) pts.(j)
          then IntTbl.add drop j i
        done
      done
    else (
      let tree = BallTree3.make' pts in
      for i = 1 to len - 1 do
        match BallTree3.search_idxs ~radius:eps tree pts.(i) with
        | [] | [ _ ] -> () (* single result will be self *)
        | hd :: tl ->
          let min_match = List.fold_left Int.min hd tl in
          if i <> min_match then IntTbl.add drop i min_match
      done )
  in
  let points =
    let f (i, acc) p = if IntTbl.mem drop i then i + 1, acc else i + 1, p :: acc in
    let _, points = Array.fold_left f (0, []) pts in
    Util.array_of_list_rev points
  and faces =
    let lookup = Array.make len 0
    and off = ref 0
    and offsets = Array.make len 0 in
    for i = 1 to len - 1 do
      offsets.(i) <- !off;
      match IntTbl.find_opt drop i with
      | Some idx ->
        lookup.(i) <- idx - offsets.(idx);
        incr off
      | None -> lookup.(i) <- i - !off
    done;
    let prune_degen acc (a, b, c) =
      let a = lookup.(a)
      and b = lookup.(b)
      and c = lookup.(c) in
      if a = b || b = c || c = a then acc else (a, b, c) :: acc
    in
    List.fold_left prune_degen [] faces
  in
  { points; faces }

let drop_unused_points { points; faces } =
  let size = Array.length points in
  let keep = Array.make size false
  and remap = Array.make size 0
  and count = ref 0 in
  let () =
    let add_face s (a, b, c) = IntSet.add a s |> IntSet.add b |> IntSet.add c in
    let set = List.fold_left add_face IntSet.empty faces in
    for i = 0 to size - 1 do
      if IntSet.mem i set
      then begin
        keep.(i) <- true;
        remap.(i) <- !count;
        incr count
      end
    done
  in
  let points =
    let a = Array.make !count V3.zero
    and idx = ref 0 in
    for i = 0 to size - 1 do
      if keep.(i)
      then (
        a.(!idx) <- points.(i);
        incr idx )
    done;
    a
  and faces = List.map (fun (a, b, c) -> remap.(a), remap.(b), remap.(c)) faces in
  { points; faces }

let rev_faces t = { t with faces = List.map Util.rev_tri t.faces }

let volume { points = pts; faces } =
  if Array.length pts = 0
  then 0.
  else (
    let sum_face total_vol (i1, i2, i3) =
      V3.(dot (cross pts.(i3) pts.(i2)) pts.(i1)) +. total_vol
    in
    List.fold_left sum_face 0. faces /. 6. )

let area { points = pts; faces } =
  if Array.length pts = 0
  then 0.
  else (
    let f sum (a, b, c) =
      let face = [ pts.(a); pts.(b); pts.(c) ] in
      let poly = Path3.(project (to_plane face) face) in
      sum +. Poly2.(area @@ make poly)
    in
    List.fold_left f 0. faces )

let centroid ?(eps = Util.epsilon) { points = pts; faces } =
  if Array.length pts = 0 then invalid_arg "No centroid for empty polyhedron.";
  let sum_face (total_vol, weighted_sum) (i1, i2, i3) =
    let vol = V3.(dot (cross pts.(i3) pts.(i2)) pts.(i1)) in
    let weighted = V3.(smul (add pts.(i1) (add pts.(i2) pts.(i3))) vol) in
    vol +. total_vol, V3.add weighted_sum weighted
  in
  let total_vol, weighted_sum = List.fold_left sum_face (0., V3.zero) faces in
  if Math.approx ~eps total_vol 0.
  then invalid_arg "The polyhedron has self-intersections.";
  V3.(sdiv weighted_sum (total_vol *. 4.))

let enforce_winding w shape =
  let reverse =
    match w with
    | `CCW -> Path2.is_clockwise shape
    | `CW -> not @@ Path2.is_clockwise shape
    | `NoCheck -> false
  in
  if reverse then List.rev shape else shape

(* Adapted from BOSL2's hull3d_faces in the geometry module
(https://github.com/revarbat/BOSL2/blob/f174071dc2e2529591ab87417e87d8daead0b1bb/geometry.scad#L2405)
which is based on the method described here:
https://www.hackerearth.com/practice/math/geometry/line-sweep-technique/tutorial/ *)

module EdgeSet = Set.Make (struct
  type t = int * int

  let compare (a1, a2) (b1, b2) =
    let c = Int.compare a1 b1 in
    if Int.equal c 0 then Int.compare a2 b2 else c
end)

let hull = function
  | [ _ ] | [ _; _ ] -> invalid_arg "Too few points (< 3) to hull."
  | points ->
    let (a, b, c), plane =
      match Path3.noncollinear_triple points with
      | Some (idxs, (a, b, c)) -> idxs, Plane.make a b c
      | None -> invalid_arg "Cannot hull collinear points."
    and non_coplanar plane ps =
      let rec loop i = function
        | [] -> None
        | hd :: tl ->
          if Float.abs (Plane.distance_to_point plane hd) > Util.epsilon
          then Some i
          else loop (i + 1) tl
      in
      loop 0 ps
    in
    ( match non_coplanar plane points with
      | None -> of_path3 @@ Path2.(lift plane @@ hull @@ Path3.project plane points)
      | Some d ->
        let ps = Array.of_list points in
        let add_tri a b c ((triangles, planes) as acc) =
          try (a, b, c) :: triangles, Plane.make ps.(a) ps.(b) ps.(c) :: planes with
          | Invalid_argument _ -> acc (* invalid triangle (points are collinear) *)
        and[@warning "-partial-match"] add_edges edges (a, b, c) =
          EdgeSet.add (c, a) edges |> EdgeSet.add (b, c) |> EdgeSet.add (a, b)
        and b, c = if Plane.is_point_above plane ps.(d) then c, b else b, c in
        let triangles, planes =
          add_tri a b c ([], []) |> add_tri d b a |> add_tri c d a |> add_tri b d c
        in
        let f idx ((triangles, planes) as acc) =
          if idx <> a && idx <> b && idx <> c && idx <> d (* skip starting points *)
          then (
            (* collect half edges of triangles that are in conflict with the points
              at idx, pruning the conflicting triangles and their planes in the process *)
            let half_edges, triangles, planes =
              let f (edges, keep_tri, keep_pln) tri pln =
                if Plane.distance_to_point pln ps.(idx) > Util.epsilon
                then add_edges edges tri, keep_tri, keep_pln
                else edges, tri :: keep_tri, pln :: keep_pln
              in
              List.fold_left2 f (EdgeSet.empty, [], []) triangles planes
            in
            (* form new triangles with the outer perimeter (horizon) of the set of
               conflicting triangles and the point at idx *)
            let non_internal (a, b) acc =
              if EdgeSet.mem (b, a) half_edges then acc else add_tri a b idx acc
            in
            EdgeSet.fold non_internal half_edges (triangles, planes) )
          else acc
        in
        let faces, _ = Util.fold_init (Array.length ps) f (triangles, planes) in
        { points = ps; faces } )

let translate p t = { t with points = APath3.translate p t.points }
let xtrans x t = { t with points = APath3.xtrans x t.points }
let ytrans y t = { t with points = APath3.ytrans y t.points }
let ztrans z t = { t with points = APath3.ztrans z t.points }
let rotate ?about r t = { t with points = APath3.rotate ?about r t.points }
let xrot ?about r t = { t with points = APath3.xrot ?about r t.points }
let yrot ?about r t = { t with points = APath3.yrot ?about r t.points }
let zrot ?about r t = { t with points = APath3.zrot ?about r t.points }
let quaternion ?about q t = { t with points = APath3.quaternion ?about q t.points }
let axis_rotate ?about ax r = quaternion ?about (Quaternion.make ax r)
let affine m t = { t with points = APath3.affine m t.points }
let scale s t = { t with points = APath3.scale s t.points }
let xscale x t = { t with points = APath3.xscale x t.points }
let yscale y t = { t with points = APath3.yscale y t.points }
let zscale z t = { t with points = APath3.zscale z t.points }
let mirror ax t = rev_faces { t with points = Array.map (V3.mirror ax) t.points }

let to_binstl ~rev path { points = pts; faces } =
  let write_unsigned_long oc n =
    Out_channel.output_byte oc Int32.(to_int n lsr 0);
    Out_channel.output_byte oc Int32.(to_int n lsr 8);
    Out_channel.output_byte oc Int32.(to_int n lsr 16);
    Out_channel.output_byte oc Int32.(to_int @@ shift_right_logical n 24)
  in
  let write_float oc n = write_unsigned_long oc (Int32.bits_of_float n) in
  let write_v3 oc p =
    write_float oc @@ V3.x p;
    write_float oc @@ V3.y p;
    write_float oc @@ V3.z p
  in
  let f oc =
    (* header *)
    let bs = Bytes.make 80 ' '
    and header = "OCADml Mesh" in
    Bytes.blit_string header 0 bs 0 (String.length header);
    Bytes.set bs 79 '\n';
    Out_channel.output_bytes oc bs;
    (* number of facets *)
    write_unsigned_long oc (Int32.of_int @@ List.length faces);
    List.iter
      (fun (a, b, c) ->
        let a, b, c =
          if rev then pts.(c), pts.(b), pts.(a) else pts.(a), pts.(b), pts.(c)
        in
        (* facet normal *)
        let normal = V3.(normalize @@ cross (sub c a) (sub b a)) in
        write_v3 oc normal;
        (* vertices *)
        write_v3 oc a;
        write_v3 oc b;
        write_v3 oc c;
        (* attribute byte count (set to zero) *)
        Out_channel.output_string oc "\000\000" )
      faces
  in
  Out_channel.with_open_bin path f

let to_stl ~rev path { points = pts; faces } =
  let write_v3 oc p =
    Out_channel.output_string oc (Float.to_string @@ V3.x p);
    Out_channel.output_char oc ' ';
    Out_channel.output_string oc (Float.to_string @@ V3.y p);
    Out_channel.output_char oc ' ';
    Out_channel.output_string oc (Float.to_string @@ V3.z p)
  in
  let write_vertex oc v =
    Out_channel.output_string oc "\n      vertex ";
    write_v3 oc v
  in
  let f oc =
    Printf.fprintf oc "solid OCADml_Mesh";
    List.iter
      (fun (a, b, c) ->
        let a, b, c =
          if rev then pts.(c), pts.(b), pts.(a) else pts.(a), pts.(b), pts.(c)
        in
        let normal = V3.(normalize @@ cross (sub c a) (sub b a)) in
        Out_channel.output_string oc "\n  facet normal ";
        write_v3 oc normal;
        Out_channel.output_string oc "\n    outer loop";
        write_vertex oc a;
        write_vertex oc b;
        write_vertex oc c;
        Out_channel.output_string oc "\n    endloop";
        Out_channel.output_string oc "\n  endfacet" )
      faces;
    Printf.fprintf oc "\nendsolid OCADml_Mesh\n"
  in
  Out_channel.with_open_bin path f

let to_stl ?(ascii = false) ?(rev = true) path t =
  if ascii then to_stl ~rev path t else to_binstl ~rev path t

let of_binstl_ic ?(rev = true) ?eps ic =
  let read_unsigned_long ic =
    let ch1 = input_byte ic
    and ch2 = input_byte ic
    and ch3 = input_byte ic
    and big = Int32.shift_left (Int32.of_int (input_byte ic)) 24 in
    let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
    Int32.logor base big
  in
  let read_float ic = Int32.float_of_bits (read_unsigned_long ic) in
  let read_vertex ic =
    let x = read_float ic
    and y = read_float ic
    and z = read_float ic in
    V3.v x y z
  in
  let rec loop ic n_facets i pos ps tris =
    if i < n_facets
    then (
      (* skip over normal *)
      In_channel.seek ic (Int64.of_int (pos + 12));
      let a = read_vertex ic
      and b = read_vertex ic
      and c = read_vertex ic in
      (* normal + verts = 48, skip over 2 \000 property bytes to 50 *)
      In_channel.seek ic (Int64.of_int (pos + 50));
      let j = i * 3 in
      let tri = if rev then j + 2, j + 1, j else j, j + 1, j + 2 in
      loop ic n_facets (i + 1) (pos + 50) (c :: b :: a :: ps) (tri :: tris) )
    else ps, tris
  in
  In_channel.seek ic (Int64.of_int 80);
  let n_facets = Int32.to_int @@ read_unsigned_long ic in
  if Int64.(In_channel.length ic <> of_int ((n_facets * 50) + 84))
  then failwith "Invalid binary stl (file size <> facets * 50 + 84)";
  (* header 80 bytes + 4 bytes for n_facets -> position 84 *)
  let ps, faces = loop ic n_facets 0 84 [] [] in
  merge_points ?eps { points = Util.array_of_list_rev ps; faces }

let of_asciistl_ic ?(rev = true) ?eps ic =
  let validate_line ic prefix =
    match In_channel.input_line ic with
    | Some line ->
      if not @@ String.(starts_with ~prefix @@ trim line)
      then failwith (Printf.sprintf "Invalid block tag in ascii stl (expected %s)" prefix)
    | None ->
      failwith (Printf.sprintf "Unexpected end of ascii stl (expected %s line)" prefix)
  in
  let read_vertex ic =
    match In_channel.input_line ic with
    | Some line ->
      ( try
          let[@warning "-partial-match"] [ "vertex"; x; y; z ] =
            String.(split_on_char ' ' @@ trim line)
          in
          Float.(V3.v (of_string x) (of_string y) (of_string z))
        with
        | _ -> failwith "Invalid vertex encountered in ascii stl." )
    | None -> failwith "Unexpected end of ascii stl file."
  in
  let rec loop ic i ps tris =
    match In_channel.input_line ic with
    | Some line ->
      let line = String.trim line in
      if String.starts_with ~prefix:"endsolid" line
      then ps, tris
      else if String.starts_with ~prefix:"facet" line
      then (
        (* ignoring normal which comes after facet *)
        validate_line ic "outer loop";
        let a = read_vertex ic
        and b = read_vertex ic
        and c = read_vertex ic in
        let tri = if rev then i + 2, i + 1, i else i, i + 1, i + 2 in
        validate_line ic "endloop";
        validate_line ic "endfacet";
        loop ic (i + 3) (c :: b :: a :: ps) (tri :: tris) )
      else if String.length line = 0
      then loop ic i ps tris
      else failwith "Invalid block tag in ascii stl (expected facet)."
    | None -> failwith "Unexpected end of ascii stl (expected facet or end of solid)"
  in
  if In_channel.length ic < Int64.of_int 15
  then failwith "File too short to be a valid ascii stl (< 15 bytes)";
  In_channel.seek ic (Int64.of_int 0);
  validate_line ic "solid";
  let ps, faces = loop ic 0 [] [] in
  merge_points ?eps { points = Util.array_of_list_rev ps; faces }

let of_stl ?rev ?eps path =
  let is_ascii ic =
    match In_channel.input_line ic with
    | Some line when String.(starts_with ~prefix:"solid " @@ trim line) ->
      ( match In_channel.input_line ic with
        | Some line -> String.(starts_with ~prefix:"facet " @@ trim line)
        | _ -> false )
    | _ -> false
  in
  In_channel.with_open_bin path (fun ic ->
    if is_ascii ic then of_asciistl_ic ?rev ?eps ic else of_binstl_ic ?rev ?eps ic )
