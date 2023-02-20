module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end)

module IntSet = Set.Make (Int)

type t =
  { size : int
  ; points : V3.t list
  ; faces : int list list
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

let empty = { size = 0; points = []; faces = [] }
let size t = t.size
let points t = t.points
let faces t = t.faces
let make ~points ~faces = { size = List.length points; points; faces }

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
    let points =
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
        List.append points (List.rev @@ loop [] 0 0)
      | _ -> points
    in
    let faces =
      let ps = Array.of_list points in
      let add_face a b c acc =
        (* drop degenerate faces *)
        if V3.(
             distance ps.(a) ps.(b) > Util.epsilon
             && distance ps.(b) ps.(c) > Util.epsilon
             && distance ps.(c) ps.(a) > Util.epsilon )
        then if rev then [ c; b; a ] :: acc else [ a; b; c ] :: acc
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
      and bottom_cap =
        List.init n_facets (if rev then Fun.id else fun i -> n_facets - 1 - i)
      and top_cap =
        let offset = n_facets * (n_layers - 1) in
        let f = if rev then fun i -> offset + n_facets - 1 - i else ( + ) offset in
        List.init n_facets f
      in
      match endcaps with
      | `Both -> top_cap :: bottom_cap :: faces
      | `None | `Loop -> faces
      | `Top -> top_cap :: faces
      | `Bot -> bottom_cap :: faces
    in
    { size = n_layers * n_facets; points; faces }

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
          let a i = [ i + start; i + start + 1; i + next_start ]
          and b i = [ i + start + 1; i + next_start + 1; i + next_start ] in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init (len - 1) b
        | 1 ->
          let a i = [ i + start; i + start + 1; i + next_start + 1 ]
          and b i = [ i + start; i + next_start + 1; i + next_start ] in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init len b
        | -1 ->
          let a i = [ i + start + 1; i + next_start + 1; i + next_start ]
          and b i = [ i + start; i + start + 1; i + next_start ] in
          Util.prepend_init (len - 2) a faces |> Util.prepend_init (len - 1) b
        | 2 ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = [ i + start; i + start + 1; i + next_start + 1 ]
          and b i =
            let i = i + count in
            [ i + start; i + start + 1; i + next_start + 2 ]
          and c i = [ i + start; i + next_start + 1; i + next_start ]
          and d i =
            let i = i + count + 1 in
            [ i + start - 1; i + next_start + 1; i + next_start ]
          in
          Util.prepend_init count a faces
          |> Util.prepend_init (len - count - 1) b
          |> Util.prepend_init (count + 1) c
          |> Util.prepend_init (next_len - 2 - count) d
        | -2 ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = [ i + next_start; i + start + 1; i + next_start + 1 ]
          and b i =
            let i = i + count - 1 in
            [ i + next_start; i + start + 2; i + next_start + 1 ]
          and c i = [ i + next_start; i + start; i + start + 1 ]
          and d i =
            let i = i + count in
            [ i + next_start - 1; i + start; i + start + 1 ]
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
        function
        | [ i0; i1; i2 ] as face ->
          if not_degen i0 i1 && not_degen i1 i2 && not_degen i2 i0
          then if rev then Some [ i2; i1; i0 ] else Some face
          else None
        | _ -> failwith "unreachable"
      in
      List.filter_map cull_degenerate all_faces
    in
    { size = Array.length verts; points; faces }

let of_path2 ?(rev = false) layer =
  let size, points, face =
    List.fold_left
      (fun (n, ps, fs) p -> n + 1, V3.of_v2 p :: ps, n :: fs)
      (0, [], [])
      layer
  in
  { size; points; faces = [ (if rev then List.rev face else face) ] }

let of_path3 ?(rev = false) layer =
  let size, points, face =
    List.fold_left (fun (n, ps, fs) p -> n + 1, p :: ps, n :: fs) (0, [], []) layer
  in
  { size; points; faces = [ (if rev then List.rev face else face) ] }

let of_poly2 ?rev p2 =
  match Poly2.holes p2 with
  | [] -> of_path2 ?rev (Poly2.outer p2)
  | holes ->
    let points, faces = PolyHoles.partition ?rev ~holes (Poly2.outer p2) in
    make ~points ~faces

let of_poly3 ?rev = function
  | Poly3.{ outer; holes = [] } -> of_path3 ?rev outer
  | Poly3.{ outer; holes } ->
    let plane = Plane.of_normal ~point:(List.hd outer) @@ Path3.normal outer in
    let project = Path3.project plane
    and lift = Plane.lift plane in
    let holes = List.map project holes in
    let points, faces = PolyHoles.partition ?rev ~lift ~holes (project outer) in
    make ~points ~faces

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
  let faces = List.init n (fun i -> List.init lengths.(i) (fun j -> j + offsets.(i))) in
  { size = offsets.(n); points = List.concat polys; faces }

let join = function
  | [] -> empty
  | [ t ] -> t
  | { size; points; faces } :: ts ->
    let f (n, ps, fs) t =
      let offset = List.map (List.map (( + ) n)) t.faces in
      n + t.size, t.points :: ps, offset :: fs
    in
    let size, ps, fs = List.fold_left f (size, [ points ], [ faces ]) ts in
    { size; points = List.concat (List.rev ps); faces = List.concat (List.rev fs) }

let merge_points ?(eps = Util.epsilon) { size; points; faces } =
  let drop = IntTbl.create 100
  and pts = Array.of_list points in
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
    List.rev points
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
    let rec prune_face i first last acc = function
      | [ hd ] ->
        let hd' = lookup.(hd) in
        if hd' <> last && hd' <> first && i >= 2
        then Some (List.rev @@ (hd' :: acc))
        else if i >= 3
        then Some (List.rev acc)
        else None
      | hd :: tl ->
        let hd' = lookup.(hd) in
        if hd' <> last
        then prune_face (i + 1) first hd' (hd' :: acc) tl
        else prune_face i first last acc tl
      | [] -> None
    in
    let f acc = function
      | [] -> acc
      | hd :: tl ->
        let hd' = lookup.(hd) in
        Util.prepend_opt (prune_face 1 hd' hd' [ hd' ] tl) acc
    in
    List.fold_left f [] faces
  in
  { size = size - IntTbl.length drop; points; faces }

let drop_unused_points { size; points; faces } =
  let keep = Array.make size false
  and remap = Array.make size 0
  and count = ref 0 in
  let () =
    let add_face s face = List.fold_left (fun s i -> IntSet.add i s) s face in
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
  let points = List.filteri (fun i _ -> keep.(i)) points
  and faces = List.map (List.map (fun i -> remap.(i))) faces in
  { size = !count; points; faces }

let rev_faces t = { t with faces = List.map List.rev t.faces }

let triangulate ?eps { size; points; faces } =
  let pts = Array.of_list points in
  let f faces = function
    | [ _; _; _ ] as face -> face :: faces
    | face ->
      let face = Array.of_list face in
      let poly = Array.init (Array.length face) (fun i -> pts.(face.(i))) in
      let norm = APath3.normal poly in
      let proj = Plane.(project (of_normal ~point:pts.(face.(0)) norm)) in
      Triangulate.triangulate ?eps (Array.map proj poly)
      |> List.map (List.map (fun i -> face.(i)))
      |> Fun.flip List.rev_append faces
  in
  { size; points; faces = List.fold_left f [] faces }

let volume { size; points; faces } =
  if size = 0
  then 0.
  else (
    let pts = Array.of_list points in
    let rec sum_face total_vol p1 idxs =
      let calc total_vol p1 p2 p3 = V3.(dot (cross p3 p2) p1) +. total_vol in
      match idxs with
      | [ i2; i3 ] -> calc total_vol p1 pts.(i2) pts.(i3)
      | i2 :: (i3 :: _ as rest) -> sum_face (calc total_vol p1 pts.(i2) pts.(i3)) p1 rest
      | _ -> invalid_arg "Polyhedron contains face with fewer than 3 points."
    in
    let f total_vol = function
      | i1 :: idxs -> sum_face total_vol pts.(i1) idxs
      | [] -> invalid_arg "Polyhedron contains empty face."
    in
    List.fold_left f 0. faces /. 6. )

let area { size; points; faces } =
  if size = 0
  then 0.
  else (
    let pts = Array.of_list points in
    let f sum idxs =
      let face = List.map (fun i -> pts.(i)) idxs in
      let poly = Path3.(project (to_plane face) face) in
      sum +. Poly2.(area @@ make poly)
    in
    List.fold_left f 0. faces )

let centroid ?(eps = Util.epsilon) { size; points; faces } =
  if size = 0 then invalid_arg "No centroid for empty polyhedron.";
  let pts = Array.of_list points in
  let rec sum_face total_vol weighted_sum p1 idxs =
    let calc total_vol weighted_sum p1 p2 p3 =
      let vol = V3.(dot (cross p3 p2) p1) in
      let weighted = V3.(smul (add p1 (add p2 p3)) vol) in
      vol +. total_vol, V3.add weighted_sum weighted
    in
    match idxs with
    | [ i2; i3 ] -> calc total_vol weighted_sum p1 pts.(i2) pts.(i3)
    | i2 :: (i3 :: _ as rest) ->
      let total_vol, weighted_sum = calc total_vol weighted_sum p1 pts.(i2) pts.(i3) in
      sum_face total_vol weighted_sum p1 rest
    | _ -> invalid_arg "Polyhedron contains face with fewer than 3 points."
  in
  let total_vol, weighted_sum =
    let f (total_vol, weighted_sum) = function
      | i1 :: idxs -> sum_face total_vol weighted_sum pts.(i1) idxs
      | [] -> invalid_arg "Polyhedron contains empty face."
    in
    List.fold_left f (0., V3.zero) faces
  in
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
          try [ a; b; c ] :: triangles, Plane.make ps.(a) ps.(b) ps.(c) :: planes with
          | Invalid_argument _ -> acc (* invalid triangle (points are collinear) *)
        and[@warning "-partial-match"] add_edges edges [ a; b; c ] =
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
        { size = Array.length ps; faces; points } )

let translate p t = { t with points = Path3.translate p t.points }
let xtrans x t = { t with points = Path3.xtrans x t.points }
let ytrans y t = { t with points = Path3.ytrans y t.points }
let ztrans z t = { t with points = Path3.ztrans z t.points }
let rotate ?about r t = { t with points = Path3.rotate ?about r t.points }
let xrot ?about r t = { t with points = Path3.xrot ?about r t.points }
let yrot ?about r t = { t with points = Path3.yrot ?about r t.points }
let zrot ?about r t = { t with points = Path3.zrot ?about r t.points }
let quaternion ?about q t = { t with points = Path3.quaternion ?about q t.points }
let axis_rotate ?about ax r = quaternion ?about (Quaternion.make ax r)
let affine m t = { t with points = Path3.affine m t.points }
let scale s t = { t with points = Path3.scale s t.points }
let xscale x t = { t with points = Path3.xscale x t.points }
let yscale y t = { t with points = Path3.yscale y t.points }
let zscale z t = { t with points = Path3.zscale z t.points }
let mirror ax t = rev_faces { t with points = Path3.mirror ax t.points }

let to_binstl ~rev path t =
  let pts =
    let a = Array.make t.size (List.hd t.points) in
    List.iteri (fun i p -> a.(i) <- p) t.points;
    a
  and write_unsigned_long oc n =
    Out_channel.output_byte oc ((n lsr 0) land 0xFF);
    Out_channel.output_byte oc ((n lsr 8) land 0xFF);
    Out_channel.output_byte oc ((n lsr 16) land 0xFF);
    Out_channel.output_byte oc ((n lsr 24) land 0xFF)
  in
  let write_float oc n = write_unsigned_long oc Int32.(to_int @@ bits_of_float n) in
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
    write_unsigned_long oc (List.length t.faces);
    List.iter
      (function
       | [ a; b; c ] ->
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
         Out_channel.output_string oc "\000\000"
       | _ -> failwith "Mesh should be triangulated for stl serialization." )
      t.faces
  in
  Out_channel.with_open_bin path f

let to_stl ~rev path t =
  let pts =
    let a = Array.make t.size (List.hd t.points) in
    List.iteri (fun i p -> a.(i) <- p) t.points;
    a
  and write_v3 oc p =
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
      (function
       | [ a; b; c ] ->
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
         Out_channel.output_string oc "\n  endfacet"
       | _ -> failwith "Mesh should be triangulated for stl serialization." )
      t.faces;
    Printf.fprintf oc "\nendsolid OCADml_Mesh\n"
  in
  Out_channel.with_open_bin path f

let to_stl ?(ascii = false) ?(rev = true) ?eps path t =
  let t = triangulate ?eps @@ merge_points ?eps t in
  if ascii then to_stl ~rev path t else to_binstl ~rev path t
