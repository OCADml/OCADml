(* Polyhole partitioning algorithm ported from
   https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad *)

open V

type tag =
  { n : int
  ; idx : int
  }

let equal_tag a b = Int.equal a.n b.n && Int.equal a.idx b.idx

let compare_tag a b =
  let c = Int.compare a.n b.n in
  if Int.equal c 0 then Int.compare a.idx b.idx else c

type point =
  { p : V2.t
  ; tag : tag
  }

let neg_point { p; tag } = { p = V2.neg p; tag }

module BridgeSet = Set.Make (struct
  type t = tag * tag

  let compare (a1, a2) (b1, b2) =
    let c = compare_tag a1 b1 in
    if Int.equal c 0 then compare_tag a2 b2 else c
end)

(* Opposite of usual convention, but this works. Flip inputs for now. *)
let is_ccw a b c = V3.get_z V2.(cross (sub b a) (sub c a)) > 0.

(* check if point p is within the CCW triangle described by p1, p2, and p3. *)
let in_tri p p1 p2 p3 = is_ccw p1 p p2 && is_ccw p2 p p3

(* find closest intersect on the outer path made with rightward horizontal ray
   from point p *)
let outer_intersect p outer =
  let len = Array.length outer in
  let seg_idx = ref 0
  and out_x = ref Float.infinity
  and out_y = ref 0. in
  let update i { x = inter_x; y = inter_y } =
    if inter_x < !out_x
    then (
      seg_idx := i;
      out_x := inter_x;
      out_y := inter_y )
  in
  for i = 0 to len - 1 do
    let { p = po1; _ } = outer.(i)
    and { p = po2; _ } = outer.((i + 1) mod len) in
    if is_ccw p po1 po2
    then
      if Float.equal p.y po1.y
      then update i po1
      else if Float.equal p.y po2.y
      then update i po2
      else if po1.y < p.y && po2.y >= p.y
      then (
        let u = (p.y -. po2.y) /. (po1.y -. po2.y) in
        update i (V2.lerp po1 po2 u) )
  done;
  if Float.is_infinite !out_x
  then failwith "Invalid polygon: holes may intersect with eachother, or the outer walls."
  else !seg_idx, V2.v !out_x !out_y

(* Find a bridge between the point p (in the interior of poly outer) and a
   vertex (given by index) in the outer path. *)
let bridge_to_outer { p = { x; y } as pt; _ } outer =
  let seg_idx, intersect = outer_intersect pt outer
  and len = Array.length outer in
  let next_idx = Util.index_wrap ~len (seg_idx + 1) in
  let first, valid_candidate =
    let { p = { x = seg_x; _ } as seg; _ } = outer.(seg_idx)
    and { p = { x = next_x; _ } as next; _ } = outer.(next_idx) in
    if seg_x > x || next_x <= x
    then seg_idx, fun ({ y = cy; _ } as cp) -> cy < y && in_tri seg pt cp intersect
    else next_idx, fun ({ y = cy; _ } as cp) -> cy > y && in_tri cp pt next intersect
  in
  let idx = ref first
  and min_x = ref @@ V2.get_x outer.(first).p in
  for i = 0 to len - 1 do
    let { p = { x = cx; _ } as p; _ } = outer.(i) in
    if valid_candidate p
    then
      if cx < !min_x
      then (
        min_x := cx;
        idx := i )
  done;
  !idx

let extremes holes =
  let max_x m ({ p = { x; _ }; _ } as e) = if x > V2.get_x m.p then e else m in
  let rightmost =
    Array.init (Array.length holes) (fun i ->
        Array.fold_left max_x holes.(i).(0) holes.(i) )
  in
  Array.sort (fun { p = p1; _ } { p = p2; _ } -> Float.compare p2.x p1.x) rightmost;
  rightmost

let polyhole_complex ~holes outer =
  let f (poly, bridges) { tag = { n = hole_idx; idx = bridge_start }; _ } =
    let hole = holes.(hole_idx) in
    let bridge_end = bridge_to_outer hole.(bridge_start) poly in
    let len_poly = Array.length poly
    and len_hole = Array.length hole
    and bridge = hole.(bridge_start).tag, poly.(bridge_end).tag in
    let poly =
      Array.concat
        [ Array.init (len_poly + 1) (fun j -> poly.((bridge_end + j) mod len_poly))
        ; Array.init (len_hole + 1) (fun j -> hole.((bridge_start + j) mod len_hole))
        ]
    in
    poly, bridge :: bridges
  in
  (* bridges will be reversed when duplicates are removed later *)
  Array.fold_left f (outer, []) (extremes holes)

let remove_duplicate_bridges a b =
  let a_set = BridgeSet.of_list a in
  let f bs ((bridge_start, bridge_end) as bridge) =
    if BridgeSet.mem (bridge_end, bridge_start) a_set then bs else bridge :: bs
  in
  List.fold_left f [] b

let insert_bridge (bridge_start, bridge_end) polys =
  let f idx poly =
    let len = Array.length poly
    and i = ref 0
    and start_idx = ref None
    and end_idx = ref None in
    while (Option.is_none !start_idx || Option.is_none !end_idx) && !i < len do
      let { tag; _ } = poly.(!i) in
      if Option.is_none !start_idx && equal_tag tag bridge_start
      then start_idx := Some !i
      else if Option.is_none !end_idx && equal_tag tag bridge_end
      then end_idx := Some !i;
      incr i
    done;
    match !start_idx, !end_idx with
    | Some si, Some ei -> Some (si, ei, idx)
    | _                -> None
  in
  let start_idx, end_idx, poly_idx = Option.get @@ Util.array_find_mapi f polys in
  let poly = polys.(poly_idx) in
  let len = Array.length poly
  and n_poly = Array.length polys in
  let len_div_es, len_div_se =
    if start_idx < end_idx
    then start_idx + len - end_idx, end_idx - start_idx
    else start_idx - end_idx, end_idx + len - start_idx
  in
  let end_to_start = Array.init (len_div_es + 1) (fun j -> poly.((end_idx + j) mod len))
  and start_to_end = Array.init (len_div_se + 1) (fun j -> poly.((start_idx + j) mod len))
  and rest = Array.init (n_poly - 1) (fun j -> polys.((poly_idx + 1 + j) mod n_poly)) in
  Array.concat [ [| end_to_start; start_to_end |]; rest ]

let partition ?(rev = false) ?(lift = fun { x; y } -> V3.v x y 0.) ~holes outer =
  let outer_sign = Path2.clockwise_sign outer in
  let flipped = Float.equal (-1.) outer_sign in
  let outer = if flipped then List.rev outer else outer in
  if not @@ List.for_all (fun h -> Path2.clockwise_sign h <> outer_sign) holes
  then invalid_arg "Holes must have opposite winding direction of the outer polygon.";
  let holes = if flipped then List.map List.rev holes else holes in
  let pos_holes =
    let f n = Util.array_of_list_mapi (fun idx p -> { p; tag = { n; idx } }) in
    Util.array_of_list_mapi f holes
  in
  let n_holes = Array.length pos_holes in
  let pos_outer =
    Util.array_of_list_mapi (fun idx p -> { p; tag = { n = n_holes; idx } }) outer
  in
  let face_offsets =
    let f (acc, start) hole =
      let start = start + Array.length hole in
      start :: acc, start
    in
    let offsets, _ = Array.fold_left f ([ 0 ], 0) pos_holes in
    Util.array_of_list_rev offsets
  in
  let poly, pos_bridges = polyhole_complex ~holes:pos_holes pos_outer
  and _, neg_bridges =
    polyhole_complex
      ~holes:(Array.map (fun hole -> Array.map neg_point hole) pos_holes)
      (Array.map neg_point pos_outer)
  in
  let bridges = remove_duplicate_bridges pos_bridges neg_bridges in
  let polys = List.fold_left (fun polys b -> insert_bridge b polys) [| poly |] bridges in
  let points = List.map lift @@ List.concat @@ List.concat [ holes; [ outer ] ]
  and faces =
    let f i =
      let poly = polys.(i) in
      let len = Array.length poly in
      let flip =
        if (rev && not flipped) || ((not rev) && flipped)
        then fun j -> len - 1 - j
        else Fun.id
      in
      List.init len (fun j ->
          let { tag = { n; idx }; _ } = poly.(flip j) in
          idx + face_offsets.(n) )
    in
    List.init (Array.length polys) f
  in
  points, faces
