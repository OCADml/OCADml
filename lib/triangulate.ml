(* algorithm ported from the polygon_triangulate function in the BOSL2 OpenSCAD library:
   https://github.com/revarbat/BOSL2/blob/6c43c19c73b1dda48f99b0aa9481007870b87fcc/geometry.scad#L1891 *)

let wrap_sub a pos len =
  let n = Array.length a in
  let pos = pos mod n in
  if pos + len <= n
  then Array.sub a pos len
  else Array.init len (fun i -> a.((pos + i) mod n))

(* any two vertices are equal *)
let degenerate_tri ?eps p0 p1 p2 =
  V2.approx ?eps p0 p1 || V2.approx ?eps p0 p2 || V2.approx ?eps p1 p2

(* returns false as soon as it finds some reflex vertex of the polygon
    described by the given [idxs] into [poly] inside the triangle different
    from p0 and p2. Input polygon is assumed to have no twists. *)
let none_inside ?eps poly idxs p0 p1 p2 =
  let len_idxs = Array.length idxs in
  let rec loop i =
    if i >= len_idxs
    then true
    else (
      let vert = poly.(idxs.(i))
      and prev = poly.(idxs.(Util.index_wrap ~len:len_idxs (i - 1)))
      and next = poly.(idxs.(Util.index_wrap ~len:len_idxs (i + 1))) in
      (* check if vert prevents the triangle (p0 p1 p2) from being an ear *)
      if V2.clockwise_sign ?eps prev vert next <= 0. (* reflex condition *)
         && ( V2.(
                (* vert is a cw reflex poly vertex inside the triangle  *)
                clockwise_sign ?eps p0 p1 vert > 0.
                && clockwise_sign ?eps p1 p2 vert > 0.
                && clockwise_sign ?eps p2 p0 vert >= 0. )
              || V2.(
                   (* vert = p1 and some of its adjacent edges cross the open segment *)
                   approx ?eps vert p1
                   && left_of_line ?eps ~line:{ a = prev; b = p1 } p0 <= 0.
                   && left_of_line ?eps ~line:{ a = p1; b = prev } p2 <= 0.
                   && left_of_line ?eps ~line:{ a = p1; b = next } p2 <= 0.
                   && left_of_line ?eps ~line:{ a = next; b = p1 } p0 <= 0. ) )
      then false
      else loop (i + 1) )
  in
  loop 0

let get_ear ?eps poly idxs =
  let len_idxs = Array.length idxs in
  if len_idxs = 3
  then Some (`Ear 0)
  else (
    let rec loop i =
      let p0 = poly.(idxs.(i))
      and p1 = poly.(idxs.((i + 1) mod len_idxs))
      and p2 = poly.(idxs.((i + 2) mod len_idxs)) in
      if V2.clockwise_sign ?eps p0 p1 p2 > 0.
         && none_inside ?eps poly (wrap_sub idxs (i + 2) (len_idxs - 1)) p0 p1 p2
      then Some (`Ear i)
      else if i < len_idxs - 1
      then loop (i + 1)
      else (
        let whisker = ref None
        and j = ref 0 in
        while Option.is_none !whisker && !j < len_idxs do
          if V2.approx ?eps poly.(idxs.(!j)) poly.(idxs.((!j + 2) mod len_idxs))
          then whisker := Some (`Degen !j)
          else incr j
        done;
        !whisker )
    in
    loop 0 )

let triangulate' ?(rev = false) ?eps poly idxs =
  let tri =
    if rev
    then fun idxs a b c -> idxs.(c), idxs.(b), idxs.(a)
    else fun idxs a b c -> idxs.(a), idxs.(b), idxs.(c)
  in
  let rec loop tris idxs =
    let len_idxs = Array.length idxs in
    if len_idxs = 3
    then
      if degenerate_tri ?eps poly.(idxs.(0)) poly.(idxs.(1)) poly.(idxs.(2))
      then tris
      else tri idxs 0 1 2 :: tris
    else (
      match get_ear ?eps poly idxs with
      | None ->
        failwith
          "Triangulation failed: the polygon has twists, is collinear, or non-coplanar."
      | Some (`Degen ear) ->
        (* discard if degenerate and move on *)
        if len_idxs <= 4 then tris else loop tris (wrap_sub idxs (ear + 3) (len_idxs - 2))
      | Some (`Ear ear) ->
        let tri =
          tri idxs (ear mod len_idxs) ((ear + 1) mod len_idxs) ((ear + 2) mod len_idxs)
        and idxs = wrap_sub idxs (ear + 2) (len_idxs - 1) in
        loop (tri :: tris) idxs )
  in
  loop [] idxs

let triangulate ?(rev = false) ?eps ?idxs poly =
  let len_poly = Array.length poly in
  let idxs, len_idxs =
    match idxs with
    | Some idxs ->
      let len_idxs = Array.length idxs in
      if len_poly < 3
      then invalid_arg "Polygon must have more than 2 points to be triangulated";
      for i = 0 to len_idxs - 1 do
        if idxs.(i) < 0 || idxs.(i) >= len_poly
        then invalid_arg "Triangulation indices are out of bound for given poly points."
      done;
      idxs, len_idxs
    | None -> Array.init len_poly Fun.id, len_poly
  in
  if len_idxs <= 2
  then []
  else if len_idxs = 3
  then
    if degenerate_tri ?eps poly.(idxs.(0)) poly.(idxs.(1)) poly.(idxs.(2))
    then []
    else (
      if V2.collinear poly.(idxs.(0)) poly.(idxs.(1)) poly.(idxs.(2))
      then invalid_arg "Triangulation cannot be performed on a collinear triplet.";
      if rev then [ idxs.(2), idxs.(1), idxs.(0) ] else [ idxs.(0), idxs.(1), idxs.(2) ] )
  else if APath2.is_clockwise (Array.init len_idxs (fun i -> poly.(idxs.(i))))
  then triangulate' ~rev ?eps poly idxs
  else
    triangulate' ~rev ?eps poly (Array.init len_idxs (fun i -> idxs.(len_idxs - 1 - i)))
    |> List.map Util.rev_tri
