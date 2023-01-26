module type TANGENTSIGN = sig
  type line

  val tangent_sign : line -> line -> float
end

module TangentSign2 = struct
  open V2

  let tangent_sign (l1 : line) (l2 : line) =
    let a = sub l1.a l1.b
    and b = sub l2.a l2.b in
    let z = V3.z @@ cross a b in
    if Float.abs z <= Util.epsilon *. norm a *. norm b then 0. else Math.sign z
end

module TangentSign3 = struct
  let tangent_sign (l1 : V3.line) (l2 : V3.line) =
    let plane = Plane.make l1.a l1.b l2.b in
    Math.sign @@ Plane.line_angle plane l2
end

module Make
    (V : V.S)
    (BT : BallTree.S with type vec := V.t)
    (TS : TANGENTSIGN with type line := V.line) =
struct
  let nearby_idxs ?(min_tree_size = 400) ?(radius = Util.epsilon) path =
    if List.length path < min_tree_size
    then
      fun target ->
      let g (i, idxs) p =
        if V.approx ~eps:radius p target then i + 1, i :: idxs else i + 1, idxs
      in
      snd @@ List.fold_left g (0, []) path
    else (
      let tree = BT.make path in
      BT.search_idxs ~radius tree )

  let nearby_points ?(min_tree_size = 400) ?(radius = Util.epsilon) path =
    if List.length path < min_tree_size
    then fun target -> List.filter (fun p -> V.approx ~eps:radius p target) path
    else (
      let tree = BT.make path in
      BT.search_points ~radius tree )

  let closest_tangent ?(closed = true) ?(offset = V.zero) ~line curve =
    match curve with
    | [] | [ _ ] -> invalid_arg "Curved path has too few points."
    | p0 :: p1 :: tl ->
      let angle_sign tangent = TS.tangent_sign line tangent in
      let f (i, min_cross, nearest_tangent, last_sign, last_tangent) p =
        let tangent = V.{ a = last_tangent.b; b = p } in
        let sign = angle_sign tangent in
        if not (Float.equal sign last_sign)
        then (
          let zero_cross = V.distance_to_line ~line (V.add last_tangent.b offset) in
          if zero_cross < min_cross
          then i + 1, zero_cross, Some (i - 1, last_tangent), sign, tangent
          else i + 1, min_cross, nearest_tangent, sign, tangent )
        else i + 1, min_cross, nearest_tangent, sign, tangent
      in
      let ((_, _, nearest_tangent, _, _) as acc) =
        let tangent = V.{ a = p0; b = p1 } in
        List.fold_left f (1, Float.max_float, None, angle_sign tangent, tangent) tl
      in
      let tangent =
        if closed
        then (
          let _, _, nearest_tangent, _, _ = f acc p0 in
          nearest_tangent )
        else nearest_tangent
      in
      ( match tangent with
      | Some tangent -> tangent
      | None -> failwith "No appropriate tangent points found." )
end
