open Cairo
open V
module Bez = Bezier.Make (V2)

let path_to_outlines ?(fn = 5) data =
  let f (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, v2 x y
    | LINE_TO (x, y) -> paths, last_p :: ps, v2 x y
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      let bez = Bez.make' [| last_p; v2 x1 y1; v2 x2 y2; v2 x3 y3 |] in
      paths, Bez.curve ~fn ~rev:true ~endpoint:false ~init:ps bez, v2 x3 y3
    | CLOSE_PATH ->
      let path =
        match ps with
        | [] -> [ last_p ]
        | _  -> if V2.approx (Util.last_element ps) last_p then ps else last_p :: ps
      in
      path :: paths, [], last_p
  in
  let paths, _, _ = Path.fold data f ([], [], v2 0. 0.) in
  List.rev_map (List.map @@ fun V.{ x; y } -> v2 x (-.y)) paths

let text ?fn ?(center = false) ?slant ?weight ?(size = 10.) ~font txt =
  let ctxt = create (Image.create Image.A1 ~w:1 ~h:1) in
  select_font_face ?slant ?weight ctxt font;
  scale ctxt 1. 1.;
  set_font_size ctxt size;
  let te = text_extents ctxt txt
  and x_offset = 0.72 *. size /. 10. in
  if center
  then (
    let x = x_offset -. (te.width /. 2.) -. te.x_bearing
    and y = 0. -. (te.height /. 2.) -. te.y_bearing in
    move_to ctxt x y )
  else move_to ctxt (-.x_offset) 0.;
  let f acc c =
    let s = String.make 1 c in
    Path.text ctxt s;
    let acc =
      match path_to_outlines ?fn (Path.copy ctxt) with
      | []          -> acc
      | outer :: tl ->
        let rec aux polys outer holes = function
          | []                    -> Poly2.make ~holes (List.rev outer) :: polys
          | (pt :: _ as hd) :: tl ->
            ( match Path2.point_inside outer pt with
            | `Inside -> aux polys outer (hd :: holes) tl
            | _       -> aux (Poly2.make ~holes (List.rev outer) :: polys) hd [] tl )
          | _                     -> aux polys outer holes tl
        in
        aux acc outer [] tl
    in
    let x, y = Path.get_current_point ctxt in
    Path.clear ctxt;
    move_to ctxt x y;
    acc
  in
  String.fold_left f [] txt
