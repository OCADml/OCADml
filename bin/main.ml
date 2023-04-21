open! OCADml

let () = print_endline "hello"

let profiles =
  let fn = 32
  and up h = Path3.translate (v3 0. 0. h) in
  let base =
    let sq = Path3.square ~center:true (v2 2. 4.) in
    Path3.(roundover ~fn (Round.flat ~corner:(Round.circ (`Radius 0.5)) sq))
  and c r h = up h @@ Path3.circle ~fn r in
  let cones = List.map (fun h -> [ c 0.6 h; c 0.5 (h +. 1.) ]) [ 4.; 5.; 6. ] in
  List.flatten @@ ([ base; up 2. base; c 0.5 3.; c 0.5 4. ] :: cones)

(* let () = Mesh.to_stl "skline_test.stl" @@ Mesh.skin ~slices:(`Flat 0) profiles *)
let () =
  Mesh.to_stl "skline_test.stl" @@ Mesh.skline ~fn:100 ~size:(`Flat (`Rel 0.05)) profiles

let () =
  let circ = Path3.circle ~fn:32 5.
  and pent = Path3.circle ~fn:5 5. in
  let profs =
    Path3.
      [ pent
      ; ztrans 6. pent
      ; translate (v3 15. 0. 20.) (yrot (Float.pi /. 2.) circ)
      ; List.rev @@ translate (v3 30. 0. 6.) (zrot Float.pi pent)
      ; List.rev @@ xtrans 30. (zrot Float.pi pent)
      ]
  in
  Mesh.to_stl "skline_test2.stl"
  @@ Mesh.skline
       ~mapping:(`Flat (`Reindex `ByLen))
       ~fn:100
       ~size:(`Flat (`Rel 0.05))
       profs
