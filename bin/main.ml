open! OCADml

let profiles =
  let fn = 64
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
      ; translate (v3 12.5 0. 20.) (yrot (Float.pi /. 2.) circ)
      ; translate (v3 17.5 0. 20.) (yrot (Float.pi /. 2.) circ)
      ; translate (v3 30. 0. 6.) (yrot Float.pi pent)
      ; xtrans 30. (yrot Float.pi pent)
      ]
  in
  Mesh.to_stl "skline_test2.stl" @@ Mesh.skline ~fn:200 ~size:(`Flat (`Rel 0.05)) profs

let () =
  let circ = Path3.circle ~fn:64 5. in
  let base = Path3.scale (v3 1.2 1.2 1.) circ
  and handle = Path3.scale (v3 0.7 0.7 1.) circ
  and up = v3 0. 0. 1. in
  let profs =
    Path3.
      [ ztrans (-3.) base
      ; circ
      ; translate (v3 15. 0. 20.) (yrot (Float.pi /. 2.) handle)
      ; xtrans 30. (yrot Float.pi circ)
      ; translate (v3 30. 0. (-3.)) (yrot Float.pi base)
      ]
  and tangents = `Tangents [ up; up; v3 1. 0. 0.; V3.neg up; V3.neg up ] in
  Mesh.to_stl "skline_test3.stl"
  @@ Mesh.skline ~fn:200 ~size:(`Flat (`Rel 0.5)) ~tangents profs

let () =
  let circ = Path3.circle ~fn:64 5. in
  let pent = Path3.(circle ~fn:5 5.) in
  let profs =
    Path3.
      [ circ
      ; translate (v3 15. 0. 20.) (yrot (Float.pi /. 2.) pent)
      ; xtrans 30. (yrot Float.pi circ)
      ; translate (v3 15. 0. (-20.)) (yrot (Float.pi *. 1.5) pent)
      ]
  in
  Mesh.to_stl "skline_test4.stl"
  @@ Mesh.skline ~endcaps:`Loop ~fn:200 ~size:(`Flat (`Rel 0.1)) profs
