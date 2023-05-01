open! OCADml

let vol_eps = 1e-4

let binary_stl () =
  let cyl = Mesh.extrude ~height:2. (Poly2.circle ~fn:36 5.) in
  Mesh.to_stl "bin_cyl.stl" cyl;
  let loaded = Mesh.of_stl "bin_cyl.stl" in
  Alcotest.(check (float vol_eps)) "same volume" (Mesh.volume cyl) (Mesh.volume loaded)

let ascii_stl () =
  let cyl = Mesh.extrude ~height:2. (Poly2.square (v2 2. 2.)) in
  Mesh.to_stl ~ascii:true "ascii_cyl.stl" cyl;
  let loaded = Mesh.of_stl "ascii_cyl.stl" in
  Alcotest.(check (float vol_eps)) "same volume" (Mesh.volume cyl) (Mesh.volume loaded)

let () =
  let open Alcotest in
  run
    "Mesh"
    [ ( "serialization"
      , [ test_case "Binary stl roundtrip" `Quick binary_stl
        ; test_case "ASCII stl roundtrip" `Quick ascii_stl
        ] )
    ]
