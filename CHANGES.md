## v0.4.1

- fix `V2.mirror` (xy were inverted) and clarify mirror doc comments
- preserve path/poly winding when mirrored

## v0.4.0

- make cairo2 a depopt and break `PolyText` into optional sub-library
 (now free of non-OCaml dependencies)
- fix bug in `Path2.point_inside` (always returning outside)
- abstract `Poly{2,3}.t` (from private) free up for potential implementation change
- add missing `{of,to}_{seq,list,array}` to `Path` and `Poly` modules for
 convenience

## v0.3.2
- add `of_list` and `to_list` convenience functions to `Poly2` and `Poly3`

## v0.3.1

- v0.3.0 being marked unavailable on opam due to bugs with Path{2,3}.bbox
- add dune to package dependencies

## v0.3.0

- add gg library dependency
- replaced vector (V{2,3,4}.t), matrix (Affine{2,3}), and bounding box types
 with those from gg
- protect bezier memoization with mutex (OCaml 5 compatibility)

## v0.2.2

- fix correctness bug in `Path.noncollinear_triple`

## v0.2.1

- Add `Mesh.to_stl` (serialization to bin/ascii stl format)

## v0.2.0
- Use v4 type for `Plane.t` abstract it and add type conversions
- `Mesh.extrude` altered to return mid-sectionless shape when `~height` is less
  than the combined height of `~caps` (rather than breaking with `nans`)
- Use `Mesh.Cap.t` and sub-variants more consistently for `~caps` specification in
  sweeping functions of `Mesh` (looping restricted using sub-types)
- Add `Mesh.revolve`

## v0.1.2
- Add v4 (4d vector) type at top level (use abstracted in `Quaternion`)

## v0.1.1
- Add `Mesh.drop_unused_points`
- Remove `Mesh.add_face` and `Mesh.add_faces`
- Abstract `Mesh.t` type (free up for potential impl changes)
- Add `Path2.triangulate` and `Mesh.triangulate`

## v0.1.0

- Initial opam release of the OCADml library
