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
