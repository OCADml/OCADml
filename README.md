# OCADml

OCADml is a collection of types and functions facilitating Computer Aided
Design (CAD) in OCaml.

## Documentation

Documentation is available
[online](https://ocadml.github.io/OCADml/OCADml/index.html), covering the
[API](https://ocadml.github.io/OCADml/OCADml/index.html#api). As this library
is not associated with any CAD backend, it is a bit dry on it's own and replete
of examples. To get a better idea of what can be done with the tools provided it
may be helpful to check out a library utilizing them.

## CAD Frontends

### OSCADml
- [github](https://github.io/OSCADml)
- [docs](https://ocadml.github.io/OSCADml/OSCADml/index.html)
- [examples](https://ocadml.github.io/OSCADml/OSCADml/index.html#examples)

## Companion PPX

There is a companion ppx, [\[@@deriving
cad\]](https://github.com/OCADml/ppx_deriving_cad) for generating
transformation functions for user-defined records and abstract types made up of
CAD specific types such as [OSCADml](https://github.com/OSCADml)'s `Scad.t`,
along with their corresponding vector (`V2.t` or `V3.t`) types (and those
composed of them) provided in this library.
