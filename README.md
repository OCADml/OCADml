# OCADml

OCADml is a collection of types and functions facilitating Computer Aided
Design (CAD) in OCaml. In particular, the style of design currently supported is
that of 2D drawing and mesh generation via sweeping/extrusion.

## Documentation

Documentation is available
[online](https://ocadml.github.io/OCADml/OCADml/index.html), covering the
[API](https://ocadml.github.io/OCADml/OCADml/index.html#api). As this library
is not associated with any CAD backend, it is a bit dry on it's own and replete
of examples. To get a better idea of what can be done with the tools provided it
may be helpful to check out a library utilizing them.

## CAD Package Specific Frontends

This library only provides a means to generate point and face based geometries,
in order to perform boolean operations, validation, and export, you'll need to
work with a particular CAD package.

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

## Inspiration

This effort began as an extension of the
[Scad_ml](https://github.com/namachan10777/scad-ml) OpenSCAD DSL, but grew to
the point where it seemed prudent to break out into a "backend" agnostic format
so as to be more generally useful.

Module organization and implementations borrow heavily
from existing libraries in the OpenSCAD community:
- [BOSL2](https://github.com/revarbat/BOSL2),
- [dotSCAD](https://github.com/JustinSDK/dotSCAD/tree/master/src)
- [PlotFunction](https://github.com/rcolyer/plot-function)
