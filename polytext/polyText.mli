(** Point path representations (using {{!OCADml.Poly2.t} [Poly2.t]}) of text via
   {{:https://github.com/Chris00/ocaml-cairo} ocaml-cairo} import of system
    fonts. {b NOTE:} Still somewhat experimental.

    Ideally, the point representations obtained from fonts here should make it
    simple to produce rounded text extrusions. Unfortunately, the story there is
    somewhat fickle at the moment due to diffculty in applying
    roundovers/offsets to the paths produced, in part due to the tight point
    spacing and sharp corners that they can have. Thus, this module may evolve
    to make use of OpenType variable fonts to produce the offset paths required
    (via variable weight). *)

val text
  :  ?fn:int
  -> ?center:bool
  -> ?slant:Cairo.slant
  -> ?weight:Cairo.weight
  -> ?size:float
  -> font:string
  -> string
  -> OCADml.Poly2.t list
