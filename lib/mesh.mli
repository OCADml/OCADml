(** Generation, and manipulation of 3-dimensional meshes (points and faces).

    This data type and its constructors/transformers are based on the
    the {{:https://github.com/revarbat/BOSL2/blob/master/vnf.scad} vnf
    structure module} of
    the {{:https://github.com/revarbat/BOSL2/} BOSL2 OpenSCAD library}. *)

(** Points and faces 3-dimensional mesh. *)
type t

(** {1 Basic Constructors }*)

(** An empty [t], with no points. *)
val empty : t

(** [make ~points ~faces]

    Create a mesh [t] from a list of {!V3.t} [points], and a list of [faces]
    described by indices into [points]. *)
val make : points:V3.t list -> faces:int list list -> t

(** {1 Accessors} *)

val size : t -> int
val points : t -> V3.t list
val faces : t -> int list list

(** {1 Low-level Generators} *)

(** Describes desired row wrapping behaviour in {!of_rows}, which creates a
    mesh from rows of points. *)
type endcaps =
  [ `Loop (** Last/top row wrapped to the first/bottom *)
  | `Both (** Both bottom and top rows are closed with flat faces *)
  | `None (** Neither top or bottom rows are closed with a face *)
  | `Top (** A face is generated to close the top row with itself *)
  | `Bot (** A face is generated to close the bottom row with itself *)
  ]

(** Quadrilateral face triangulation strategy. *)
type style =
  [ `Default
  | `Alt
  | `MinEdge
  | `Quincunx
  | `Convex
  | `Concave
  ]

(** [prune_rows ?min_dist rows]

    Filter [rows] such that each row polygon is at least [min_dist] (default [0.05])
    above the plane of the previous polygon, indicating the dropped indices.
    This can be useful for avoiding self-intersections in the output of {!of_rows}.
    Note that all polygons in [rows] must be planar, else [Failure] will be
    raised. *)
val prune_rows : ?min_dist:float -> Path3.t list -> int list * Path3.t list

(** [of_rows ?rev ?endcaps ?col_wrap ?style rows]

    Create a {!type:t} representing a polyhedron from a list of layers
    (counter_clockwise loops of 3d points). [endcaps] defaults to [`Both], which
    specifies that faces should be generated to close off the bottom and top
    layers of the generated shape. If it is instead set to [`Loop], the open
    faces of the first and last layers will be closed with one another. For more
    advanced usages, one or both of the endcaps can be left open, so the resulting
    meshes can be closed off by some other means.
    - [col_wrap] sets whether faces should be generated to loop between the ends
      of each row.
    - If [rev] is [true], faces winding direction will be reversed (default = [false])
    - [style] governs how the quadrilaterals formed by the rows and columns of
      points are divided into triangles:
    {ul
        {- [`Default] is an arbitrary systematic subdivision in the same direction}
        {- [`Alt] is the uniform subdivision in the other (alternate direction)}
        {- [`MinEdge] picks the shorter edge to subdivide the quadrilateral, so
           the division may not be uniform across the shape}
        {- [`Quincunx] adds a vertex in the middle of each quadrilateral and
           creates four triangles}
        {- [`Convex] and [`Concave] choose the locally convex/concave subdivision}}
    - If [rows] is empty, a {!empty} is returned. Throws [Invalid_argument] if
    [rows] contains only one row, or if it is not rectangular (any row differs in
    length). *)
val of_rows
  :  ?rev:bool
  -> ?endcaps:endcaps
  -> ?col_wrap:bool
  -> ?style:style
  -> V3.t list list
  -> t

(** [of_ragged ?looped ?reverse rows]

    Create a triangular mesh from a list of rows, where each row can differ in
    length relative to its neighbours by up to 2. Since the rows can be ragged,
    no (columnar) wrapping is done, thus they are best described as rows, rather
    than layers as with {!of_rows} which produces an enclosed polyhedron.
    Instead, this function is useful for the generation of triangular patches
    that can be joined with one another to create a complete polyhedron. Setting
    [looped] to true will generate faces between the last and first rows, so long
    as their lengths differ by no more than 2. Face winding order is reversed if
    [reverse] is [true]. Throws [Invalid_argument] if a row length delta of
    greater than 2 is encountered. *)
val of_ragged : ?looped:bool -> ?rev:bool -> V3.t list list -> t

(** [of_path3 ?rev layer]

    Create a mesh from a single path (a closed loop of {!V2.t}), returning a
    {!type:t} with a single face including all of the points. Face winding order
    is reversed if [rev] is [true]. This can be useful for producing a flat
    patch mesh to be combined with other meshes to produce a complete shape. *)
val of_path2 : ?rev:bool -> Path2.t -> t

(** [of_path3 ?rev layer]

    Create a mesh from a single path (a closed loop of {!V3.t}, should be
   coplanar though it is not confirmed), returning a {!type:t} with a single
   face including all of the points. Face winding order is reversed if [rev] is
   [true]. This can be useful for producing a flat patch mesh to be combined
   with other meshes to produce a complete shape. *)
val of_path3 : ?rev:bool -> Path3.t -> t

(** [of_poly2 ?rev poly]

    Create a mesh from a 2d polygon. If [poly] does not have any holes, then
    this is equivalent to {!of_path2}. If there are holes, polyhole
    partitioning is performed to determine a set of faces that can close the
    points.

    The earcutting algorithm used to partition the polygon into faces is a port
    of RonaldoCMP's work found
    {{:https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad}
    here}. *)
val of_poly2 : ?rev:bool -> Poly2.t -> t

(** [of_poly3 ?rev poly]

    Create a mesh from a 3d polygon. If [poly] does not have any holes, then
    this is equivalent to {!of_path3}. If there are holes, polyhole
    partitioning is performed to determine a set of faces that can close the
    points.

    The earcutting algorithm used to partition the polygon into faces is a port
    of RonaldoCMP's work found
    {{:https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad}
    here}. *)
val of_poly3 : ?rev:bool -> Poly3.t -> t

(** [of_polygons polys]

    Create a polyhedron mesh from a list of polygonal point faces. *)
val of_polygons : Path3.t list -> t

(** [hull points]

    Create a convex hull mesh that encloses [points]. If the points are
    coplanar, a 2-dimensional hull is found, resulting in an unclosed single
    face mesh (as with {!of_path3}). Unused points are not discarded. *)
val hull : Path3.t -> t

(** {1 Skins}

    Functions for generating meshes which cover over a sequence of closed
    polygonal {!Path3.t} profiles. Ported from the
    {{:https://github.com/revarbat/BOSL2/blob/master/skin.scad} skin} module of
    the {{:https://github.com/revarbat/BOSL2} BOSL2} OpenSCAD library. *)

(** Path resampling vertex mapping strategies.

    Each of these variants specify that profiles of incommensurate length should
    simply resampled with {!Path3.subdivide} with the provided point distribution
    frequency strategy ([[`ByLen | `BySeg]]). In the case of [`Direct _], the
    profiles are assumed to be "lined up", with the points at their zeroth indices
    corresponding to eachother. The [`Reindex _] strategy will rotate the
    second profile of a pair via {!Path3.reindex_polygon} following resampling
    to minimize the distance between the zeroth indices of the two paths. *)
type resampler =
  [ `Direct of [ `ByLen | `BySeg ]
  | `Reindex of [ `ByLen | `BySeg ]
  ]

(** Point duplicating vertex mapping strategies.

    Each of these variants specify profiles of incommensurate length should be
    matched up by computing vertex mappings between the profiles, and
    duplicating vertices on the smaller/shorter profile until the point counts are
    equalized. See the conspicuously named vertex matching functions
    {!Path3.distance_match}, {!Path3.aligned_distance_match}, and
    {!Path3.tangent_match} for more details (also available in the {!Path2}
    module). *)
type duplicator =
  [ `Distance
    (** Minimize the length of the edges between associated vertices. Best
        results when connecting discrete polygon profiles with low point counts. *)
  | `FastDistance
    (** Like [`Distance], but profiles are assumed to already be
          lined up, with their zeroth indices corresponding to one another. *)
  | `Tangent
    (** Split finely sampled (convex) curve into groups of points, and
          map each group to point on the smaller discrete polygon. Can fail if
          the larger curved path is non-convex, or does not have enough points. *)
  ]

(** Vertex count matching strategy specification type. *)
type mapping =
  [ resampler
  | duplicator
  ]

(** [slice_profiles ?looped ~slices profiles]

    Linearly transition between each neighbouring pair of closed paths in
    [profiles] to produce new interpolated list of profiles. The number of
    [slices] inserted between can either be the same between each pair
    ([`Flat n]), or specified separately with [`Mix ns]. If [looped] is [true],
    then slices will also be inserted between the last and initial profiles
    (default is [false]). Lists of profiles such as these can be used to produce
    meshes with {!of_rows} (as {!skin} does).

    Raises [Invalid_argument] if the length of [`Mix ns] does not correspond to
    the number of transitions, or if [profiles] has fewer than two elements. *)
val slice_profiles
  :  ?looped:bool
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Path3.t list

(** [skin ?style ?endcaps ?refine ?mapping ~slices profiles]

    Produce a mesh that skins over two or more 3d [profiles] -- closed, ideally
    coplanar (though some slight variation can be ok) paths. This works by
    linearly interpolating between neighbouring profiles with [slices] steps,
    and passing the profiles along to {!of_rows}, which generates faces to
    enclose the shape. For this to be well defined, each row must have the same
    length, thus [mapping] can be used to specify the strategy used to
    map/associate the vertices between them and reconcile the point counts and
    improve alignment for their connecting edges (see {!resampler} and
    {!duplicator} configuration variants). By default this is [`Direct], which
    simply applies resampling without altering the vertex associations
    established by the start indices of each profile.

    - [refine] can be specified to apply additional upsampling which may help
      to improve the smoothness of the resulting mesh. Uses {!Path3.subdivide}
      with the sampling frequency indicated for {!resampler} mapped transitions,
      and [`BySeg] for {!duplicator}s.
    - [slices] and [mapping] can be provided as [`Flat _] to be applied to all
      transitions, or as [`Mix l], where [l] is a list with length equal to the
      number of profile transitions ([length profiles - 1], or [length profiles]
      if [endcaps] is [`Loop])
    - {b NOTE:} mixing mapping strategies can be fickle, and some combinations may
      not work depending on the profiles. This may improve as kinks are worked
      out, but maybe not *)
val skin
  :  ?style:style
  -> ?endcaps:endcaps
  -> ?refine:int
  -> ?mapping:[ `Flat of mapping | `Mix of mapping list ]
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> t

(** [skin_between ?style ?endcaps ?refine ?mapping ~slices a b]

    Create a mesh that {i skins} over a linear interpolation/morph  between 3d
    profiles [a] and [b] over [slices] steps. See {!skin} for more details. *)
val skin_between
  :  ?style:style
  -> ?endcaps:[ `Both | `None | `Top | `Bot ]
  -> ?refine:int
  -> ?mapping:mapping
  -> slices:int
  -> Path3.t
  -> Path3.t
  -> t

(** {1 Sweeps, extrusions, and morphs with roundovers}

Sweeps, extrusions and morphs from 2d to 3d. Each of which can be given rounded
over end caps via their optional [?caps] parameters with specifications
contsructed by the {!Cap} module. and the optional [?caps]. Roundovers are based on the
implementations found in the {{:https://github.com/revarbat/BOSL2} BOSL2}
library's [offset_sweep] functions from the
{{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding} module. *)

module Cap : sig
  (** Configuration module for declaring how extrusions from 2d to 3d via
    {!sweep} should be capped off. *)

  (** Offset diameter [d] (positive or negative), and corresponding vertical
    step [z] (enforced positive only when consumed). *)
  type offset =
    { d : float
    ; z : float
    }

  type offset_mode =
    | Delta
    | Chamfer
    | Radius of
        { fn : int option
        ; fs : float option
        ; fa : float option
        }

  (** A list of {!offset} describing a 3d end-cap extrusion roundover.
    Abstracted to enforce use of constructors. *)
  type offsets

  (** Specifies how holes in the end-cap should be treated, either relative to
    the outer shape, or independantly. When multiple holes are present, [`Mix]
    allows each one to be specified separately, to treat all the same, use the
    other variants directly. *)
  type holes =
    [ `Same (** Offset [d] and [z] values from outer are copied. *)
    | `Flip
      (** Offset [d] and [z] values from outer are copied, but [d] sign is flipped. *)
    | `Spec of offsets
      (** Supplies a different set of offsets for the holes. Note that this
    should have the same [z] values, or else the roundover mesh will not be well
    formed. *)
    | `Mix of [ `Same | `Flip | `Spec of offsets ] list
    ]

  type poly =
    { outer : offsets
    ; holes : holes
    ; mode : offset_mode
    }

  (** Specifies whether an end of the extrusion should be left [`Empty], sealed
    with a [`Flat] face, or [`Round]ed over with a given offset specification. *)
  type poly_spec =
    [ `Empty
    | `Flat
    | `Round of poly
    ]

  type caps =
    { top : poly_spec
    ; bot : poly_spec
    }

  (** Top-level configuration type for {!sweep}, allowing for the end-caps
   to be specified using the types above, or to simply loop the first and last
   layers of the mesh together (see: {!type:endcaps} [`Loop] as used by
   {!of_rows}). *)
  type t =
    [ `Looped
    | `Caps of caps
    ]

  (** [chamf ?angle ?cut ?width ?height ()]

       Create offsets that will produce a chamfer roundover. One of [cut]
   (amount of corner "cut off"), [width] (horizontal distance of chamfer), or
   [height] (vertical distance of chamfer) can be specified to be used in
   conjunction with [angle] (default = pi / 4), or [width] and [height] can be
   provided together, in which case [angle] is not used. *)
  val chamf
    :  ?angle:float
    -> ?cut:float
    -> ?width:float
    -> ?height:float
    -> unit
    -> offsets

  (** [circ ?fn roundover]

      Create offsets that will produce a circular roundover, according to
    either a given [`Cut] distance, or a [`Radius], over [fn] steps (default 16). *)
  val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets

  (** [tear ?fn roundover]

      Create offsets that will produce a teardrop rounover (circular, endind in
    a chamfer). *)
  val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets

  (** [bez ?curv ?fn spec]

      Create offsets that will produce a smooth bezier roundover. The amplitude
    of the curve can be given by [`Cut] (amount of corner "cut off"), or
    [`Joint] (distance vertically the bezier covers). [curv] is the curvature
    smoothness parameter, ranging from gradual [0.], to abrupt [1.] (default [0.5]). *)
  val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> offsets

  (** [offsets offsets]

      Sanitize (enforce positive and increasing [z] values) and pack a list of
      {!type:offset}. *)
  val offsets : offset list -> offsets

  (** [unwrap_offsets offsets]

      Retrieve list of {!type:offset} from the abstract {!type:offsets}. *)
  val unwrap_offsets : offsets -> offset list

  (** [round ?mode ?holes offsets]

      Construct a roundover {!type:poly_spec}. [mode] specifies the kind offset
      (see {!val:Path2.offset}) performed on the paths on each "vertical" step of
      the roundover. Roundover behaviour for inner paths are specified with
      [holes]. This defaults to [`Flip], as for the more common positive
      roundovers (flaring inward), using [`Same] polarity would lead to
      "pinching off" of the holes. *)
  val round : ?mode:offset_mode -> ?holes:holes -> offsets -> [> `Round of poly ]

  (** [looped]

      [`Looped], indicating that the top should loop around to the bottom of
      a sweep. *)
  val looped : t

  (** [capped ~top ~bot]

      Construct a {!Cap.t} specifying how the [top] and [bot] caps of a sweep
      extrusion should be sealed off (or not). *)
  val capped : top:poly_spec -> bot:poly_spec -> t

  (** [flat_caps]

      Default {!Cap.t} configuration for flat (no roundover) sealed caps. *)
  val flat_caps : t

  (** [open_caps]

      Default {!Cap.t} configuration for unsealed caps (open ends). *)
  val open_caps : t
end

(** {2 Fixed polygon sweeps and extrusions} *)

(** [sweep ?check_valid ?style ?winding ?merge ?fn ?fs ?fa ?caps ~transforms poly]

    Sweep a 2d polygon into a 3d mesh by applying a sequence of [transforms] to
    the original shape. The [winding] parameter can be used to set automatic
    enforcement of polygon winding direction, which will impact the winding of
    the generated faces of the mesh. What is done with the endcaps can be
    specified with [caps]. By default the ends of the extrusion are sealed with
    flat faces, but they can instead be looped to eachother, left empty, or
    rounded over. If [style] is provided, it will be passed along to {!of_rows},
    which handles converting the swept shapes into a mesh.

    If [merge] is [true] (as is default), {!merge_points} is
    applied to the resulting mesh, as duplicate points are introduced when end
    caps are joined to the outer and inner meshes. If the duplicate points aren't
    a problem for you (they aren't {i necessarily}), this can be turned off to
    save some compute.

    [check_valid] determines whether validity checks are performed during
    offset operations (see {!Path2.offset}), for cap roundovers (if
    specified). Additionally, unless [check_valid] is [`No], polygon validation
    will be performed with final outer and inner paths of the caps before their
    mesh is generated. *)
val sweep
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?caps:Cap.t
  -> transforms:Affine3.t list
  -> Poly2.t
  -> t

(** [extrude ~height poly]

    Vertically extrude a 2d polygon from the XY plane to [height]. If [?center]
    is true, the resulting 3D object is centered around the XY plane, rather than
    resting on top of it. Roundovers described by [caps] are taken into account such
    that the final shape conforms to the specified [height]. If [height] is less
    than the combined height of [caps], there will simply be no "mid-section"
    (and the resulting height will not reflect the [height] parameter).
    - [?twist] rotates the shape by the specified angle as it is extruded
      upwards
    - [?slices] specifies the number of intermediate points along the Z axis of
      the extrusion. By default this increases with the value of [?twist],
      though manual refinement my improve results.
    - [?scale] expands or contracts the shape in X and Y as it is extruded
      upward. Default is [(v2 1. 1.)], no scaling.
    - Scaling/twisting proceed linearly by default, though bezier easing can be
      specified by providing handle points to the [scale_ez] and [twist_ez]
      parameter respectively. (see {!Path3.scaler} and {!Path3.twister}). *)
val extrude
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fa:float
  -> ?slices:int
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> ?center:bool
  -> ?caps:Cap.caps
  -> height:float
  -> Poly2.t
  -> t

(** [path_extrude ?check_valid ?style ?merge ?winding ?caps ?euler
     ?scale_ez ?twist_ez ?scale ?twist ~path poly]

    Extrude a 2d polygon along the given [path] into a 3d mesh. This is a
    convenience function that composes transform generation using
    {!Path3.to_transforms} with {!sweep}. *)
val path_extrude
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?caps:Cap.t
  -> ?euler:bool
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> path:Path3.t
  -> Poly2.t
  -> t

(** [helix_extrude ?check_valid ?style ?merge ?fn ?fs ?fa ?scale_ez ?twist_ez ?scale ?twist
     ?caps ?left ~n_turns ~pitch ?r2 r1 poly]

    Helical extrusion of a 2d polygon into a 3d mesh. This is a special case of
    {!path_extrude}, but following a path generated with {!Path3.helix}, and
    using transforms that take the helical rotation into account. *)
val helix_extrude
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> ?caps:Cap.caps
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Poly2.t
  -> t

(** {2 Morphing sweeps and extrusions}

    These functions serve as the morphing counterparts of the fixed polygon
    sweeping functions above. In contrast to the more general {!skin} which
    transitions between 3d {!Path3.t} profiles in sequence, these restrict the
    bounding shapes to 2d, and lift to 3d via the provided transforms, or path
    specifications. This separation of the morphing transition and spatial
    transformations allows for the easy addition of non-linear {!Easing} between
    the shapes via the [?ez] parameters (default is linear transition along the
    spatial distance covered by the sweep beginning from its origin). *)

(** [morphing_sweep ~transforms a b]

    Morph between the polygons [a] and [b] while sweeping the hybrids along
    [transforms] to create a mesh. The [outer_map], [hole_map], and [refine]
    correspond to the the similarly named parameters of the more general
    {!skin}, while the optional [ez] parameter allows the transition to be
    bezier eased via {!Easing.make}, rather than strictly linearly. See {!sweep}
    for details on the remaining common parameters. *)
val morphing_sweep
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?caps:Cap.caps
  -> ?outer_map:mapping
  -> ?hole_map:[ `Same | `Flat of mapping | `Mix of mapping list ]
  -> ?refine:int
  -> ?ez:V2.t * V2.t
  -> transforms:Affine3.t list
  -> Poly2.t
  -> Poly2.t
  -> t

(** [morph ~height a b]

    Vertically morph between the 2d polygons [a] and [b]. This function is to
    {!morphing_sweep}, as {!extrude} is to {!sweep}. See each of the former for
    details on their common parameters.  *)
val morph
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW ]
  -> ?fa:float
  -> ?slices:int
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> ?center:bool
  -> ?caps:Cap.caps
  -> ?outer_map:mapping
  -> ?hole_map:[ `Flat of mapping | `Mix of mapping list | `Same ]
  -> ?refine:int
  -> ?ez:V2.t * V2.t
  -> height:float
  -> Poly2.t
  -> Poly2.t
  -> t

(** [path_morph ~path poly]

    Morph between the 2d polygons [a] and [b] along the given [path]. This is a
    convenience function that composes transform generation using
    {!Path3.to_transforms} with {!morphing_sweep}.  *)
val path_morph
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?caps:Cap.caps
  -> ?outer_map:mapping
  -> ?hole_map:[ `Flat of mapping | `Mix of mapping list | `Same ]
  -> ?refine:int
  -> ?ez:V2.t * V2.t
  -> ?euler:bool
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> path:Path3.t
  -> Poly2.t
  -> Poly2.t
  -> t

(** [helix_morph ~n_turns ~pitch ?r2 r1 a b]

    Morph between the 2d polygons [a] and [b] along a helical path. This is a
    special case of {!path_morph}, but following a path generated with
    {!Path3.helix}, and using transforms that take the helical rotation into
    account. *)
val helix_morph
  :  ?style:style
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?merge:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> ?caps:Cap.caps
  -> ?outer_map:mapping
  -> ?hole_map:[ `Flat of mapping | `Mix of mapping list | `Same ]
  -> ?refine:int
  -> ?ez:V2.t * V2.t
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Poly2.t
  -> Poly2.t
  -> t

(** {1 Generalized prisms with continous rounding} *)

module Prism : sig
  (** Rounded prism configuration module. *)

  (** Rounded prism joint and curvature specification.

   In general, [joint_] parameters  are pairs determine the distance away from
   the edge that curvature begins, and [k] parameters set the smoothness of the
   curvature. *)
  type spec =
    { k : float (** default curvature used if specific curvatures are [None] *)
    ; k_bot : float option (** curvature smoothness of bottom edges *)
    ; k_top : float option (** curvature smoothness of bottom top *)
    ; k_sides : [ `Flat of float | `Mix of float list ] option
          (** smoothness to be applied flatly to all side edges, or a list
                 specifying a smoothness for each edge (must be same length as
                 corresponding paths) *)
    ; joint_bot : float * float
          (** pair of inwards (into bottom face) and upwards (towards top)
                joint distances  *)
    ; joint_top : float * float
          (** pair of inwards (into top face) and downwards (towards bottom)
                joint distances  *)
    ; joint_sides : [ `Flat of float * float | `Mix of (float * float) list ]
          (** pair of backwards and forwards joint distances to be applied
                flatly to all side edges, or a list specifying joints for each
                edge (must be same length as corresponding paths) *)
    }

  (** Specifies how holes in the prism should be treated, either relative to
    the outer shape, or independantly. When multiple holes are present, [`Mix]
    allows each one to be specified separately, to treat all the same, use the
    other variants directly. Defaults to [`Flip] for the {!prism} and
    {!linear_prism} functions. *)
  type holes =
    [ `Same (** the outer path {!type:spec} should be used for the holes *)
    | `Flip
      (** the outer path {!type:spec} should be used for the holes, but
             with the top and bottom inward joint directions flipped (see {!flip}) *)
    | `Spec of spec (** one {!type:spec} to use for all holes *)
    | `Mix of [ `Spec of spec | `Flip | `Same ] list
    ]

  (** [flip spec]

      Negate the top and bottom inwards joints (firsts of the [joint_bot] and
      [joint_top] pairs) of [caps]. These values govern whether the roundover
      flare inwards (positive when shape is CCW) or outwards (negative when shape
      is CCW). Since holes (inner paths) have reverse winding compared the outer
      path, you'll often want to use opposite polarity inward joints. *)
  val flip : spec -> spec

  (** [spec ?k ?k_bot ?k_top ?k_sides ?joint_bot ?joint_top ?joint_sides ()]

       Construct a {!type:spec} with joint distances set to [0.] by default (no
       rounding), and a default curvature smoothess [k = 0.5]. *)
  val spec
    :  ?k:float
    -> ?k_bot:float
    -> ?k_top:float
    -> ?k_sides:[ `Flat of float | `Mix of float list ]
    -> ?joint_bot:float * float
    -> ?joint_top:float * float
    -> ?joint_sides:[ `Flat of float * float | `Mix of (float * float) list ]
    -> unit
    -> spec
end

(** [prism ?debug ?fn ?holes ?outer bottom top]

    Create a prism with continuous curvature rounding from the given [bottom]
    and [top] polygons. The edges running between the corresponding paths must
    produce a valid polyhedron with coplanar side faces, thus the top should
    generally be the same shape as the bottom translated/transformed in such a
    way as to not violate this assumption (avoid z-rotation for one). Roundover
    specifications are provided with [outer] and [holes] (see {!type:Prism.spec}
    and {!type:Prism.holes} for details).

    - [debug] can be set to [true] to skip validity checks that would otherwise
    raise exceptions on failure, so a mesh can still be obtained for
    inspection. *)
val prism
  :  ?debug:bool
  -> ?fn:int
  -> ?holes:Prism.holes
  -> ?outer:Prism.spec
  -> Poly3.t
  -> Poly3.t
  -> t

(** [linear_prism ?debug ?fn ?holes ?outer ?center ~height bottom]

    Create a prism with continuous curvature rounding by extruding the polygon
    [bottom] linearaly upward to the given [height]. If [center] is [true], the
    resulting prism will be centred in z around the xy plane. See the
    more general case {!val:prism} for more details. *)
val linear_prism
  :  ?debug:bool
  -> ?fn:int
  -> ?holes:Prism.holes
  -> ?outer:Prism.spec
  -> ?center:bool
  -> height:float
  -> Poly2.t
  -> t

(** {1 Function Plotting}

    Ported from the {{:https://github.com/rcolyer/plot-function} PlotFunction}
    library by Ryan Colyer. *)

(** [cartesian_plot ~min_x ~x_steps ~max_x ~min_y ~y_steps ~max_y f]

    Create a mesh of the function [f] (from x and y to z) over the ranges of x
   and y defined by the rest of the parameters. *)
val cartesian_plot
  :  min_x:float
  -> x_steps:int
  -> max_x:float
  -> min_y:float
  -> y_steps:int
  -> max_y:float
  -> (x:float -> y:float -> float)
  -> t

(** [polar_plot ?r_step ~max_r f]

    Create a mesh of the function [f] (from radius and angle to z) between the
   z-axis and the radius [max_r], with the minimum radial step [r_step]. *)
val polar_plot : ?r_step:float -> max_r:float -> (r:float -> a:float -> float) -> t

(** [axial_plot ?fn ~min_z ~z_step ~max_z f]

    Create a mesh of the function [f] (from z-height and angle to radius). [fn]
    sets the number of angular steps around the z-axis. *)
val axial_plot
  :  ?fn:int
  -> min_z:float
  -> z_steps:int
  -> max_z:float
  -> (z:float -> a:float -> float)
  -> t

(** {1 Mesh Utilities} *)

(** [join ts]

    Join a list of meshes. This is not a boolean operation, it is simply
    collecting the points from each and updating face indices accordingly.
    Intended for use when building a closed mesh from a set of partial meshes. *)
val join : t list -> t

(** [merge_points ?eps t]

    Eliminate duplicate points (less than [eps] distance apart) from [t]. *)
val merge_points : ?eps:float -> t -> t

(** [drop_unused_points t]

    Drop unreferenced points (not included in any face) from the mesh [t]. *)
val drop_unused_points : t -> t

(** [triangulate ?eps t]

    Triangulate the faces of the mesh [t]. Some degree of coplanarity in the
    input faces can be fine, though too much  can cause triangulation to fail. If
    provided, [eps] is used for duplicate point and collinearity checks. *)
val triangulate : ?eps:float -> t -> t

(** [rev_faces t]

    Flip all faces of the mesh [t]. *)
val rev_faces : t -> t

(** [volume t]

    Calculate the volume of the mesh [t]. *)
val volume : t -> float

(** [area t]

    Calculate the surface area of the mesh [t]. *)
val area : t -> float

(** [centroid ?eps t]

    Calculate the centroid of the mesh [t]. *)
val centroid : ?eps:float -> t -> V3.t

(** {1 Basic Transfomations} *)

val translate : V3.t -> t -> t
val xtrans : float -> t -> t
val ytrans : float -> t -> t
val ztrans : float -> t -> t
val rotate : ?about:V3.t -> V3.t -> t -> t
val xrot : ?about:V3.t -> float -> t -> t
val yrot : ?about:V3.t -> float -> t -> t
val zrot : ?about:V3.t -> float -> t -> t
val quaternion : ?about:V3.t -> Quaternion.t -> t -> t
val axis_rotate : ?about:V3.t -> V3.t -> float -> t -> t
val affine : Affine3.t -> t -> t
val scale : V3.t -> t -> t
val xscale : float -> t -> t
val yscale : float -> t -> t
val zscale : float -> t -> t
val mirror : V3.t -> t -> t
