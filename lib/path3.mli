(** 3d path generation (including arcs and basic shapes), manipulation
   (including roundovers (see {!module:Round}), and conversion to sweeping
   transformations with {!to_transforms}), and measurement. *)

(** @inline *)
include Path.S with type vec := V3.t and type line := V3.line

(** {1 Search} *)

(** [nearby_idxs ?min_tree_size ?radius path p]

    Find the indices of points within [radius] (default = [1e-9]) distance from
    the target point [p] in [path]. Match indices will be returned in arbitrary
    order (unsorted). When [path] is provided (eagerly on partial application),
    the length will be checked and a function to perform the search will be
    generated. If [path] is shorter than [min_tree_size], it will be a simple
    direct search otherwise a {!BallTree3.t} will be constructed. Thus, if you
    plan to search for more than one target point, take care to apply this
    function in two steps to avoid repeated length checks and closure/tree
    generations. *)
val nearby_idxs : ?min_tree_size:int -> ?radius:float -> V3.t list -> V3.t -> int list

(** [nearby_points ?min_tree_size ?radius path]

    Find the points within [radius] (default = [1e-9]) distance from the target
    point [p] in [path]. Matched points will be returned in arbitrary order
    (unsorted). When [path] is provided (eagerly on partial application), the
    length will be checked and a function to perform the search will be
    generated. If [path] is shorter than [min_tree_size], it will be a simple
    direct search otherwise a {!BallTree3.t} will be constructed. Thus, if you
    plan to search for more than one target point, take care to apply this
    function in two steps to avoid repeated length checks and closure/tree
    generations. *)
val nearby_points : ?min_tree_size:int -> ?radius:float -> V3.t list -> V3.t -> V3.t list

(** [closest_tangent ?closed ?offset ~line t]

    Find the tangent segment (and its index) on the curved path [t] closest to [line]
    after [offset] (default = [V3.zero]) is applied to the points of [t] (can
    be used to centre the path relative to [line] to help in choosing the
    desired tangent). *)
val closest_tangent : ?closed:bool -> ?offset:V3.t -> line:V3.line -> t -> int * V3.line

(** {1 Creation and 2d-3d Conversion} *)

(** [of_tups ps]

    Create a 3d path from a list of xyz coordinate triples. *)
val of_tups : (float * float * float) list -> t

(** [of_path2 ?plane path]

    Lift a 2d [path] onto [plane] (default = {!Plane.xy}). *)
val of_path2 : ?plane:Plane.t -> Path2.t -> t

(** [to_path2 ?plane t]

    Project the 3d path [t] onto [plane] (default = {!Plane.xy}). *)
val to_path2 : ?plane:Plane.t -> t -> Path2.t

(** [to_plane ?eps t]

    Compute the normalized cartesian equation of the plane that the path [t]
   resides on. If there are fewer than three points in [t], or they are not
   coplanar within the tolerance [eps], an [Invalid_argument] exception is
   raised. *)
val to_plane : ?eps:float -> t -> Plane.t

(** [project plane t]

    Project the 3d path [t] onto [plane]. *)
val project : Plane.t -> t -> Path2.t

(** {1 Basic Shapes} *)

(** [circle ?fn ?fa ?fs ?plane radius]

    Draw a circular path of radius [r] onto [plane] (default = {!Plane.xy}). *)
val circle : ?fn:int -> ?fa:float -> ?fs:float -> ?plane:Plane.t -> float -> t

(** [square ?center ?plane dims]

    Draw a rectangular path with xy [dims] (e.g. width and height) onto
    [plane] (default = {!Plane.xy}). If [center] is [true] then the path will be
    centred around the origin (default = [false]). *)
val square : ?center:bool -> ?plane:Plane.t -> V2.t -> t

(** [ellipse ?fn ?fa ?fs ?plane radii]

    Draw an ellipse with xy [radii] onto [plane] (default = {!Plane.xy}). The
    greater of the two radii is used for fragment/resolution calculation. *)
val ellipse : ?fn:int -> ?fa:float -> ?fs:float -> ?plane:Plane.t -> V2.t -> t

(** [star ?plane ~r1 ~r2 n]

    Draw an [n] pointed star with inner radius [r1] and outer radius [r2] onto
    [plane] (default = {!Plane.xy}). *)
val star : ?plane:Plane.t -> r1:float -> r2:float -> int -> t

(** {1 Drawing Arcs} *)

(** [arc ?rev ?fn ?fa ?fs ?plane ?wedge ~centre ~radius ~start a]

    Draw an arc onto a 3d [plane] (default = {!Plane.xy}). See {!Path2.arc}. *)
val arc
  :  ?rev:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?plane:Plane.t
  -> ?wedge:bool
  -> centre:V3.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [arc_about_centre ?rev ?fn ?fa ?fs ?dir ?wedge ~centre p1 p2]

    Draw an arc between [p1] and [p2], about [centre], on the 3d plane occupied
    by the three points. See {!Path2.arc_about_centre}. *)
val arc_about_centre
  :  ?rev:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?dir:[ `CW | `CCW ]
  -> ?wedge:bool
  -> centre:V3.t
  -> V3.t
  -> V3.t
  -> t

(** [arc_through ?rev ?fn ?fa ?fs ?wedge p1 p2 p3]

    Draw an arc through the points [p1], [p2], and [p3]. See {!Path2.arc_through}. *)
val arc_through
  :  ?rev:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?wedge:bool
  -> V3.t
  -> V3.t
  -> V3.t
  -> t

(** [helix ?fn ?fa ?fs ?left ~n_turns ~pitch ?r2 r1]

    Draw a 3d helical path around a cylinder/cone with start radius [r1] and
    end radius [r2] (default = [r1]).
    - [n_turns] sets the number of revolutions around the z-axis
    - [pitch] describes the height of one complete turn
    - [left] is used to set handedness (default = [true])
    - [fn], [fa], and [fs] parameters govern quality as they do in OpenSCAD *)
val helix
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> t

(** {1 Roundovers}

    Specification and application of circular, chamfer, and bezier (continuous
    curvature) roundovers to 3d paths.

    Based on the {{:https://github.com/revarbat/BOSL2} BOSL2}
    {{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding}
    module. *)

include Rounding.S with type vec := V3.t (** @inline *)

(** {1 Geometry} *)

(** [normal t]

   Calculate the normal vector of the path [t]. An [Invalid_argument] exception
   is raised if there are fewer than three points in [t]. *)
val normal : t -> V3.t

(** [centroid ?eps t]

    Compute the centroid of the path [t]. If [t] is collinear or
   self-intersecting (within [eps] tolerance), an [Invalid_argument] exception
   is raised. *)
val centroid : ?eps:float -> t -> V3.t

(** [area ?signed t]

  Calculate the area of the co-planar path (describing a polygon) [t]. If
  [signed] is [true], the signed area is returned. *)
val area : ?signed:bool -> t -> float

(** [coplanar ?eps t]

  Returns [true] if all points in [t] are coplanar, within the tolerance [eps].
  If there are fewer than 3 points, or the path is collinear, this returns [false]. *)
val coplanar : ?eps:float -> t -> bool

(** [bbox t]

    Compute the 3d bounding box of the path [t]. *)
val bbox : t -> V3.bbox

(** {1 Sweeping Transform Helpers} *)

(** [scaler ?ez scale]

    Create a lookup from relative position ([0.] to [1.]) to scaling
    transformation matrix for interpolating from [{x = 1.; y = 1.}] at [0.] to
    [scale] by [1.]. If provided, the pair of handle points [ez] will be used to
    ease the scaling (see {!Easing.make}). *)
val scaler : ?ez:V2.t * V2.t -> V2.t -> float -> Affine3.t

(** [twister ?ez angle]

    Create a lookup from relative position ([0.] to [1.]) to rotation
    transformation matrix for interpolating from [0.] (no rotation) at [0.] to
    [angle] by [1.]. If provided, the pair of handle points [ez] will be used to
    ease the scaling (see {!Easing.make}). *)
val twister : ?ez:V2.t * V2.t -> float -> float -> Affine3.t

(** [to_transforms ?mode ?scale_ez ?twist_ez ?scale ?twist t]

   Generate list of transformations that can be applied to three-dimensional
   vectors ({!V3.t} via {!Affine3.transform}) or shapes, to move them along the
   path [t] (intended to be applied to the vector/shape from its original
   position each time). Note that the transforms are generated assuming the
   shape is centred on the origin, and for the purposes of alignment to the
   beginning of the path and scaling/twisting, laying flat on the XY plane.

   When [mode] is [`Auto | `Align _ | `NoAlign] (default it [`Auto]), tangents
   are used to estimate appropriate rotations for each translation, using
   quaternion alignment from tangent to tangent, accumulating rotation along the
   way. In the case of [`Auto], some effort is made to automatically apply an
   alignment rotation towards the first tangent of the path before each
   transformation such that the orientation of the shape being swept will be
   consistent and sensible, though some manual rotation of the shape before sweeping
   along the generated transforms may be necessary to get the desired result.

   [`Align v] and [`NoAlign] follow the same quaternion accumulation strategy
   along the path [t], however they differ from [`Auto] in their handling of pre-alignment
   transformations. With [`Align v], the vector [v] will be used to align the
   assumed shape normal of [(v3 0. 0. 1.)] to, while [`NoAlign] will skip the
   alignment step entirely (likely desirable if the intention is to sweep a shape
   {i not} laying on the XY plane).

   Setting [mode = `Euler] will use euler rotations instead, which can
   have results more in line with expectations in some scenarios (helical-like
   paths for example, though {!Mesh.helix_extrude} may be a better fit in that
   case), but fail in others. For instance, [`Euler] can generate an abrupt when
   the path tangent is exactly vertical.

   If provided, [scale] and [twist], specify scaling and rotation to be applied
   along the path increasing up to the provided value by the end.
   Scaling/twisting proceeds linearly by default, unless the corresponding
   [_ez] parameters are provided to describe the desired eased transition (see
   {!scaler} and {!twister}). *)
val to_transforms
  :  ?mode:[ `Auto | `Align of V3.t | `NoAlign | `Euler ]
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> t
  -> Affine3.t list

(** [helical_transforms ?fn ?fs ?fa ?scale_ez ?twist_ez ?scale ?twist
     ?left ~n_turns ~pitch ?r2 r1]

    Affine transformations following a helical path. This can be thought of like
    a special case of {!to_transforms}, but using a path generated with {!helix},
    and with rotational calculations taking into account the helical
    trajectory. *)
val helical_transforms
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale_ez:V2.t * V2.t
  -> ?twist_ez:V2.t * V2.t
  -> ?scale:V2.t
  -> ?twist:float
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Affine3.t list

(** {1 Path Matching / Vertex Association}

  Point duplicating strategies for associating vertices between incommensurate
  closed polygonal paths/profiles. Primarily for use in conjunction with
  {!Mesh.skin} and {!Mesh.morphing_sweep}, where commensurate profiles are required to
  draw edges between.

  Ported from the {{:https://github.com/revarbat/BOSL2/blob/master/skin.scad}
  skin} module of the {{:https://github.com/revarbat/BOSL2} BOSL2} OpenSCAD library. *)

include PathMatch.S with type vec := V3.t (** @inline *)

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
val mirror : V3.t -> t -> t
