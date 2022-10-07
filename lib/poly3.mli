(** Planar 3d polygons made up of an outer, and zero or more inner {!Path3.t}s.
   Includes basic shape creation helpers, manipulations, (including offset and
   basic transformations), measurement, and validation. *)

(** 3-dimensional (planar) polygon

    This type is kept private to force use of {!make}, which ensures that all
    points of the polygon are planar and performs {!Poly2.validation} by default,
    hopefully providing an early warning that the shape may have issues rendering
    in a CAD backend. *)
type t = private
  { outer : Path3.t
  ; holes : Path3.t list
  }

(** {1 Creation and 2d-3d conversion} *)

(** [make ?validate ?holes outer]

    Create a 3d polygon from an [outer] path (perimeter), and zero or more
    [holes] (default = [[]]). If validate is [true] (as it is by default), all
    paths are checked for coplanarity, then {!val:Poly2.validation} is performed
    on a 2d projection, raising exceptions if the defined polygon is not simple
    (and thus, may cause problems in CGAL). *)
val make : ?validate:bool -> ?holes:Path3.t list -> Path3.t -> t

(** [add_holes ?validate ~holes t]

    Add [holes] to [t]. If validate is [true] (as it is by default),
    {!val:Poly2.validation} is performed, raising exceptions if the defined
    polygon is not simple (and thus, may cause problems in CGAL). *)
val add_holes : ?validate:bool -> holes:V3.t list list -> t -> t

(** [of_poly2 ?plane poly]

    Lift the 2d polygon [poly] onto [plane] (default = {!val:Plane.xy}). *)
val of_poly2 : ?plane:Plane.t -> Poly2.t -> t

(** [to_poly2 ?validate ?plane t]

    Project the 3d polygon [t] onto [plane] (default = {!Plane.xy}). If
    [validate] is [true], 2d polygon {!Poly2.validation} is performed (default =
    [true]). *)
val to_poly2 : ?validate:bool -> ?plane:Plane.t -> t -> Poly2.t

(** {1 Basic Shapes} *)

(** [circle ?fn ?fa ?fs ?plane r]

    Create a circle of radius [r], on the optionally provided [plane]
    (default = {!val:Plane.xy}). *)
val circle : ?fn:int -> ?fa:float -> ?fs:float -> ?plane:Plane.t -> float -> t

(** [wedge ?fn ?fa ?fs ?plane ~centre ~radius ~start a]

    Create an arcing path (as in {!val:Path3.arc}), with the [centre] point
    included to close the path, forming a wedge. The polygon is drawn on the
    provided [plane] (default = {!val:Plane.xy}). *)
val wedge
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?plane:Plane.t
  -> centre:V3.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [square ?center ?plane dims]

    Create a rectangular polygon with xy [dims] (e.g. width and height). If
    [center] is [true] then the path will be centred around the origin
    (default = [false]). The polygon is drawn on the provided [plane]
    (default = {!val:Plane.xy}). *)
val square : ?center:bool -> ?plane:Plane.t -> V2.t -> t

(** [ellipse ?fn ?fa ?fs ?plane radii]

    Draw an ellipse with xy [radii] onto [plane] (default = {!Plane.xy}). The
    greater of the two radii is used for fragment/resolution calculation. *)
val ellipse : ?fn:int -> ?fa:float -> ?fs:float -> ?plane:Plane.t -> V2.t -> t

(** [star ?plane ~r1 ~r2 n]

    Draw an [n] pointed star with inner radius [r1] and outer radius [r2] onto
    [plane] (default = {!Plane.xy}). *)
val star : ?plane:Plane.t -> r1:float -> r2:float -> int -> t

(** [ring ?fn ?fa ?fs ?plane ~thickness radii]

    Draw an empty elliptical ring of outer xy [radii], with the given radial
    [thickness] (difference between outer and inner radii) on the provided
    [plane] (default = {!val:Plane.xy}). For a circular ring, use equal x and y
    radii. *)
val ring
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?plane:Plane.t
  -> thickness:V2.t
  -> V2.t
  -> t

(** [box ?center ?plane ~thickness dims]

    Create a rectangular empty box of outer xy dimensions [dims], with the given
    xy [thickness] (difference between outer and inner xy dimensions). If
    [center] is [true], then the path will be centred around the origin
    (default = [false]). The polygon is drawn on the provided [plane]
    (default = {!val:Plane.xy}). *)
val box : ?center:bool -> ?plane:Plane.t -> thickness:V2.t -> V2.t -> t

(** {1 Geometry} *)

(** [bbox t]

    Compute the 3d bounding box of the polygon [t]. *)
val bbox : t -> V3.bbox

(** [centroid ?eps t]

    Compute the centroid of the outer path of the polygon [t]. If [t.outer] is
    collinear or self-intersecting (within [eps] tolerance), an
    [Invalid_argument] exception is raised. *)
val centroid : ?eps:float -> t -> V3.t

(** [area ?signed t]

    Compute the total area of the polygon [t]. If [signed] is [true], then the
    signed areas of the [outer] and [holes] paths of [t] will be summed (keep in
    mind that this is dependent on winding direction, which should generally be
    opposite between outer and inner paths), otherwise the unsigned (positive)
    area of the [holes] will be subtracted (default = [false]). *)
val area : ?signed:bool -> t -> float

(** {1 Offset and Basic Transformations} *)

(** [offset ?fn ?fs ?fa ?check_valid ?mode d t]

    Offset outer and inner paths of [t] by the specified distance [d]. The
    [mode] governs how [d] is used to create the new corners.
    - [`Delta] will create a new outline whose sides are a fixed distance [d]
      (+ve out, -ve in) from the original outline (this is the default behaviour).
    - [`Chamfer] fixed distance offset by [d] as with delta, but with corners
      chamfered.
    - [`Radius] creates a new outline as if a circle of some radius [d] is
      rotated around the exterior ([d > 0]) or interior ([d < 0]) original
      outline. [fn], [fs], and [fa] parameters govern the number of points that
      will be used for these arcs (they are ignored for delta and chamfer modes).
    - The [check_valid] default of [`Quality 1] will check the validity of
      shifted line segments by checking whether their ends and [n] additional
      points spaced throughout are far enough from the original path. If there are
      no points that have been offset by the target [d], a [Failure] exception will
      be raised. Checking can be turned off by setting this to [`No]. *)
val offset
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Delta ]
  -> float
  -> t
  -> t

(** [map f t]

    Map the outer and inner paths of [t] with the function [f]. *)
val map : (Path3.t -> Path3.t) -> t -> t

val translate : V3.t -> t -> t
val xtrans : float -> t -> t
val ytrans : float -> t -> t
val ztrans : float -> t -> t
val rotate : ?about:V3.t -> V3.t -> t -> t
val xrot : ?about:V3.t -> float -> t -> t
val yrot : ?about:V3.t -> float -> t -> t
val zrot : ?about:V3.t -> float -> t -> t
val affine : Affine3.t -> t -> t
val quaternion : ?about:V3.t -> Quaternion.t -> t -> t
val axis_rotate : ?about:V3.t -> V3.t -> float -> t -> t
val scale : V3.t -> t -> t
val mirror : V3.t -> t -> t
