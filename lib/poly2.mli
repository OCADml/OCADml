(** 2d polygons made up of an outer, and zero or more inner {!Path2.t}s.
   Includes basic shape creation helpers, manipulations, (including offset and
   basic transformations), measurement, and validation. *)

type invalid =
  [ `SelfIntersection of int
    (** Raised during polygon validation if a path is found to self-intersect. The
          index refers to the culprit path (outer = [0], with holes increasing). *)
  | `CrossIntersection of int * int
    (** Raised during polygon validation if two paths are found to intersect
          eachother. The indices refer to the paths in question (outer =
          [0], with holes increasing). *)
  | `DuplicatePoints
    (** Raised during polygon validation if there are any duplicate points across
          all of the paths contained within {!type:t}. *)
  ]

(** Exception raised on validation failure (see {!invalid} for reasons) *)
exception InvalidPoly of invalid

(** 2-dimensional polygon

    This type is kept private to force use of {!make}, which performs
    {!validation} by default, hopefully providing an early warning that the
    shape may have issues rendering in a CAD backend. *)
type t

(** {1 Accessors} *)

(** [outer t] gets the outer path of [t] *)
val outer : t -> Path2.t

(** [holes t] gets the inner (hole) paths of [t] *)
val holes : t -> Path2.t list

(** {1 Creation and Validation} *)

(** [make ?validate ?holes outer]

    Create a 2d polygon from an [outer] path (perimeter), and zero or more
    [holes] (default = [[]]). If validate is [true] (as it is by default),
    {!val:validation} is performed, raising exceptions if the defined polygon is
    not simple (and thus, may cause problems in CGAL). *)
val make : ?validate:bool -> ?holes:Path2.t list -> Path2.t -> t

(** [of_paths ?validate ps]

    Create a 2d polygon from a list of paths, with the outline first, followed
    by the inner hole paths (if any), e.g. [(outer :: holes) as ps]. See
    {!make} for details. *)
val of_paths : ?validate:bool -> Path2.t list -> t

(** [of_list ps]

    Create a 2d polygon from a list of paths. Same as {!of_paths} with
    [~validate:false]. *)
val of_list : V2.t list list -> t

(** [to_list t]

    Convert the polygon [t] into a list with the [outer] path as the head. *)
val to_list : t -> V2.t list list

(** [of_seq s]

     Construct a polygon from a sequence of sequences of points [s]. *)
val of_seq : V2.t Seq.t Seq.t -> t

(** [to_seq t]

     Convert the polygon [t] to a sequence of sequences of points. *)
val to_seq : t -> V2.t Seq.t Seq.t

(** [of_array a]

     Construct a polygon from an array of arrays of points [a]. *)
val of_array : V2.t array array -> t

(** [to_array t]

     Convert the polygon [t] to an array of arrays of points. *)
val to_array : t -> V2.t array array

(** [add_holes ?validate ~holes t]

    Add [holes] to [t]. If validate is [true] (as it is by default),
    {!val:validation} is performed, raising exceptions if the defined polygon is
    not simple (and thus, may cause problems in CGAL). *)
val add_holes : ?validate:bool -> holes:V2.t list list -> t -> t

(** [validation ?eps t]

    Validate that [t] is a legal polygon, without self-intersections within
    paths, cross-intersections between paths, or any duplicate points. All
    checks are performed with the tolerance [eps]. If valid, unit will be
    returned, otherwise an exception will be raised.

    @raise InvalidPoly if any paths in [t] self-intersect, there are
    cross-intersections between them, or if there are duplicate points. *)
val validation : ?eps:float -> t -> unit

(** [is_simple ?eps t]

    Return [true] if [t] is a simple polygon, without self-intersections within
    paths, cross-intersections between paths, or any duplicate points. All
    checks are performed with the tolerance [eps]. *)
val is_simple : ?eps:float -> t -> bool

(** {1 Basic Shapes} *)

(** [circle ?fn ?fa ?fs r]

    Create a circle of radius [r]. *)
val circle : ?fn:int -> ?fa:float -> ?fs:float -> float -> t

(** [wedge ?fn ?fa ?fs ~centre ~radius ~start a]

    Create an arcing path (as in {!val:Path2.arc}), with the [centre] point
    included to close the path, forming a wedge. *)
val wedge
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> centre:V2.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [square ?center dims]

    Create a rectangular polygon with xy [dims] (e.g. width and height). If
    [center] is [true] then the path will be centred around the origin (default
    = [false]). *)
val square : ?center:bool -> V2.t -> t

(** [ellipse ?fn ?fa ?fs radii]

    Draw an ellipse with xy [radii]. The greater of the two radii is used for
    fragment/resolution calculation. *)
val ellipse : ?fn:int -> ?fa:float -> ?fs:float -> V2.t -> t

(** [star ~r1 ~r2 n]

    Draw an [n] pointed star with inner radius [r1] and outer radius [r2]. *)
val star : r1:float -> r2:float -> int -> t

(** [ring ?fn ?fa ?fs ~thickness radii]

    Draw an empty elliptical ring of outer xy [radii], with the given radial
    [thickness] (difference between outer and inner radii). For a circular
    ring, use equal x and y radii. *)
val ring : ?fn:int -> ?fa:float -> ?fs:float -> thickness:V2.t -> V2.t -> t

(** [box ?center ~thickness dims]

    Create a rectangular empty box of outer xy dimensions [dims], with the given
    xy [thickness] (difference between outer and inner xy dimensions). If
    [center] is [true], then the path will be centred around the origin
    (default = [false]). *)
val box : ?center:bool -> thickness:V2.t -> V2.t -> t

(** {1 Geometry} *)

(** [bbox t]

    Compute the 2d bounding box of the polygon [t]. *)
val bbox : t -> Gg.Box2.t

(** [centroid ?eps t]

    Compute the centroid of the outer path of the polygon [t]. If [t.outer] is
    collinear or self-intersecting (within [eps] tolerance), an
    [Invalid_argument] exception is raised. *)
val centroid : ?eps:float -> t -> V2.t

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

(** {1 Transformations} *)

(** [map f t]

    Map the outer and inner paths of [t] with the function [f]. *)
val map : (Path2.t -> Path2.t) -> t -> t

val translate : V2.t -> t -> t
val xtrans : float -> t -> t
val ytrans : float -> t -> t
val rotate : ?about:V2.t -> float -> t -> t
val zrot : ?about:V2.t -> float -> t -> t
val scale : V2.t -> t -> t
val xscale : float -> t -> t
val yscale : float -> t -> t
val mirror : V2.t -> t -> t
val affine : Affine2.t -> t -> t
