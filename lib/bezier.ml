(* Based on: https://github.com/revarbat/BOSL2/blob/master/beziers.scad *)

module Tbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end)

let signed_pascals_triangle =
  let tbl = Tbl.create 10 in
  Tbl.add tbl 0 (Array.make 1 (-1.));
  fun n ->
    let len = Tbl.length tbl in
    if n > len - 1
    then
      for i = len - 1 to n do
        let row = Array.make (i + 2) (-1.)
        and last = Tbl.find tbl i in
        for j = 0 to i - 1 do
          let sign = if j mod 2 = 1 then -1. else 1. in
          row.(j + 1) <- sign *. (Float.abs last.(j) +. Float.abs last.(j + 1))
        done;
        row.(i + 1) <- (if i mod 2 = 0 then 1. else -1.);
        Tbl.add tbl (i + 1) row
      done
    else ();
    tbl

let bezier_matrix =
  let tbl = Tbl.create 10 in
  fun n ->
    match Tbl.find_opt tbl n with
    | Some m -> m
    | None   ->
      let tri = signed_pascals_triangle n
      and m = Array.make_matrix (n + 1) (n + 1) 0. in
      let nth_row = Tbl.find tri n in
      for i = 0 to n do
        let a = Tbl.find tri i
        and b = nth_row.(i) in
        for j = 0 to i do
          m.(i).(j) <- a.(j) *. b
        done
      done;
      Tbl.add tbl n m;
      m

module type S = sig
  type vec

  (** A bezier curve function. *)
  type t = float -> vec

  (** [coefs ps]

      Compute the bezier algebraic coefficients for the control points [ps]. *)
  val coefs : vec list -> vec array

  (** [make ps]

      Create bezier function of degree n ([n = List.length ps - 1]) from the
      control points [ps]. The resulting continuous curve advances from the 0th
      control point at [0.], to the final control point at [1.]. *)
  val make : vec list -> t

  (** [curve ?init ?rev ?fn ?endpoint t]

      Draw a path of [fn] segments (default = [16]) along the bezier curve [t].

      - [init] can be provided to be prepended to (defaults to an empty list)
      - If [rev] is [true], the bezier will be drawn in reverse (default = [false])
      - If [endpoint] is [true], the last point will be a control point (last or
        first depending on [rev]), otherwise it will be off by the step size
        ([1 / fn]) (default = [true]). *)
  val curve : ?init:vec list -> ?rev:bool -> ?fn:int -> ?endpoint:bool -> t -> vec list

  (** [length ?start_u ?end_u ?max_deflect ps]

      Compute the length along the bezier defined by the control points [ps]
      between the fractional positions [start_u] and [end_u] ([0.] to [1.] by
      default). This is approximated as the sum of line segments whose midpoints
      deviate from the bezier by no more than [max_deflect]. *)
  val length : ?start_u:float -> ?end_u:float -> ?max_deflect:float -> vec list -> float

  (** [patch grid]

      Create a bezier patch (curved surface) from an rectangular [grid] of
      control points. *)
  val patch : vec list list -> float -> t

  (** [patch_curve ?fn p]

      Sample a grid of [fn] by [fn] segments describing a curved surface from the
      bezier patch [p]. (default [fn = 16]). *)
  val patch_curve : ?fn:int -> (float -> t) -> vec list list

  (** [of_bezpath ?n ps]

      Create a bezier function from a series of degree [n] beziers connected
      end-to-end defined by the control points [ps]. The end-point of the first
      curve is the start-point of the next, and so on. Thus, two cubic-beziers
      ([n = 3], the default) in series is described by seven control points
      (middle point is shared). The number of curves is [List.length ps - 1 / n],
      if [ps] cannot be broken by degree [n] in this way, an [Invalid_argument]
      exception will be raised. *)
  val of_bezpath : ?n:int -> vec list -> t

  (** [bezpath_of_path ?closed ?uniform ?size ?tangents path]

    Create a bezier path (see {!of_bezpath}) which defines a curve that
    passes through the points of [path].

    - [size] sets the absolute or relative (as a fraction of segment length)
      distance that the computed curve can deviate from the input [path].
      Provided either as a flat value for all points, or a list with a value for
      each {i segment} of the [path] (default = [`Flat (`Rel 0.1)]).
    - [tangents] provides control over the tangents of the computed curve where
      it passes through the points of [path]. Tangents can either be provided by
      the user with [`Tangents l], or computed [`NonUniform | `Uniform] (see
      {!Path2.tangents}).
    - If [closed] is [true] (default = [false]), an additional segment between
      the last and first points of [path] will be included in the computations.
      Thus, this impacts the correct lengths of lists provided to the [size] and
      [tangents] parameters (= length of [path] if [closed], one less if not). *)
  val bezpath_of_path
    :  ?closed:bool
    -> ?size:
         [ `Abs of float list
         | `Rel of float list
         | `Flat of [ `Abs of float | `Rel of float ]
         | `Mix of [ `Abs of float | `Rel of float ] list
         ]
    -> ?tangents:[ `NonUniform | `Uniform | `Tangents of vec list ]
    -> vec list
    -> vec list

  (** [bezpath_curve ?fn ?n ps]

      Compute a bezier function from a series of degree [n] beziers connected
      end-to-end defined by the control points [ps] and use to draw a path of
      [fn] segments. See {!of_bezpath} for explanation of bezier paths. *)
  val bezpath_curve : ?fn:int -> ?n:int -> vec list -> vec list

  (** [of_path ?closed ?uniform ?size ?tangents path]

    Create a bezier function which defines a curve that passes through the
    points of [path].

    - [size] sets the absolute or relative (as a fraction of segment length)
      distance that the computed curve can deviate from the input [path].
      Provided either as a flat value for all points, or a list with a value for
      each {i segment} of the [path] (default = [`Flat (`Rel 0.1)]).
    - [tangents] provides control over the tangents of the computed curve where
      it passes through the points of [path]. Tangents can either be provided by
      the user with [`Tangents l], or computed [`NonUniform | `Uniform] (see
      {!Path2.tangents}). Default is [`NonUniform].
    - If [closed] is [true] (default = [false]), an additional segment between
      the last and first points of [path] will be included in the computations.
      Thus, this impacts the correct lengths of lists provided to the [size] and
      [tangents] parameters (= length of [path] if [closed], one less if not). *)
  val of_path
    :  ?closed:bool
    -> ?size:
         [ `Abs of float list
         | `Rel of float list
         | `Flat of [ `Abs of float | `Rel of float ]
         | `Mix of [ `Abs of float | `Rel of float ] list
         ]
    -> ?tangents:[ `NonUniform | `Uniform | `Tangents of vec list ]
    -> vec list
    -> t

  (** [closest_point ?n ?max_err t p]

      Find the fractional position along the bezier [t] closest to the point
      [p]. [t] is treated as cubic (degree [n = 3]) by default, subdividing the
      bezier by [3] for each degree. Search continues until a position less
      [max_err] (default = [0.01]) distance from [p] is found. *)
  val closest_point : ?n:int -> ?max_err:float -> t -> vec -> float

  (** [deriv ?order ps]

      Calculate the [order] derivative of the bezier defined by the control points
      [ps]. The first derivative is taken by default ([order = 1]).
      [Invalid_argument] is raised if [order] is below [0], or the degree of
      the bezier ([length ps - 1]) is lower than the [order]. *)
  val deriv : ?order:int -> vec list -> t
end

module type S' = sig
  include S

  val coefs' : vec array -> vec array
  val make' : vec array -> t
  val curve' : ?rev:bool -> ?fn:int -> ?endpoint:bool -> t -> vec array
  val patch' : vec array array -> float -> t
  val patch_curve' : ?fn:int -> (float -> t) -> vec array array
end

module Make (V : V.S) = struct
  type t = float -> V.t

  module P = Path.Make (V)

  let coefs' ps =
    let n = Array.length ps - 1 in
    let bm = bezier_matrix n
    and m = Array.make (n + 1) V.zero in
    for i = 0 to n do
      let row = bm.(i) in
      for j = 0 to n do
        m.(i) <- V.add m.(i) (V.smul ps.(j) row.(j))
      done
    done;
    m

  let coefs ps = coefs' (Array.of_list ps)

  let make' ps =
    let n = Array.length ps - 1 in
    let m = coefs' ps in
    fun u ->
      let pt = ref V.zero in
      for i = 0 to n do
        pt := V.add !pt (V.smul m.(i) (Float.pow u (Float.of_int i)))
      done;
      !pt

  let make ps = make' (Array.of_list ps)

  let curve ?(init = []) ?(rev = false) ?(fn = 16) ?(endpoint = true) bez =
    let step = 1. /. Float.of_int fn in
    let last = if endpoint then 1. else 1. -. step in
    let dt = 1. /. Float.of_int fn *. if rev then last else -.last in
    let rec loop acc i t =
      if i <= fn then loop (bez t :: acc) (i + 1) (t +. dt) else acc
    in
    loop init 0 (if rev then 0. else 1.)

  let curve' ?(rev = false) ?(fn = 16) ?(endpoint = true) bez =
    let a = Array.make (fn + 1) V.zero
    and step = 1. /. Float.of_int fn in
    let last = if endpoint then 1. else 1. -. step in
    let dt = 1. /. Float.of_int fn *. if rev then last else -.last
    and t = ref (if rev then 1. else 0.) in
    for i = 0 to fn do
      a.(i) <- bez !t;
      t := !t +. dt
    done;
    a

  let length ?(start_u = 0.) ?(end_u = 1.) ?(max_deflect = 0.01) ps =
    let n_segs = List.length ps * 2
    and bz = make ps in
    let d = Float.of_int n_segs in
    let rec aux su eu =
      let path =
        let f i = bz (Math.lerp su eu (Float.of_int i /. d)) in
        Array.init (n_segs + 1) f
      in
      (* maximum deviation from a straight line *)
      let deflection =
        let mx = ref Float.min_float
        and p = Array.unsafe_get path in
        for i = 0 to n_segs - 2 do
          let mid_point = V.mid (p i) (p (i + 2)) in
          mx := Float.max !mx (V.distance (p (i + 1)) mid_point)
        done;
        !mx
      in
      if deflection <= max_deflect
      then P.length' path
      else (
        let sum = ref 0. in
        for i = 0 to n_segs - 1 do
          let i = Float.of_int i in
          let su' = Math.lerp su eu (i /. d)
          and eu' = Math.lerp su eu ((i +. 1.) /. d) in
          sum := !sum +. aux su' eu'
        done;
        !sum )
    in
    aux start_u end_u

  let patch grid =
    let horizontal_bezs = List.map make grid in
    fun u ->
      let vertical_bez = make @@ List.map (fun bz -> bz u) horizontal_bezs in
      vertical_bez

  let patch' grid =
    let horizontal_bezs = Array.map make' grid in
    fun u ->
      let vertical_bez = make' @@ Array.map (fun bz -> bz u) horizontal_bezs in
      vertical_bez

  let patch_curve' ?(fn = 16) p =
    let m = Array.make_matrix (fn + 1) (fn + 1) V.zero
    and step = 1. /. Float.of_int fn in
    for col = 0 to fn do
      let vbez = p (Float.of_int col *. step) in
      for row = 0 to fn do
        m.(row).(col) <- vbez (Float.of_int row *. step)
      done
    done;
    m

  let patch_curve ?(fn = 16) p =
    let m = patch_curve' ~fn p in
    List.init (fn + 1) (fun i -> List.init (fn + 1) (fun j -> m.(i).(j)))

  let of_bezpath ?(n = 3) bezpath =
    let bezpath = Array.of_list bezpath in
    let len = Array.length bezpath in
    if len mod n <> 1
    then (
      let msg =
        Printf.sprintf
          "The length of a degree %i bezier path should be a multiple of %i, plus 1."
          len
          len
      in
      invalid_arg msg );
    let n_segs = (len - 1) / n in
    let segs =
      Array.init n_segs (fun i -> List.init (n + 1) (fun j -> bezpath.((i * n) + j)))
    in
    let bezs = Array.map (fun ps -> make ps) segs in
    let lengths, total =
      let f (acc, total) seg =
        let total = total +. length seg in
        total :: acc, total
      in
      let acc, total = Array.fold_left f ([ 0. ], 0.) segs in
      Util.array_of_list_rev acc, total
    in
    let extrapolate s =
      if s >= 1.
      then bezs.(n_segs - 1) 1.0
      else (
        let d = Float.(min (max s 0.) 1.) *. total in
        let i = ref 0
        and p = ref None in
        while Option.is_none !p && !i < n_segs do
          let idx = !i in
          let d0 = Array.unsafe_get lengths idx
          and d1 = Array.unsafe_get lengths (idx + 1) in
          if d >= d0 && d <= d1
          then (
            let frac = (d -. d0) /. (d1 -. d0) in
            p := Some (bezs.(idx) frac) )
          else incr i
        done;
        Option.get !p )
    in
    extrapolate

  let bezpath_of_path
      ?(closed = false)
      ?(size = `Flat (`Rel 0.1))
      ?(tangents = `NonUniform)
      path
    =
    let ps = Array.of_list path
    and get_size =
      let valid unwrap l =
        let mn = List.fold_left (fun m a -> Float.min m (unwrap a)) Float.infinity l in
        Float.is_finite mn && mn > 0.
      and unwrap = function
        | `Abs s | `Rel s -> s
      in
      match size with
      | `Flat (`Rel s) when s > 0. -> fun _ seg_len -> seg_len *. s
      | `Flat (`Abs s) when s > 0. -> fun _ _ -> s
      | `Rel ss when valid Fun.id ss ->
        let ss = Array.of_list ss in
        fun i seg_len -> seg_len *. Array.unsafe_get ss i
      | `Abs ss when valid Fun.id ss ->
        let ss = Array.of_list ss in
        fun i _ -> Array.unsafe_get ss i
      | `Mix ss when valid unwrap ss ->
        let ss = Array.of_list ss in
        fun i seg_len ->
          ( match Array.unsafe_get ss i with
          | `Rel s -> seg_len *. s
          | `Abs s -> s )
      | _ -> invalid_arg "Size must be greater than zero."
    and power_poly =
      let m =
        [| [| -3.; 6.; -3. |]; [| 7.; -9.; 2. |]; [| -5.; 3.; 0. |]; [| 1.; 0.; 0. |] |]
      in
      fun n1 n2 -> Math.matmul m V.[| [| dot n1 n1 |]; [| dot n1 n2 |]; [| dot n2 n2 |] |]
    in
    let tangents =
      ( match tangents with
      | `Tangents tangents -> List.map V.normalize tangents
      | `NonUniform        -> P.tangents ~uniform:false ~closed path
      | `Uniform           -> P.tangents ~uniform:true ~closed path )
      |> Array.of_list
    in
    let len_ps = Array.length ps
    and len_ts = Array.length tangents in
    let f i acc =
      let p1 = ps.(i)
      and p2 = ps.(Util.index_wrap ~len:len_ps (i + 1)) in
      let seg_len = V.distance p2 p1 in
      let seg_vec = V.(sdiv (sub p2 p1) seg_len)
      and t1 = tangents.(i) (* second tangent pointing backwards *)
      and t2 = V.neg tangents.(Util.index_wrap ~len:len_ts (i + 1)) in
      (* total component of tangents parallel to the segment *)
      let parallel = Float.abs (V.dot t1 seg_vec) +. Float.abs (V.dot t2 seg_vec)
      and normal1 = V.(sub t1 (smul seg_vec (dot t1 seg_vec)))
      and normal2 = V.(sub t2 (smul seg_vec (dot t2 seg_vec))) in
      let p = Array.map (fun a -> a.(0)) (power_poly normal1 normal2) in
      let uextreme =
        let p_norm = Float.sqrt (Array.fold_left (fun sum a -> (a *. a) +. sum) 0. p) in
        if Math.approx p_norm 0.
        then [||]
        else (
          let f acc r = if r > 0. && r < 1. then r :: acc else acc in
          Util.array_of_list_rev (Array.fold_left f [] (Math.real_roots p)) )
      in
      let dists =
        let bz = make' [| V.zero; normal1; normal2; V.zero |] in
        Array.map (fun u -> V.norm (bz u)) uextreme
      in
      let scale =
        match Array.length dists with
        | 0 -> 0.
        | 1 -> dists.(0)
        | _ ->
          let sum = Array.fold_left ( +. ) 0. dists
          and min = Array.fold_left Float.min 0. dists in
          sum -. (2. *. min)
      in
      let l = Float.min (seg_len /. parallel) (get_size i seg_len /. scale) in
      V.(add p2 (smul t2 l)) :: V.(add p1 (smul t1 l)) :: p1 :: acc
    in
    let last_idx = len_ps - if closed then 0 else 1 in
    List.rev (ps.(last_idx mod len_ps) :: Util.fold_init last_idx f [])

  let bezpath_curve ?(fn = 16) ?(n = 3) bezpath =
    let bezpath = Array.of_list bezpath in
    let len = Array.length bezpath in
    if len mod n <> 1
    then (
      let msg =
        Printf.sprintf
          "The length of a degree %i bezier path should be a multiple of %i, plus 1."
          len
          len
      in
      invalid_arg msg );
    let n_segs = (len - 1) / n in
    let f i pts =
      let bez = make' @@ Array.init (n + 1) (fun j -> bezpath.((i * n) + j)) in
      let pts = curve ~init:pts ~rev:true ~fn bez in
      (* avoid duplication of endpoints *)
      if i < n_segs - 1 then List.tl pts else pts
    in
    List.rev @@ Util.fold_init n_segs f []

  let of_path ?closed ?size ?tangents path =
    of_bezpath ~n:3 @@ bezpath_of_path ?closed ?size ?tangents path

  let closest_point ?(n = 3) ?max_err bez p =
    P.continuous_closest_point ~n_steps:(n * 3) ?max_err bez p

  let deriv ?(order = 1) ps =
    let rec aux n order = function
      | hd :: tl ->
        let deltas =
          let f (acc, last) p = V.(smul (sub p last) (Float.of_int n)) :: acc, p in
          List.rev @@ fst @@ List.fold_left f ([], hd) tl
        in
        if order = 1 then make deltas else aux (n - 1) (order - 1) deltas
      | _        -> failwith "impossible"
    in
    let n = List.length ps - 1 in
    if order < 0
    then invalid_arg "Derivative order cannot be negative."
    else if n <= order
    then
      invalid_arg
        (Printf.sprintf "Bezier degree (%i) must be no lower than order (%i)." n order)
    else if order = 0
    then make ps
    else aux n order ps
end
