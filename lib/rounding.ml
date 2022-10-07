module type S = sig
  (** Roundovers inspired by the {{:https://github.com/revarbat/BOSL2} BOSL2}
   {{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding} module. *)

  type vec

  (** Configuration module with types and helpers for specifying path
    roundovers. *)
  module Round : sig
    (** Radius of circular arc roundovers. *)
    type radius = [ `Radius of float ]

    (** Distance away from the corner the roundover should start. *)
    type joint = [ `Joint of float ]

    (** Distance in from the corner that should be cut off by the roundover. *)
    type cut = [ `Cut of float ]

    (** Width of the segment replacing chamfered corners. *)
    type width = [ `Width of float ]

    (** Roundover specification for a corner of a path. *)
    type corner

    (** Full roundover specification for a path, either given as a mixed list of
         pairs of coordinates and {!type:corner} specifications that apply to
         them, or a single spec to be applied to all
         corners of the included path. *)
    type t

    (** {1 Corners} *)

    (** [chamf spec]

        Create a chamfered {!type:corner} specification. *)
    val chamf : [ cut | joint | width ] -> corner

    (** [circ spec]

        Create a circular {!type:corner} specification. *)
    val circ : [ cut | joint | radius ] -> corner

    (** [bez ?curv spec]

        Create a continuous curvature {!type:corner} specification. [curv] sets
        the smoothness of bezier curvature (default = [0.5]). *)
    val bez : ?curv:float -> [ cut | joint ] -> corner

    (** {1 General specifications} *)

    (** [mix l]

        Wrap a list of points paired with (optional) corner specifications as a
        {!type:t}. Note that it is the users responsibility to leave the specs for
        the first and last points as [None] if they intend to treat the path as
        open. *)
    val mix : (vec * corner option) list -> t

    (** [flat ?closed ~corner path]

        Create a roundover specification that will apply [corner] to each of
        the points in [path] (other than the first and last points if [closed] is
        [false], default = [true]). *)
    val flat : ?closed:bool -> corner:corner -> vec list -> t

    (** {1 Variable amplitude specifications} *)

    (** [chamfers ~kind spec_pts]

        Create an all chamfer {!type:t} specification, with variable amplitude
        of the given [kind] paired with each point of the path. *)
    val chamfers : kind:[ `Cut | `Joint | `Width ] -> (vec * float) list -> t

    (** [circles ~kind spec_pts]

        Create an all circular {!type:t} specification, with variable amplitude
        of the given [kind] paired with each point of the path. *)
    val circles : kind:[ `Radius | `Cut | `Joint ] -> (vec * float) list -> t

    (** [bezier ?curv ~kind spec_pts]

         Create an all continuour curvature {!type:t} specification, with variable
         amplitude of the given [kind] paired with each point of the path. Curvature
         smoothness of all roundovers is set by [curv] (default = [0.5]). If variable
         smoothness is desired, {!val:bez} and {!val:mix} may be used in conjunction
         to achieve it. *)
    val beziers : ?curv:float -> kind:[ `Cut | `Joint ] -> (vec * float) list -> t
  end

  (** [roundover ?fn ?fa ?fs ?overrun path_spec]

      Apply the roundover specifictions in [path_spec] on the bundled
      path/shape, with quality set by the [fn], [fa], and [fs] parameters.
      Collinear points are ignored (included in output without roundover
      applied).

      When [overrun] is set to [`Fail] (as it is by default) this function will
      raise [Failure] if computed joint distances would lead to point insertion
      that causes the path to reverse/double back on itself. Alternatively:
      - [`Warn] will print the detected overruns to [stdout] rather than
        raising [Failure] (useful for debuggin)
      - [`Fix] will automatically reduce the corner joints involved in each
        overrun proportional to their lengths.
      - [`NoCheck] skips overrun detection *)
  val roundover
    :  ?fn:int
    -> ?fa:float
    -> ?fs:float
    -> ?overrun:[ `Fail | `Warn | `Fix | `NoCheck ]
    -> Round.t
    -> vec list
end

module type Arc = sig
  type vec

  val arc_about_centre
    :  ?rev:bool
    -> ?fn:int
    -> ?fa:float
    -> ?fs:float
    -> ?dir:[ `CW | `CCW ]
    -> ?wedge:bool
    -> centre:vec
    -> vec
    -> vec
    -> vec list
end

module Make (V : V.S) (Arc : Arc with type vec := V.t) = struct
  module Bz = Bezier.Make (V)
  module P = Path.Make (V)

  module Round = struct
    type radius = [ `Radius of float ]
    type joint = [ `Joint of float ]
    type cut = [ `Cut of float ]
    type width = [ `Width of float ]

    type corner =
      | Chamf of [ joint | cut | width ]
      | Circ of [ radius | joint | cut ]
      | Bez of
          { spec : [ joint | cut ]
          ; curv : float
          }

    type t =
      | Mix of (V.t * corner option) list
      | Flat of
          { path : V.t list
          ; corner : corner
          ; closed : bool
          }

    let chamf spec = Chamf spec
    let circ spec = Circ spec
    let bez ?(curv = 0.5) spec = Bez { spec; curv }
    let mix ss = Mix ss
    let flat ?(closed = true) ~corner path = Flat { path; corner; closed }

    let chamfers ~kind spec_pts =
      let wrap =
        match kind with
        | `Cut   -> fun c -> `Cut c
        | `Joint -> fun j -> `Joint j
        | `Width -> fun w -> `Width w
      in
      let f (p, v) = p, if Float.equal 0. v then None else Some (chamf (wrap v)) in
      mix @@ List.map f spec_pts

    let circles ~kind spec_pts =
      let wrap =
        match kind with
        | `Radius -> fun r -> `Radius r
        | `Cut    -> fun c -> `Cut c
        | `Joint  -> fun j -> `Joint j
      in
      let f (p, v) = p, if Float.equal 0. v then None else Some (circ (wrap v)) in
      mix @@ List.map f spec_pts

    let beziers ?curv ~kind spec_pts =
      let wrap =
        match kind with
        | `Cut   -> fun c -> `Cut c
        | `Joint -> fun j -> `Joint j
      in
      let f (p, v) = p, if Float.equal 0. v then None else Some (bez ?curv (wrap v)) in
      mix @@ List.map f spec_pts
  end

  open Round

  let bez ~curv ~prev ~next ~d p =
    V.
      [ add p (smul prev d)
      ; add p (smul prev (curv *. d))
      ; p
      ; add p (smul next (curv *. d))
      ; add p (smul next d)
      ]

  let bez_spec ?(curv = 0.5) ~spec p1 p2 p3 =
    match spec with
    | `Joint d -> `Bez (d, curv)
    | `Cut c   ->
      let half_angle = V.angle_points p1 p2 p3 /. 2. in
      `Bez (8. *. c /. Float.cos half_angle /. (1. +. (4. *. curv)), curv)

  let chamf_spec ~spec p1 p2 p3 =
    match spec with
    | `Joint d -> `Chamf d
    | `Cut c   -> `Chamf (c /. Float.cos (V.angle_points p1 p2 p3 /. 2.))
    | `Width w -> `Chamf (w /. Float.sin (V.angle_points p1 p2 p3 /. 2.) /. 2.)

  let circ_spec ~spec p1 p2 p3 =
    let half_angle = V.angle_points p1 p2 p3 /. 2. in
    let is_180 = Math.approx half_angle (Float.pi /. 2.) in
    match spec with
    | `Joint d  ->
      let rad = d *. Float.tan half_angle in
      `Circ (d, rad)
    | `Radius r -> `Circ (r /. Float.tan half_angle, r)
    | `Cut c    ->
      let rad = c /. ((1. /. Float.sin half_angle) -. 1.) in
      `Circ ((if is_180 then Float.infinity else rad /. Float.tan half_angle), rad)

  let get_d = function
    | `Bez (d, _) | `Chamf d | `Circ (d, _) -> d

  let bez_corner ?fn ?(fs = Util.fs) ?(curv = 0.5) ~d p1 p2 p3 =
    let ps =
      let prev = V.(normalize @@ sub p1 p2)
      and next = V.(normalize @@ sub p3 p2) in
      V.
        [ add p2 (smul prev d)
        ; add p2 (smul prev (curv *. d))
        ; p2
        ; add p2 (smul next (curv *. d))
        ; add p2 (smul next d)
        ]
    in
    let fn =
      match fn with
      | Some fn -> fn
      | None    -> Float.(to_int @@ ceil (Bz.length ps /. fs))
    in
    Bz.curve ~fn:(Int.max fn 3) (Bz.make ps)

  let chamfer_corner ~d p1 p2 p3 =
    let prev = V.(normalize @@ sub p1 p2)
    and next = V.(normalize @@ sub p3 p2) in
    V.[ add p2 (smul prev d); add p2 (smul next d) ]

  let circle_corner ?fn ?(fa = Util.fa) ?(fs = Util.fs) ~d ~rad p1 p2 p3 =
    let half_angle = V.angle_points p1 p2 p3 /. 2. in
    let is_180 = Math.approx half_angle (Float.pi /. 2.) in
    let prev = V.(normalize @@ sub p1 p2)
    and next = V.(normalize @@ sub p3 p2) in
    let p1' = V.(add p2 (smul prev d))
    and p3' = V.(add p2 (smul next d)) in
    if is_180
    then [ p1'; p3' ]
    else (
      let centre =
        V.(add p2 (smul (normalize @@ add prev next) (rad /. Float.sin half_angle)))
      and fn =
        let frags = Float.of_int @@ Util.helical_fragments ?fn ~fa ~fs rad in
        Float.(to_int @@ max 3. @@ ceil (((pi /. 2.) -. half_angle) /. pi *. frags))
      in
      Arc.arc_about_centre ~fn ~centre p1' p3' )

  let specialize t =
    match t with
    | Chamf spec         -> chamf_spec ~spec
    | Circ spec          -> circ_spec ~spec
    | Bez { spec; curv } -> bez_spec ~curv ~spec

  let spec_to_corner ?fn ?fa ?fs t =
    match t with
    | `Chamf d       -> chamfer_corner ~d
    | `Circ (d, rad) -> circle_corner ~d ~rad ?fn ?fa ?fs
    | `Bez (d, curv) -> bez_corner ?fn ?fs ~curv ~d

  let prune_mixed_spec mix =
    let path, specs = Util.unzip mix in
    let path = Array.of_list path in
    let len = Array.length path in
    let w = Util.index_wrap ~len in
    let f (i, sps) sp =
      let p = path.(i) in
      if (not (V.collinear path.(w (i - 1)) p path.(w (i + 1)))) || Option.is_none sp
      then i + 1, sp :: sps
      else i + 1, None :: sps
    in
    let _, specs = List.fold_left f (0, []) specs in
    path, Array.get (Util.array_of_list_rev specs)

  let overruns_to_string ~specs = function
    | [] -> None
    | l  ->
      let f acc (i, over_prev, over_next) =
        let prev =
          match over_prev with
          | None   -> ""
          | Some v -> Printf.sprintf "overran the previous point by %.3f" v
        and next =
          match over_next with
          | Some v ->
            if Option.is_some over_prev
            then Printf.sprintf " and the next point by %.3f" v
            else Printf.sprintf "overran the next point by %.3f" v
          | None   -> ""
        in
        Printf.sprintf
          "%s\nRoundover of point %i with joint of %f %s%s."
          acc
          i
          (get_d (Option.get specs.(i))) (* overruns must have a spec *)
          prev
          next
      in
      Some (List.fold_left f "" l)

  let fix_specs ~path ~overruns specs =
    let len = Array.length path in
    let wrap = Util.index_wrap ~len in
    let set_d i d =
      if Math.approx d 0.
      then specs.(i) <- None
      else (
        match specs.(i) with
        | Some (`Bez (_, curv)) -> specs.(i) <- Some (`Bez (d, curv))
        | Some (`Circ _)        ->
          (* rad must be recomputed to match new joint distance *)
          specs.(i)
            <- (let p1 = path.(wrap (i - 1))
                and p2 = path.(i)
                and p3 = path.(wrap (i + 1)) in
                Some (circ_spec ~spec:(`Joint d) p1 p2 p3) )
        | Some (`Chamf _)       -> specs.(i) <- Some (`Chamf d)
        | None                  -> () )
    and prev = Array.make len None
    and next = Array.make len None in
    let f (i, prv, nxt) =
      let d = get_d (Option.get specs.(i)) in
      if Option.is_none prev.(i)
      then (
        let over = Option.value ~default:0. prv in
        match List.find_opt (fun (j, _, _) -> i = wrap (j + 1)) overruns with
        | Some (j, _, _) ->
          let prev_d = get_d (Option.get specs.(j)) in
          let sum = prev_d +. d in
          let prev_d' = prev_d -. (over *. (prev_d /. sum))
          and d' = d -. (over *. (d /. sum)) in
          prev.(i) <- Some d';
          next.(j) <- Some prev_d'
        | None           -> prev.(i) <- Some (d -. over) );
      if Option.is_none next.(i)
      then (
        let over = Option.value ~default:0. nxt in
        match List.find_opt (fun (j, _, _) -> i = wrap (j - 1)) overruns with
        | Some (j, _, _) ->
          let next_d = get_d (Option.get specs.(j)) in
          let sum = next_d +. d in
          let next_d' = next_d -. (over *. (next_d /. sum))
          and d' = d -. (over *. (d /. sum)) in
          next.(i) <- Some d';
          prev.(j) <- Some next_d'
        | None           -> next.(i) <- Some (d -. over) )
    in
    List.iter f overruns;
    for i = 0 to len - 1 do
      match prev.(i), next.(i) with
      | Some p, Some n -> set_d i (Float.min p n)
      | Some p, _      -> set_d i p
      | _, Some n      -> set_d i n
      | None, None     -> ()
    done

  let roundover ?fn ?fa ?fs ?overrun path_spec =
    let overrun = Option.value ~default:`Fail overrun in
    let path, get_spec =
      match path_spec with
      | Mix mix                       -> prune_mixed_spec mix
      | Flat { path; corner; closed } ->
        let path = Array.of_list path in
        let len = Array.length path in
        let get_corner =
          let w = Util.index_wrap ~len in
          let g i =
            if V.collinear path.(w (i - 1)) path.(i) path.(w (i + 1))
            then None
            else Some corner
          in
          if closed then g else fun i -> if i = 0 || i = len - 1 then None else g i
        in
        path, get_corner
    in
    let len = Array.length path in
    let wrap = Util.index_wrap ~len in
    let specs =
      Array.init len (fun i ->
          Option.map
            (fun s -> specialize s path.(wrap (i - 1)) path.(i) path.(wrap (i + 1)))
            (get_spec i) )
    and distances = Array.init len (fun i -> V.distance path.(i) path.(wrap (i + 1))) in
    let overruns =
      match overrun with
      | `NoCheck -> []
      | _        ->
        let avail d i j =
          distances.(i) -. d -. Util.value_map_opt get_d ~default:0. specs.(j)
        and over v = if v < 0. then Some (-.v) else None in
        let f i acc =
          let i = len - i - 1 in
          match specs.(i) with
          | Some spec ->
            let d = get_d spec in
            if d > 0.
            then (
              let prev = over @@ avail d (wrap (i - 1)) (wrap (i - 1))
              and next = over @@ avail d i (wrap (i + 1)) in
              if Option.is_some prev || Option.is_some next
              then (i, prev, next) :: acc
              else acc )
            else acc
          | None      -> acc
        in
        Util.fold_init len f []
    in
    let () =
      (* if in `Fix mode, mutate specs to eliminate any overruns by
            reducing the joint distances of each corner involved *)
      match overrun with
      | `Fix -> fix_specs ~path ~overruns specs
      | _    -> ()
    in
    let f i =
      match specs.(i) with
      | Some spec ->
        let corner = spec_to_corner ?fn ?fa ?fs spec in
        corner path.(wrap (i - 1)) path.(i) path.(wrap (i + 1))
      | None      -> [ path.(i) ]
    in
    let parts = List.init len f in
    ( match overrun with
    | `Fail ->
      Util.value_map_opt failwith ~default:() (overruns_to_string ~specs overruns)
    | `Warn ->
      Util.value_map_opt
        (Printf.printf "\n%s\n")
        ~default:()
        (overruns_to_string ~specs overruns)
    | _     -> () );
    P.deduplicate_consecutive ~closed:true @@ List.concat parts
end
