module type S = sig
  type vec
  type line
  type t = vec list

  (** {1 Construction / Conversion} *)

  (** [of_list l]

      Construct a path from a list of points [l] (no-op). *)
  val of_list : vec list -> t

  (** [of_seq s]

      Construct a path from a sequence of points [s]. *)
  val of_seq : vec Seq.t -> t

  (** [of_array a]

      Construct a path from an array of points [a]. *)
  val of_array : vec array -> t

  (** [to_list t]

      Convert the path [t] to a list of points (no-op). *)
  val to_list : t -> vec list

  (** [to_seq t]

      Convert the path [t] to a sequence of points. *)
  val to_seq : t -> vec Seq.t

  (** [to_array t]

      Convert the path [t] to a array of points. *)
  val to_array : t -> vec array

  (** {1 General Path Utilities} *)

  (** [length ?closed path]

      Calculate the length (total travel distance) of the [path]. If [closed]
      is [true], include the distance between the endpoints (default = [false]). *)
  val length : ?closed:bool -> t -> float

  (** [cummulative_length ?closed path]

      Calculate the cummulative length (distance travelled by each point) along
      the [path]. If [closed] is [true], include the distance between the
      endpoints (default = [false]). *)
  val cummulative_length : ?closed:bool -> t -> float list

  (** [to_continuous path]

      Return a continuous function from values over the range of [0.] to [1.]
      to positions along [path] (like a bezier function), treated as open
      ([closed = false]) by default. *)
  val to_continuous : ?closed:bool -> t -> float -> vec

  (** [resample ~freq path]

      Resample [path] with the given [freq]uency (either a flat number of
      points, or a target point spacing). Note that the only points guaranteed
      to appear in the output are the start and end points of [path]. For
      upsampling that preserves input points, see {!subdivide}. *)
  val resample : freq:[< `N of int | `Spacing of float ] -> t -> t

  (** [subdivide ?closed ~freq path]

      Subdivides [path] with given [freq]uency, including each of the original
      points in the output (unlike {!resample}). This can be a flat number of points
      provided directly with [`N (n, by)], or as a multiple of number of points in
      [path] with [`Refine (factor, by)]. The strategy for distribution of
      points for these count based methods is set with the second parameter [by],
      which can be either [`BySeg] (same number of additional points per segment)
      and [`ByLen] (segments gain new points proportional to their length).
      Alternatively, a maximum [`Spacing dist] can be specified instead. The
      [`Rough] point sampling variants will favour sampling uniformity, at the
      expense of not adhering exactly to the requested point count. *)
  val subdivide
    :  ?closed:bool
    -> freq:
         [ `N of int * [ `ByLen | `BySeg ]
         | `RoughN of int * [ `ByLen | `BySeg ]
         | `Refine of int * [ `ByLen | `BySeg ]
         | `RoughRefine of int * [ `ByLen | `BySeg ]
         | `Spacing of float
         ]
    -> vec list
    -> vec list

  (** [cut ?closed ~distances path]

      Cut [path] at a list of increasing [distances] ([`Abs]olute or
      [`Rel]ative) along it from the start. If [closed] is [true], the segment
      between the end and beginning of [path] will be considered, and the first
      point will be the last of the second path returned. Negative [`Abs distance]
      will start from the end to find the split point. Raises [Invalid_argument] if
      [distance] is an endpoint, or further than the end of the [path]. *)
  val cut
    :  ?closed:bool
    -> distances:[ `Abs of float list | `Rel of float list ]
    -> t
    -> t list

  (** [split ?closed ~distance path]

      Split [path] into two at the position [distance] ([`Abs]olute or
      [`Rel]ative) along [path] from the start. Otherwise the behaviour is the
      same as {!cut}. *)
  val split : ?closed:bool -> distance:[ `Abs of float | `Rel of float ] -> t -> t * t

  (** [noncollinear_triple ?eps path]

      Returns a pair of triples of non-collinear indices and the corresponding
      points from [path] (if the path is not completely collinear). Two well
      separated points are selected, and the third point is the furthest off the
      line drawn by the first two points.*)
  val noncollinear_triple
    :  ?eps:float
    -> t
    -> ((int * int * int) * (vec * vec * vec)) option

  (** [is_collinear ?eps path]

      Returns [true] if all points in [path] are collinear (fall within [eps]
    distance of the same line). *)
  val is_collinear : ?eps:float -> t -> bool

  (** [prune_collinear path]

      Remove collinear points from [path]. If [closed] is [true] the last point
      can be pruned as collinear with the first. The first point is never pruned. *)
  val prune_collinear : ?closed:bool -> t -> t

  (** [deduplicate_consecutive ?closed ?keep ?eq path]

      Remove consecutive duplicate points as determined by the (approximate)
      equality function [eq] ([V.approx ~eps:1e-9] by default) from [path]. By
      default [keep] is [`First], which includes the first point of each run of
      duplicates in the output. This can be instead be set to [keep] the [`Last],
      or to [`FirstAndEnds] or [`LastAndEnds], which follow their respective
      simpler rules with the caveat of preserving the endpoints (first and last
      points) of the path. The path is treated as open ([closed = false]) by
      default, if [closed] is [true] the last point of the path may be dropped
      (even if [keep] is [`FirstAndEnds | `LastAndEnds]). *)
  val deduplicate_consecutive
    :  ?closed:bool
    -> ?keep:[ `First | `Last | `FirstAndEnds | `LastAndEnds ]
    -> ?eq:(vec -> vec -> bool)
    -> t
    -> t

  (** [deriv ?closed ?h path]

      Computes a numerical derivative of [path], with [h] (default [1.]) giving
    the step size of the sampling of [path], so that the derivative can be
    scaled correctly. Setting [closed] to [true] will include computation of
    the derivative between the last and first point of the [path] (default
    [false]). *)
  val deriv : ?closed:bool -> ?h:float -> t -> t

  (** [deriv_nonuniform ?closed ?h path]

      Computes a numerical derivative of [path], with [h] giving
    the non-uniform step sizes of the sampling of [path], so that the derivative can be
    scaled correctly. Setting [closed] to [true] will include computation of
    the derivative between the last and first point of the [path] (default
    [false]). As [h] provides scaling factors for each segment of the path, it
    must have a length of one less than [path] if it's unclosed, and the same
    length if [closed] is [true]. *)
  val deriv_nonuniform : ?closed:bool -> h:float list -> t -> t

  (** [tangents ?uniform ?closed path]

      Compute tangent unit vectors of [path]. Set [closed] to [true] to indicate
      that tangents should include between the end and beginning of the path
      (default = [false]). Sampling of [path] is assumed to be [uniform] unless the
      parameter is set to [false], in which case the derivatives will be adjusted
      to correct for non-uniform sampling of points. *)
  val tangents : ?uniform:bool -> ?closed:bool -> t -> t

  (** [continuous_closest_point ?closed ?n_steps ?max_err f p]

      Find the closest position (from [0.] to [1.]) along the path function [f]
      to the point [p].

      - [n_steps] sets the granularity of search at each stage.
      - [max_err] the maximum distance the solution can be from the target [p] *)
  val continuous_closest_point
    :  ?closed:bool
    -> ?n_steps:int
    -> ?max_err:float
    -> (float -> vec)
    -> vec
    -> float

  (** [segment ?closed path]

      Break [path] into line segments. If [closed] is [true], include a segment
      between the last and first points of [path] (default [false]). *)
  val segment : ?closed:bool -> t -> line list

  (** [reindex_polygon reference poly]

       Rotate the polygonal (closed / coplanar) path [poly] to optimize its
       pairwise point association with the [reference] polygon. Paths should have
       the same clockwise winding direction (not checked / corrected). *)
  val reindex_polygon : t -> t -> t

  (** [lerp a b u]

      Linearly interpolate between the paths [a] and [b]. Raises
      [Invalid_argument] if the paths are of unequal length. *)
  val lerp : t -> t -> float -> t
end

module type S' = sig
  include S

  val length' : ?closed:bool -> vec array -> float
  val cummulative_length' : ?closed:bool -> vec array -> float array
  val prune_collinear' : vec array -> vec array
end

module Make (V : V.S) = struct
  type vec = V.t
  type line = V.line
  type t = vec list

  let of_list l = l
  let of_seq s = List.of_seq s
  let of_array a = Array.to_list a
  let to_list t = t
  let to_seq t = List.to_seq t
  let to_array t = Array.of_list t

  let length' ?(closed = false) path =
    let len = Array.length path
    and p = Array.unsafe_get path in
    if len < 2
    then 0.
    else (
      let sum = ref 0. in
      for i = 0 to len - 2 do
        sum := !sum +. V.distance (p i) (p (i + 1))
      done;
      if closed then !sum +. V.distance (p (len - 1)) (p 0) else !sum )

  let length ?(closed = false) = function
    | [] | [ _ ] -> 0.
    | hd :: tl ->
      let f (sum, last) p = sum +. V.distance p last, p in
      let sum, last = List.fold_left f (0., hd) tl in
      if closed then sum +. V.distance last hd else sum

  let cummulative_length ?(closed = false) = function
    | [] -> []
    | hd :: tl ->
      let f (acc, sum, last) p =
        let sum = sum +. V.distance p last in
        sum :: acc, sum, p
      in
      let travels, sum, last = List.fold_left f ([ 0. ], 0., hd) tl in
      if closed
      then List.rev ((sum +. V.distance last hd) :: travels)
      else List.rev travels

  let cummulative_length' ?(closed = false) path =
    let len = Array.length path in
    let n = len + if closed then 1 else 0 in
    if len < 1
    then [||]
    else (
      let travels = Array.make n 0. in
      for i = 1 to n - 1 do
        travels.(i) <- travels.(i - 1) +. V.distance path.(i - 1) path.(i mod len)
      done;
      travels )

  let segment_lengths ?(closed = false) = function
    | [] -> []
    | hd :: tl ->
      let f (acc, last) p = V.distance p last :: acc, p in
      let lengths, last = List.fold_left f ([], hd) tl in
      List.rev (if closed then V.distance last hd :: lengths else lengths)

  let to_continuous ?closed path =
    let travels = Array.of_list (cummulative_length ?closed path)
    and path =
      let a = Array.of_list path in
      let len = Array.length a in
      fun i -> Array.unsafe_get a (Util.index_wrap ~len i)
    in
    let len = Array.length travels in
    let total = travels.(len - 1) in
    let extrapolate s =
      let d = Float.(min (max s 0.) 1.) *. total in
      let i = ref 0
      and p = ref None in
      while Option.is_none !p && !i < len - 1 do
        let idx = !i in
        let d0 = Array.unsafe_get travels idx
        and d1 = Array.unsafe_get travels (idx + 1) in
        if d >= d0 && d <= d1
        then (
          let frac = (d -. d0) /. (d1 -. d0)
          and p0 = path idx
          and p1 = path (idx + 1) in
          p := Some (V.lerp p0 p1 frac) )
        else incr i
      done;
      Option.get !p
    in
    extrapolate

  let resample ~freq path =
    let n =
      match freq with
      | `N n -> n
      | `Spacing s -> Int.of_float @@ (length path /. s)
    in
    if n < 0 then invalid_arg "Resampling frequency must be positive.";
    let step = 1. /. Float.of_int (n - 1)
    and f = to_continuous path in
    List.init n (fun i -> f @@ (Float.of_int i *. step))

  let subdivide ?(closed = false) ~freq = function
    | [] | [ _ ] -> invalid_arg "Cannot subdivide path with fewer than 2 points."
    | [ _; _ ] when closed -> invalid_arg "Path of length 2 cannot be closed."
    | hd :: tl as path ->
      let lerp init a b n =
        let f i ps = V.lerp a b (Float.of_int i /. Float.of_int n) :: ps in
        Util.fold_init n f init
      and len = List.length path in
      ( match freq with
        | `Refine (1, _) | `RoughRefine (1, _) -> path
        | (`N (n, _) | `RoughN (n, _)) when len = n -> path
        | `Spacing s when s <= 0. -> invalid_arg "Minumum spacing must be greater than 0."
        | `Spacing s ->
          let f (a, ps) b =
            let n = Float.(to_int @@ ceil (V.distance a b /. s)) in
            b, lerp ps a b n
          in
          let last, ps = List.fold_left f (hd, []) tl in
          List.rev @@ if closed then snd @@ f (last, ps) hd else last :: ps
        | freq ->
          let exact, density_by, n =
            match freq with
            | `N (n, by) -> true, by, n
            | `RoughN (n, by) -> false, by, n
            | `Refine (factor, by) -> true, by, len * factor
            | `RoughRefine (factor, by) -> false, by, len * factor
            | _ -> failwith "`Spacing is unreachable."
          in
          if n < len
          then
            invalid_arg
            @@ Printf.sprintf
                 "Target number of points (%i) must not be less than input length (%i)."
                 n
                 len;
          let add_ns =
            let seg_lens =
              match density_by with
              | `ByLen -> segment_lengths ~closed path
              | `BySeg ->
                let n_segs = len - if closed then 0 else 1 in
                let l = length ~closed path /. Float.of_int n_segs in
                List.init n_segs (Fun.const l)
            in
            let density = Float.of_int (n - len) /. List.fold_left ( +. ) 0. seg_lens in
            if exact
            then (
              (* To obtain exact number of points / refinement, rounding error is
                    carried over between segments. Goal is to distribute the
                    error in a uniform manner. *)
              let f (err, adds) seg_len =
                let a = (seg_len *. density) -. err in
                let a' = Float.round a in
                a' -. a, Float.to_int a' :: adds
              in
              Util.array_of_list_rev @@ snd @@ List.fold_left f (0., []) seg_lens )
            else
              Util.array_of_list_map
                (fun l -> Float.(to_int @@ round @@ (l *. density)))
                seg_lens
          in
          let f ((i, a), ps) b = (i + 1, b), lerp ps a b (add_ns.(i) + 1) in
          let last, ps = List.fold_left f ((0, hd), []) tl in
          List.rev @@ if closed then snd @@ f (last, ps) hd else snd last :: ps )

  let cut ?(closed = false) ~distances = function
    | [] | [ _ ] -> invalid_arg "Path must have more than one point to be cut."
    | path ->
      let path = Array.of_list path in
      let travels = cummulative_length' ~closed path in
      let len = Array.length path
      and n_segs = Array.length travels in
      let total = travels.(n_segs - 1) in
      let distances =
        let dist, ds =
          match distances with
          | `Abs ds -> (fun d -> if d < 0. then total +. d else d), ds
          | `Rel ds -> (fun d -> total *. d), ds
        in
        let f d (acc, next_dist) =
          let d = dist d in
          if d <= 0. || d >= total
          then invalid_arg "Cut distances must fall between endpoints of path."
          else if d >= next_dist
          then invalid_arg "Cut distances must increase monotonically."
          else d :: acc, d
        in
        fst @@ List.fold_right f ds ([], Float.max_float)
      in
      let f (parts, first_pt, start_idx) d =
        let idx =
          let i = ref start_idx
          and continue = ref true in
          while !continue && !i < n_segs do
            if travels.(!i) > d then continue := false else incr i
          done;
          !i - 1
        in
        let cut_pt =
          let a = travels.(idx)
          and b = travels.(idx + 1) in
          V.lerp path.(idx) path.(Util.index_wrap ~len (idx + 1)) ((d -. a) /. (b -. a))
        in
        let part =
          let n = idx - start_idx + 1 in
          let cut_at_idx = V.approx cut_pt path.(start_idx + n - 1) in
          let f i = if i < n - 1 then path.(i + start_idx + 1) else cut_pt in
          let ps = List.init (if cut_at_idx then n - 1 else n) f in
          if V.approx first_pt path.(start_idx + 1) then ps else first_pt :: ps
        in
        part :: parts, cut_pt, idx
      in
      let parts, first_pt, start_idx = List.fold_left f ([], path.(0), 0) distances in
      let last_part =
        let n = len - start_idx - 1 in
        let f i =
          if i < n
          then path.(i + start_idx + 1)
          else if closed
          then path.(0)
          else path.(len - 1)
        in
        first_pt :: List.init n f
      in
      List.rev (last_part :: parts)

  let split ?closed ~distance path =
    let distances =
      match distance with
      | `Abs d -> `Abs [ d ]
      | `Rel d -> `Rel [ d ]
    in
    match cut ?closed ~distances path with
    | [ a; b ] -> a, b
    | _ -> failwith "impossible"

  let noncollinear_triple ?(eps = Util.epsilon) = function
    | [] | [ _ ] | [ _; _ ] -> None
    | hd :: (p :: tl as rest) ->
      let _, furthest_idx, furthest_point, dist =
        let f (i, idx, fp, dist) p =
          let d = V.distance hd p in
          if d > dist then i + 1, i, p, d else i + 1, idx, fp, dist
        in
        List.fold_left f (2, 1, p, V.distance hd p) tl
      in
      if dist <= eps
      then None
      else (
        let n = V.(sdiv (sub hd furthest_point) dist)
        and threshold = dist *. eps in
        let _, offline, _ =
          let f (i, pair, offset) p =
            let off = V.distance_to_vector (V.sub p hd) n in
            if off > offset then i + 1, Some (i, p), off else i + 1, pair, offset
          in
          List.fold_left f (1, None, threshold) rest
        in
        let f (oi, op) = (0, furthest_idx, oi), (hd, furthest_point, op) in
        Option.map f offline )

  let is_collinear ?eps path = Option.is_none (noncollinear_triple ?eps path)

  let prune_collinear_rev' ?(closed = false) path =
    let len = Array.length path in
    let w = Util.index_wrap ~len in
    let f i acc =
      let p = path.(i) in
      if i = 0 || ((not closed) && i = len - 1)
      then p :: acc
      else if not (V.collinear path.(w (i - 1)) p path.(w (i + 1)))
      then p :: acc
      else acc
    in
    Util.fold_init len f []

  let prune_collinear' ?closed path =
    Util.array_of_list_rev (prune_collinear_rev' ?closed path)

  let prune_collinear ?closed path =
    List.rev @@ prune_collinear_rev' ?closed (Array.of_list path)

  let deduplicate_consecutive
    ?(closed = false)
    ?(keep = `First)
    ?(eq = V.approx ~eps:Util.epsilon)
    = function
    | [] -> []
    | [ a; b ] as l ->
      ( match eq a b, keep with
        | true, `First -> [ a ]
        | true, `Last -> [ b ]
        | _ -> l )
    | first :: rest ->
      let final last acc = if closed && eq first last then acc else last :: acc in
      let rec loop acc is_first last = function
        | [ hd ] ->
          ( match eq hd last, keep with
            | true, `First -> final last acc
            | true, (`Last | `LastAndEnds | `FirstAndEnds) -> final hd acc
            | false, _ -> final hd (last :: acc) )
        | hd :: tl ->
          ( match eq hd last, is_first, keep with
            | true, _, (`First | `FirstAndEnds) -> loop acc is_first last tl
            | true, true, `LastAndEnds -> loop acc is_first last tl
            | true, _, `Last | true, false, `LastAndEnds -> loop acc is_first hd tl
            | false, _, _ -> loop (last :: acc) false hd tl )
        | [] -> failwith "unreachable"
      in
      List.rev (loop [] true first rest)

  let deriv ?(closed = false) ?(h = 1.) path =
    let path = Array.of_list path in
    let len = Array.length path in
    let f =
      let g i = Array.unsafe_get path (Util.index_wrap ~len i) in
      let calc i = V.(sub (g (i + 1)) (g (i - 1))) in
      if closed
      then calc
      else
        function
        | 0 when len < 3 -> V.sub (g 1) (g 0)
        | 0 -> V.(sub (smul (sub (g 1) (g 0)) 3.) (sub (g 2) (g 1)))
        | i when i = len - 1 && len < 3 -> V.sub (g (-1)) (g (-2))
        | i when i = len - 1 ->
          V.(sub (sub (g (-3)) (g (-2))) (smul (sub (g (-2)) (g (-1))) 3.))
        | i -> calc i
    in
    List.init len (fun i -> V.sdiv (f i) (2. *. h))

  let deriv_nonuniform ?(closed = false) ~h path =
    let path = Array.of_list path
    and h = Array.of_list h in
    let len = Array.length path in
    let valid_h_len = len - Bool.to_int (not closed) in
    if valid_h_len <> Array.length h
    then (
      let msg =
        Printf.sprintf
          "Invalid length of non-uniform sampling parameter `h`. Should be %i."
          valid_h_len
      in
      invalid_arg msg );
    let f =
      let w = Util.index_wrap ~len in
      let calc i =
        let h1 = h.(w (i - 1))
        and h2 = h.(i)
        and vc = path.(i)
        and v1 = path.(w (i - 1))
        and v2 = path.(w (i + 1)) in
        let v1 = if h2 < h1 then V.lerp vc v1 (h2 /. h1) else v1
        and v2 = if h1 < h2 then V.lerp vc v2 (h1 /. h2) else v2 in
        V.(sdiv (sub v2 v1) (2. *. Float.min h1 h2))
      in
      if closed
      then calc
      else
        function
        | 0 -> V.(sdiv (sub path.(1) path.(0)) h.(0))
        | i when i = len - 1 -> V.(sdiv (sub path.(i) path.(i - 1)) h.(i - 1))
        | i -> calc i
    in
    List.init len f

  let tangents ?(uniform = true) ?(closed = false) path =
    ( if uniform
      then deriv ~closed path
      else deriv_nonuniform ~closed ~h:(segment_lengths ~closed path) path )
    |> List.map V.normalize

  let continuous_closest_point
    ?(closed = false)
    ?(n_steps = 15)
    ?(max_err = 0.01)
    path_f
    p
    =
    let step = 1. /. Float.of_int n_steps in
    let wrap u = if u < 0. then 1. +. u else if u > 1. then u -. 1. else u in
    let rec aux start_u end_u =
      let minima_ranges =
        let us =
          let f i = Math.lerp start_u end_u (step *. Float.of_int i) in
          Array.init (n_steps + 1) f
        in
        let ps = Array.map (fun u -> path_f @@ wrap u) us in
        let len = Array.length ps in
        let f i acc =
          let i = len - 1 - i in
          let d1 = V.distance ps.(i - 2) p
          and d2 = V.distance ps.(i - 1) p
          and d3 = V.distance ps.(i) p in
          if d2 <= d1 && d2 <= d3 then (us.(i - 2), us.(i)) :: acc else acc
        in
        Util.fold_init (len - 2) f []
      in
      match minima_ranges with
      | [ (a, b) ] when V.distance (path_f a) (path_f b) < max_err -> (a +. b) /. 2.
      | [ (a, b) ] -> aux a b
      | (a1, b1) :: tl ->
        let f (min_u, min_dist) (a, b) =
          let u = aux a b in
          let d = V.distance (path_f u) p in
          if d < min_dist then u, d else min_u, min_dist
        in
        fst @@ List.fold_left f (f (0., Float.max_float) (a1, b1)) tl
      | [] ->
        failwith
        @@ Printf.sprintf
             "Failure to find minima, consider increasing n_steps from %i."
             n_steps
    in
    wrap @@ if closed then aux (-0.5) 1.5 else aux 0. 1.

  let segment ?(closed = false) = function
    | [] | [ _ ] -> invalid_arg "Cannot segment path with fewer than 2 points."
    | [ _; _ ] when closed -> invalid_arg "Path of length 2 cannot be closed."
    | hd :: tl ->
      let f (a, segs) b = b, V.{ a; b } :: segs in
      let last, segs = List.fold_left f (hd, []) tl in
      List.rev @@ if closed then V.{ a = last; b = hd } :: segs else segs

  let reindex_polygon reference poly =
    let ref' = Array.of_list reference
    and poly' = Array.of_list poly in
    let len = Array.length ref' in
    if len <> Array.length poly' then invalid_arg "Polygons must have the same length.";
    let idx =
      let min_dist = ref Float.max_float
      and min_idx = ref 0
      and dist = ref 0. in
      for i = 0 to len - 1 do
        for j = 0 to len - 1 do
          dist := !dist +. V.distance ref'.(j) poly'.((i + j) mod len)
        done;
        if !dist < !min_dist
        then (
          min_dist := !dist;
          min_idx := i );
        dist := 0.
      done;
      !min_idx
    in
    List.init len (fun i -> poly'.(Util.index_wrap ~len (idx + i)))

  let lerp a b u =
    try List.map2 (fun a b -> V.lerp a b u) a b with
    | Invalid_argument _ ->
      invalid_arg "Cannot interpolate between paths of unequal length."
end
