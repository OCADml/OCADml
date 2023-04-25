type resampler =
  [ `Direct of [ `ByLen | `BySeg ]
  | `Reindex of [ `ByLen | `BySeg ]
  ]

type duplicator =
  [ `Distance
  | `FastDistance
  | `Tangent
  ]

type mapping =
  [ resampler
  | duplicator
  ]

let is_direct = function
  | `Direct _ -> true
  | _ -> false

let is_resampler = function
  | `Direct _ | `Reindex _ -> true
  | _ -> false

let is_duplicator = function
  | `Distance | `FastDistance | `Tangent -> true
  | _ -> false

let linear_transition ~fn ~init a b =
  let step = 1. /. Float.of_int fn
  and lerps =
    try List.map2 V3.lerp a b with
    | Invalid_argument _ -> invalid_arg "Profiles must have equal length."
  in
  let f j acc =
    let u = Float.of_int j *. step in
    List.map (fun lerp -> lerp u) lerps :: acc
  in
  Util.fold_init fn f init

let slice_profiles ?(looped = false) ~slices = function
  | [] | [ _ ] -> invalid_arg "Too few profiles to slice."
  | hd :: tl as profs ->
    let len = List.length profs in
    let get_slices =
      Util.getter ~len:(len - if looped then 0 else 1) ~name:"slice" slices
    in
    let f (init, last, i) next =
      let acc = linear_transition ~fn:(get_slices i + 1) ~init last next in
      acc, next, i + 1
    in
    let profiles, last, i = List.fold_left f ([], hd, 0) tl in
    if looped
    then (
      let profiles, _, _ = f (profiles, last, i) hd in
      List.rev profiles )
    else List.rev (last :: profiles)

let skin
  ?(style = `MinEdge)
  ?(endcaps = `Both)
  ?refine
  ?(mapping = `Flat (`Direct `ByLen))
  ~slices
  = function
  | [] | [ _ ] -> invalid_arg "At least two profiles are required to skin."
  | profs ->
    let refine = Option.bind refine (fun n -> if n > 1 then Some n else None)
    and looped, bot_cap, top_cap =
      match endcaps with
      | `Both -> false, true, true
      | `Loop -> true, false, false
      | `Bot -> false, true, false
      | `Top -> false, false, true
      | `None -> false, false, false
    and resample n s = Path3.subdivide ~closed:true ~freq:(`N (n, s))
    and profs = Array.of_list profs in
    let n_profs = Array.length profs in
    let n_transitions = n_profs - if looped then 0 else 1 in
    let get_mapping = Util.getter ~len:n_transitions ~name:"mapping" mapping
    and n =
      let max = Array.fold_left (fun mx l -> Int.max (List.length l) mx) 0 profs in
      Util.value_map_opt ~default:max (fun r -> r * max) refine
    and all_resamplers =
      match mapping with
      | `Flat (`Direct _ | `Reindex _) -> true
      | `Mix l -> List.for_all is_resampler l
      | _ -> false
    in
    let len_sliced, sliced =
      if all_resamplers
      then (
        (* there are no duplicators, so all profiles can be handled together. *)
        let unpack_resampler i =
          match get_mapping i with
          | `Direct sampling -> true, sampling
          | `Reindex sampling -> false, sampling
          | _ -> failwith "impossible"
        in
        let f i (acc, last_p) =
          let direct, sampling = unpack_resampler i in
          let resampled = resample n sampling profs.(i + 1) in
          if direct
          then resampled :: acc, resampled
          else Path3.reindex_polygon last_p resampled :: acc, resampled
        and resampled_hd = resample n (snd @@ unpack_resampler 0) profs.(0) in
        let fixed_hd =
          if looped
          then (
            let direct, samp = unpack_resampler (n_profs - 1) in
            if not direct
            then Path3.reindex_polygon (resample n samp profs.(n_profs - 1)) resampled_hd
            else resampled_hd )
          else resampled_hd
        in
        let fixed =
          let l, _ = Util.fold_init (n_profs - 1) f ([ resampled_hd ], resampled_hd) in
          List.rev @@ if looped then fixed_hd :: l else l
        in
        1, [ slice_profiles ~looped:false ~slices fixed ] )
      else (
        let get_slices = Util.getter ~len:n_transitions ~name:"slices" slices in
        (* This is likely to change, but it is my attempt to support having
              sampling method on a per resampler basis, while also navigating the
              transitions between resamplers and duplicators. It may not really be possible
              to achieve something flexible and intuitive though, so transitioning to
              something more similar to BOSL2s solution is still in the cards. *)
        let up =
          let fallback i p =
            match get_mapping (Util.index_wrap ~len:n_profs (i - 1)) with
            | `Direct sampling | `Reindex sampling -> resample n sampling p
            | _ -> resample n `BySeg p
          in
          let f i p =
            if i < n_transitions || looped
            then (
              match get_mapping i with
              | `Direct sampling | `Reindex sampling -> resample n sampling p
              | _ -> if i > 0 || looped then fallback i p else resample n `BySeg p )
            else fallback i p
          in
          Array.mapi f profs
        and upsample_dups (a, b) = [ resample n `BySeg a; resample n `BySeg b ] in
        let f i acc =
          let j = (i + 1) mod n_profs in
          let pair =
            (* resamplers are upsampled before alignment, duplicators are upsampled after *)
            match get_mapping (Util.index_wrap ~len:n_profs i) with
            | `Direct _ -> [ up.(i); up.(j) ]
            | `Reindex _ -> [ up.(i); Path3.reindex_polygon up.(i) up.(j) ]
            | `Distance -> upsample_dups @@ Path3.distance_match profs.(i) profs.(j)
            | `FastDistance ->
              upsample_dups @@ Path3.aligned_distance_match profs.(i) profs.(j)
            | `Tangent -> upsample_dups @@ Path3.tangent_match profs.(i) profs.(j)
          in
          slice_profiles ~slices:(`Flat (get_slices i)) pair :: acc
        in
        n_transitions, List.rev @@ Util.fold_init n_transitions f [] )
    in
    let f (i, acc) rows =
      let endcaps =
        match bot_cap, top_cap with
        | true, true when i = 0 && i = len_sliced - 1 -> `Both
        | true, _ when i = 0 -> `Bot
        | _, true when i = len_sliced - 1 -> `Top
        | _ -> `None
      in
      i + 1, Mesh0.of_rows ~style ~endcaps rows :: acc
    in
    Mesh0.join @@ snd @@ List.fold_left f (0, []) sliced

let skin_between ?style ?endcaps ?refine ?mapping:(m = `Direct `ByLen) ~slices:s a b =
  skin ?style ?refine ~mapping:(`Flat m) ?endcaps ~slices:(`Flat s) [ a; b ]

let skline
  ?(style = `MinEdge)
  ?(endcaps = `Both)
  ?refine
  ?(mapping = `Flat (`Direct `ByLen))
  ?(fn = 36)
  ?size
  ?tangents
  = function
  | [] | [ _ ] -> invalid_arg "At least two profiles are required to skin."
  | profs ->
    let refine = Option.bind refine (fun n -> if n > 1 then Some n else None)
    and looped, bot_cap, top_cap =
      match endcaps with
      | `Both -> false, true, true
      | `Loop -> true, false, false
      | `Bot -> false, true, false
      | `Top -> false, false, true
      | `None -> false, false, false
    and resample n s = Path3.subdivide ~closed:true ~freq:(`N (n, s))
    and profs = Array.of_list profs in
    let n_profs = Array.length profs in
    let n_transitions = n_profs - if looped then 0 else 1 in
    let prof_lens = Array.map List.length profs in
    let get_mapping = Util.getter ~len:n_transitions ~name:"mapping" mapping
    and n =
      let max = Array.fold_left (fun mx len -> Int.max len mx) 0 prof_lens in
      Util.value_map_opt ~default:max (fun r -> r * max) refine
    and all_resamplers =
      match mapping with
      | `Flat (`Direct _) -> true
      | `Mix l -> List.for_all is_direct l
      | _ -> false
    and get_prof_len i = prof_lens.(Util.index_wrap ~len:n_profs i) in
    let len_sliced, sliced =
      if all_resamplers
      then (
        (* there are no duplicators, so all profiles can be handled together. *)
        let unpack_sampling i =
          match get_mapping i with
          | `Direct sampling -> sampling
          | _ -> failwith "impossible"
        in
        let resampled_hd = Array.of_list @@ resample n (unpack_sampling 0) profs.(0) in
        let fixed =
          let a = Array.make (n_profs + Bool.to_int looped) resampled_hd in
          for i = 1 to n_profs - 1 do
            a.(i) <- Array.of_list @@ resample n (unpack_sampling (i - 1)) profs.(i)
          done;
          a
        in
        let layers =
          let n_profs = Array.length fixed
          and n_bezs = Array.length fixed.(0) in
          let bezs =
            let f i =
              Bezier3.of_path ?size ?tangents (List.init n_profs (fun j -> fixed.(j).(i)))
            in
            Array.init n_bezs f
          in
          let s = 1. /. Float.of_int fn in
          let f i = List.init n_bezs (fun j -> bezs.(j) (Float.of_int i *. s)) in
          List.init fn f
        in
        1, [ layers ] )
      else (
        (* TODO:
    Need to alter this to a scheme where all of the profiles can be handled at
    once like above (then the bezier creation will not need to be duplicated).
    This likely will mean that certain transitions will probably not be allowed.
    Should check the way that BOSL2 does it again. In order for the curves to be
    continuous between the profiles, the resamplings/duplications can't rely on
    being joined as they are now.

    See https://github.com/revarbat/BOSL2/blob/master/skin.scad#L468
    A transition profile is where there is a switch between resampler and duplicator.
    Need to read more closely, but as the assert says, this scheme rejects when
    there is a profile length mismatch at the transitions (to make sure that the
    points line up (duplicated points are fine as long as they are in the same
    spot)). Unfortunately, this is not good enough for me, as they still do the
    slicing separately for each transition. This bezier skinning will only work
    if the points can be connected in continuous curves through all the profiles.

   With that in mind, the constraints of skline with duplicators are:
   - if the first of a pair has points added to match up with the second
     the second cannot have its point count change during the next
     pairing (via duplication or sampling).
   - the next pair could have its second part change to match with the
     second of the first
   - thus, a general rule is two profiles in a row can't have their
     point counts change (the next must be a profile that is equal to or
     greater than the number of points than the one following it)
   - when looping, the final version of the first profile must be
     identical, such that Bezier3.of_path can be called with ~closed:true
   - NOTE: the larger path of a pair is actually reindexed by point rotation by
     duplicators (except in aligned_distance_match which assumes that they are
     already lined up -- there is no aligned version of tangent_match
     unfortunately).
   - therefore, the above mentioned constraints are not adequate. I need to
     ignore the reindexed big profile returned by tangent_match (and make a note
     that this is the behaviour in the doc comment)
        *)
        let _num, _layers =
          let last_duped =
            if looped && (not @@ is_direct (get_mapping (n_profs - 1)))
            then ref (Some (get_prof_len (n_profs - 1) > get_prof_len 0))
            else ref None
          in
          for i = 1 to n_profs - 2 do
            if not @@ is_direct (get_mapping (i - 1))
            then (
              let duped = get_prof_len (i - 1) > get_prof_len i in
              if duped
                 && Option.value ~default:false !last_duped
                 && get_prof_len i < get_prof_len (i + 1)
              then invalid_arg "skline: same profile cannot be duplicated twice";
              last_duped := Some duped )
            else last_duped := None
          done;
          (* TODO:
                 - need to check lengths of the profiles and make sure that
                   there won't be any illegal changes (anything involved in a
                   duplicator cannot be involved in another transition taht would require
                   adding to its points)
                 - handle the first profile resampling appropriately (depends on
                   the last if it is looped, otherwise it is the same as usual) *)
          let unpack_sampling i =
            match get_mapping i with
            | `Direct sampling -> sampling
            | _ -> failwith "impossible"
          in
          let fixed =
            let a = Array.make (n_profs + Bool.to_int looped) [||] in
            for i = 1 to n_profs - 1 do
              a.(i) <- Array.of_list @@ resample n (unpack_sampling (i - 1)) profs.(i)
            done;
            if looped then a.(n_profs) <- a.(0);
            a
          in
          let layers =
            let n_profs = Array.length fixed
            and n_bezs = Array.length fixed.(0) in
            let bezs =
              let f i =
                Bezier3.of_path
                  ?size
                  ?tangents
                  (List.init n_profs (fun j -> fixed.(j).(i)))
              in
              Array.init n_bezs f
            in
            let s = 1. /. Float.of_int fn in
            let f i = List.init n_bezs (fun j -> bezs.(j) (Float.of_int i *. s)) in
            List.init fn f
          in
          1, [ layers ]
        in
        let up =
          let fallback i p =
            match get_mapping (Util.index_wrap ~len:n_profs (i - 1)) with
            | `Direct sampling -> resample n sampling p
            | _ -> resample n `BySeg p
          in
          let f i p =
            if i < n_transitions || looped
            then (
              match get_mapping i with
              | `Direct sampling -> resample n sampling p
              | _ -> if i > 0 || looped then fallback i p else resample n `BySeg p )
            else fallback i p
          in
          Array.mapi f profs
        and upsample_dups (a, b) = [ resample n `BySeg a; resample n `BySeg b ] in
        let f i acc =
          let j = (i + 1) mod n_profs in
          let pair =
            (* resamplers are upsampled before alignment, duplicators are upsampled after *)
            match get_mapping i with
            | `Direct _ -> [ up.(i); up.(j) ]
            | `FastDistance ->
              upsample_dups @@ Path3.aligned_distance_match profs.(i) profs.(j)
            | `DirectTangent -> upsample_dups @@ Path3.tangent_match profs.(i) profs.(j)
          in
          (* slice_profiles ~slices:(`Flat (get_slices i)) pair :: acc *)
          slice_profiles ~slices:(`Flat fn) pair :: acc
        in
        n_transitions, List.rev @@ Util.fold_init n_transitions f [] )
    in
    let f (i, acc) rows =
      let endcaps =
        match bot_cap, top_cap with
        | true, true when i = 0 && i = len_sliced - 1 -> `Both
        | true, _ when i = 0 -> `Bot
        | _, true when i = len_sliced - 1 -> `Top
        | _ -> `None
      in
      i + 1, Mesh0.of_rows ~style ~endcaps rows :: acc
    in
    Mesh0.join @@ snd @@ List.fold_left f (0, []) sliced
