let fa = 12. *. Float.pi /. 180.
let fs = 2.
let epsilon = 1e-9
let index_wrap ~len i = ((i mod len) + len) mod len

let rev_array arr =
  let open Array in
  let swap i j =
    let tmp = get arr i in
    set arr i (get arr j);
    set arr j tmp
  in
  let i = ref 0
  and j = ref (length arr - 1) in
  while !i < !j do
    swap !i !j;
    incr i;
    decr j
  done

let array_of_list_rev l =
  match l with
  | [] -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len hd
    and r = ref tl in
    (* Start at [len - 2] as [make] has placed [hd] at [t.(len - 1)]. *)
    for i = len - 2 downto 0 do
      match !r with
      | [] -> assert false
      | hd :: tl ->
        a.(i) <- hd;
        r := tl
    done;
    a

let array_of_list_map f l =
  match l with
  | [] -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len (f hd)
    and r = ref tl in
    for i = 1 to len - 1 do
      match !r with
      | [] -> assert false
      | hd :: tl ->
        a.(i) <- f hd;
        r := tl
    done;
    a

let array_of_list_rev_map f l =
  match l with
  | [] -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len (f hd)
    and r = ref tl in
    for i = len - 2 downto 0 do
      match !r with
      | [] -> assert false
      | hd :: tl ->
        a.(i) <- f hd;
        r := tl
    done;
    a

let array_of_list_mapi f l =
  match l with
  | [] -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len (f 0 hd)
    and r = ref tl in
    for i = 1 to len - 1 do
      match !r with
      | [] -> assert false
      | hd :: tl ->
        a.(i) <- f i hd;
        r := tl
    done;
    a

let array_of_list_rev_mapi f l =
  match l with
  | [] -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len (f 0 hd)
    and r = ref tl in
    for i = len - 2 downto 0 do
      match !r with
      | [] -> assert false
      | hd :: tl ->
        a.(i) <- f i hd;
        r := tl
    done;
    a

let array_find_mapi f a =
  let len = Array.length a
  and res = ref None
  and i = ref 0 in
  while Option.is_none !res && !i < len do
    res := f !i a.(!i);
    incr i
  done;
  !res

let flatten_array m =
  let size = Array.fold_left (fun s r -> s + Array.length r) 0 m in
  if size > 0
  then (
    let first = ref None
    and start = ref 0
    and i = ref 0 in
    while Option.is_none !first do
      if Array.length m.(!start) > 0 then first := Some m.(!start).(0) else incr i
    done;
    let v = Array.make size (Option.get !first) in
    for j = !start to Array.length m - 1 do
      let row = m.(j) in
      for k = 0 to Array.length row - 1 do
        v.(!i) <- row.(k);
        incr i
      done
    done;
    v )
  else [||]

let array_all_equal f a =
  let len = Array.length a in
  if len < 2 then true else Array.for_all (f a.(0)) a

let value_map_opt ~default f = function
  | Some a -> f a
  | None -> default

let unzip l =
  let rec loop l1 l2 = function
    | [] -> l1, l2
    | (h1, h2) :: tl -> loop (h1 :: l1) (h2 :: l2) tl
  in
  loop [] [] (List.rev l)

let unzip_array a =
  let len = Array.length a in
  if len = 0
  then [||], [||]
  else (
    let h1, h2 = a.(0) in
    let a1 = Array.make len h1
    and a2 = Array.make len h2 in
    for i = 1 to len - 1 do
      let h1, h2 = a.(i) in
      a1.(i) <- h1;
      a2.(i) <- h2
    done;
    a1, a2 )

let fold_init n f init =
  let rec loop acc i = if i < n then loop (f i acc) (i + 1) else acc in
  loop init 0

let prepend_init n f init = fold_init n (fun i acc -> f i :: acc) init

let fold3 f init l1 l2 l3 =
  let rec loop acc l1 l2 l3 =
    match l1, l2, l3 with
    | [], [], [] -> acc
    | h1 :: t1, h2 :: t2, h3 :: t3 -> loop (f acc h1 h2 h3) t1 t2 t3
    | _ -> invalid_arg "Util.fold3: Unequal lengths."
  in
  loop init l1 l2 l3

let last_element = function
  | [] -> invalid_arg "No last element in empty list."
  | hd :: tl -> List.fold_left (fun _ e -> e) hd tl

let prepend_opt opt l =
  match opt with
  | Some a -> a :: l
  | None -> l

let helix_arc_length ~height ~r twist =
  twist *. Float.(sqrt ((r *. r) +. pow (height /. twist) 2.))

(* Based on:
https://github.com/openscad/openscad/blob/5e1a7cddd26de6fcfee753ee1d0fde5c90f785cd/src/calc.cc#L72 *)
let helical_slices ?fn ?(fa = fa) twist =
  let twist = Float.abs twist in
  let min_slices = Int.max 1 Float.(twist /. (pi /. 3.) |> ceil |> to_int) in
  ( match fn with
  | Some n -> Float.(Float.of_int n *. twist /. (2. *. pi) |> ceil |> to_int)
  | None -> Float.(to_int @@ ceil (twist /. fa)) )
  |> Int.max min_slices

let helical_fragments ?fn ?(fa = fa) ?(fs = fs) radius =
  match fn with
  | Some n -> Int.max 3 n
  | None ->
    Float.(to_int @@ max (ceil @@ min (2. *. pi /. fa) (radius *. pi *. 2. /. fs)) 5.)

let getter ~len ~name = function
  | `Flat n -> Fun.const n
  | `Mix l ->
    let a = Array.of_list l in
    if Array.length a <> len
    then
      invalid_arg
      @@ Printf.sprintf
           "`Mix %s entries (%i) do not match the number of transitions (%i)"
           name
           (Array.length a)
           len;
    Array.get a
