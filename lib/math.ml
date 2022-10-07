let deg_of_rad r = 180.0 *. r /. Float.pi
let rad_of_deg d = d *. Float.pi /. 180.
let sign a = Float.(of_int @@ compare a 0.)
let clamp ~min ~max a = if a < min then min else if a > max then max else a
let lerp a b u = if u = 0. then a else if u = 1. then b else ((1. -. u) *. a) +. (u *. b)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then Int.max 1 (n - 1) else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let quant ~q v = Float.floor ((v /. q) +. 0.5) *. q
let quant_down ~q v = Float.floor (v /. q) *. q
let quant_up ~q v = Float.ceil (v /. q) *. q

let approx ?(eps = Util.epsilon) a b =
  not (Int.equal Float.(compare (abs (a -. b)) eps) 1)

let law_of_cosines a b c =
  Float.acos
  @@ clamp ~min:(-1.) ~max:1. (((a *. a) +. (b *. b) -. (c *. c)) /. (2. *. a *. b))

let posmod a m = ((a mod m) + m) mod m

let mat_dims m =
  let n_rows = Array.length m in
  let n_cols = if n_rows = 0 then 0 else Array.length m.(0) in
  if Array.for_all (fun c -> Array.length c = n_cols) m
  then n_rows, n_cols
  else invalid_arg "mat_dims: Matrix has ragged rows."

let matmul a =
  let a_rows, a_cols = mat_dims a in
  fun b ->
    let b_rows, b_cols = mat_dims b in
    if a_cols <> b_rows
    then
      invalid_arg
        (Printf.sprintf "matmul: Inner dims do not match (%i x %i)" a_cols b_rows);
    let out = Array.make_matrix a_rows b_cols 0. in
    for i = 0 to a_rows - 1 do
      for j = 0 to b_cols - 1 do
        for k = 0 to b_rows - 1 do
          out.(i).(j) <- out.(i).(j) +. (a.(i).(k) *. b.(k).(j))
        done
      done
    done;
    out

let transpose a =
  let n_rows, n_cols = mat_dims a in
  let m = Array.make_matrix n_cols n_rows a.(0).(0) in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      m.(j).(i) <- a.(i).(j)
    done
  done;
  m

(* Removes the leading zero terms of a polynomial. *)
let poly_trim_head p =
  let i = ref 0
  and first = ref None
  and len = Array.length p in
  while Option.is_none !first && !i < len do
    if p.(!i) = 0. then incr i else first := Some !i
  done;
  match !first with
  | Some first -> Array.init (len - first) (fun i -> p.(i + first))
  | None       -> [| 0. |]

(* Removes the leading and trailing zero terms of a polynomial. *)
let poly_trim_head_tail p =
  let len = Array.length p in
  let i = ref 0
  and j = ref (len - 1)
  and first = ref None
  and last = ref None in
  while (Option.is_none !first || Option.is_none !last) && !i < len do
    if Option.is_none !first && p.(!i) <> 0. then first := Some !i else incr i;
    if Option.is_none !last && p.(!j) <> 0. then last := Some !j else decr j
  done;
  match !first, !last with
  | Some first, Some last -> Array.init (last - first + 1) (fun i -> p.(i + first))
  | _                     -> [| 0. |]

(* Evaluates the real polynomial p at the real input value z. *)
let polynomial_real p z =
  let f total x = (total *. z) +. x in
  Array.fold_left f 0. (poly_trim_head p)

(* Evaluates the real polynomial p at the complex input value z. *)
let polynomial_complex p z =
  let f total re = Complex.(add (mul total z) { re; im = 0. }) in
  Array.fold_left f Complex.zero (poly_trim_head p)

(* Returns all complex roots of the real polynomial p.
   Adapted from: https://github.com/revarbat/BOSL2/blob/master/math.scad#L1418 *)
let poly_roots ?(tol = 1e-14) p =
  let p = poly_trim_head_tail p in
  if Array.for_all (( = ) 0.) p then invalid_arg "Input polynomial cannot be zero.";
  let n = Array.length p - 1 in
  (* polynomial degree *)
  if n = 0
  then [||], [||]
  else if n = 1
  then [| Complex.{ re = -.p.(1) /. p.(0); im = 0. } |], [| 0. |]
  else (
    let p0 = p.(0)
    and p1 = p.(1) in
    let p_deriv = Array.init n (fun i -> p.(i) *. Float.of_int (n - i)) in
    let s =
      Array.init (n + 1) (fun i ->
          Float.abs p.(i) *. ((4. *. Float.of_int (n - i)) +. 1.) )
    and beta = -.p1 /. p0 /. Float.of_int n in
    let z =
      let r =
        let poly = polynomial_real p beta in
        1. +. Float.(pow (abs (poly /. p0)) (1. /. of_int n))
      in
      let f i =
        let angle = Float.((pi *. 2. *. (of_int i /. of_int n)) +. (1.5 /. of_int n)) in
        Complex.(
          add { re = beta; im = 0. } Float.{ re = cos angle *. r; im = sin angle *. r })
      in
      Array.init n f
    in
    let i = ref 0
    and complete = Array.make n false
    and n_complete = ref 0
    and z_diff = ref Complex.zero in
    while !n_complete < n && !i < 45 do
      for j = 0 to n - 1 do
        if not complete.(j)
        then (
          let sval = tol *. polynomial_real s (Complex.norm z.(j))
          and p_of_z = polynomial_complex p z.(j) in
          if Complex.norm p_of_z <= sval
          then (
            complete.(j) <- true;
            incr n_complete )
          else (
            let newton = Complex.div p_of_z (polynomial_complex p_deriv z.(j)) in
            for k = 0 to n - 1 do
              if j <> k then z_diff := Complex.(add !z_diff (div one (sub z.(j) z.(k))))
            done;
            let w = Complex.(div newton (sub one (mul newton !z_diff))) in
            z_diff := Complex.zero;
            z.(j) <- Complex.sub z.(j) w ) )
      done;
      incr i
    done;
    if !n_complete < n then failwith "poly_roots exceeded iteration limit.";
    let error =
      let f xi =
        let num =
          Complex.norm (polynomial_complex p xi)
          +. (tol *. polynomial_real s (Complex.norm xi))
        and denom =
          Float.abs
            ( Complex.norm (polynomial_complex p_deriv xi)
            -. (tol *. polynomial_real s (Complex.norm xi)) )
        in
        Float.of_int n *. num /. denom
      in
      Array.map f z
    in
    z, error )

let real_roots ?eps ?(tol = 1e-14) p =
  let p = poly_trim_head p in
  let roots, errors = poly_roots ~tol p in
  let f =
    match eps with
    | Some eps ->
      fun (_, acc) (Complex.{ re; im } as z) ->
        if Float.abs im /. (1. +. Complex.norm z) < eps then 0, re :: acc else 0, acc
    | None     ->
      fun (i, acc) Complex.{ re; im } ->
        if Float.abs im <= errors.(i) then i + 1, re :: acc else i + 1, acc
  in
  let _, l = Array.fold_left f (0, []) roots in
  Util.array_of_list_rev l

let bisection ?(max_iter = 100) ?(tolerance = 0.001) ~lower ~upper f =
  let rec loop i a b =
    let c = (a +. b) /. 2. in
    if (b -. a) /. 2. < tolerance
    then c
    else (
      let res = f c in
      if res = 0.
      then c
      else if i < max_iter
      then
        if Float.(Int.equal (compare 0. res) (compare 0. (f a)))
        then loop (i + 1) c b
        else loop (i + 1) a c
      else failwith "Maximum iterations reached in bisection search." )
  in
  loop 0 lower upper
