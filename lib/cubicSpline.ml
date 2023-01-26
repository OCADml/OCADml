open V

type boundary =
  [ `Quadratic
  | `NotAKnot
  | `Periodic
  | `Natural
  ]

type coef =
  { a : float
  ; b : float
  ; c : float
  ; d : float
  }

type t =
  { len : int
  ; xmins : float array
  ; xmaxs : float array
  ; coefs : coef array
  }

let zero_coefs = { a = 0.; b = 0.; c = 0.; d = 0. }
let len t = t.len
let xmins t = Array.to_list t.xmins
let xmaxs t = Array.to_list t.xmaxs
let coefs t = Array.to_list t.coefs
let get_xmin_exn t i = Array.get t.xmins i
let get_xmax_exn t i = Array.get t.xmaxs i
let get_coef_exn t i = Array.get t.coefs i
let in_bounds t i = i >= 0 && i < t.len
let get_xmin t i = if in_bounds t i then Some (Array.get t.xmins i) else None
let get_xmax t i = if in_bounds t i then Some (Array.get t.xmaxs i) else None
let get_coef t i = if in_bounds t i then Some (Array.get t.coefs i) else None

let coef_to_string { a; b; c; d } =
  Printf.sprintf "{ a = %f; b = %f; c = %f; d = %f }" a b c d

(* Reduced row echelon form
   Taken from https://rosettacode.org/wiki/Reduced_row_echelon_form *)
let rref m =
  let n_row, n_col = Array.length m, Array.length m.(0) in
  let add r s k =
    for i = 0 to n_col - 1 do
      m.(r).(i) <- m.(r).(i) +. (m.(s).(i) *. k)
    done
  in
  for c = 0 to min (n_col - 1) (n_row - 1) do
    for r = c + 1 to n_row - 1 do
      if Float.abs m.(c).(c) < Float.abs m.(r).(c)
      then (
        let v = m.(r) in
        m.(r) <- m.(c);
        m.(c) <- v )
    done;
    let t = m.(c).(c) in
    if t <> 0.0
    then (
      for r = 0 to n_row - 1 do
        if r <> c then add r c (-.m.(r).(c) /. t)
      done;
      for i = 0 to n_col - 1 do
        m.(c).(i) <- m.(c).(i) /. t
      done )
  done

(* Implementation based on:
   https://github.com/Simsso/Online-Tools/blob/master/src/page/logic/cubic-spline-interpolation.js *)
let fit ?(boundary = `Natural) ps =
  let ps =
    let f (p1 : V2.t) (p2 : V2.t) = V2.(Float.compare (x p1) (x p2)) in
    Array.of_list @@ List.sort_uniq f ps
  and row = ref 0 in
  let len = Array.length ps in
  let solution_idx = (len - 1) * 4 in
  let m = Array.make_matrix solution_idx (solution_idx + 1) 0. in
  (* splines through p equations *)
  for n = 0 to len - 2 do
    let r = m.(!row)
    and n4 = n * 4
    and p0 = ps.(n)
    and p1 = ps.(n + 1) in
    let () =
      r.(n4) <- Float.pow (V2.x p0) 3.;
      r.(n4 + 1) <- Float.pow (V2.x p0) 2.;
      r.(n4 + 2) <- V2.x p0;
      r.(n4 + 3) <- 1.;
      r.(solution_idx) <- V2.y p0;
      incr row
    in
    let r = m.(!row) in
    r.(n4) <- Float.pow (V2.x p1) 3.;
    r.(n4 + 1) <- Float.pow (V2.x p1) 2.;
    r.(n4 + 2) <- V2.x p1;
    r.(n4 + 3) <- 1.;
    r.(solution_idx) <- V2.y p1;
    incr row
  done;
  (* first derivative *)
  for n = 0 to len - 3 do
    let x1 = V2.x ps.(n + 1)
    and r = m.(!row)
    and n4 = n * 4 in
    r.(n4) <- 3. *. Float.pow x1 2.;
    r.(n4 + 1) <- 2. *. x1;
    r.(n4 + 2) <- 1.;
    r.(n4 + 4) <- -3. *. Float.pow x1 2.;
    r.(n4 + 5) <- -2. *. x1;
    r.(n4 + 6) <- -1.;
    incr row
  done;
  (* second derivative *)
  for n = 0 to len - 3 do
    let x1 = V2.x ps.(n + 1)
    and r = m.(!row)
    and n4 = n * 4 in
    r.(n4) <- 6. *. x1;
    r.(n4 + 1) <- 2.;
    r.(n4 + 4) <- -6. *. x1;
    r.(n4 + 5) <- -2.;
    incr row
  done;
  (* boundary conditions *)
  let () =
    match boundary with
    | `Quadratic ->
      (* first and last spline quadratic *)
      m.(!row).(0) <- 1.;
      incr row;
      m.(!row).(solution_idx - 4) <- 1.
    | `NotAKnot ->
      let r = m.(!row) in
      let () =
        r.(0) <- 1.;
        r.(4) <- -1.;
        incr row
      in
      let r = m.(!row) in
      r.(solution_idx - 8) <- 1.;
      r.(solution_idx - 4) <- -1.
    | `Periodic ->
      (* first derivative of first and last point equal *)
      let r = m.(!row)
      and x0 = V2.x ps.(0)
      and xn = V2.x ps.(len - 1) in
      let () =
        r.(0) <- 3. *. Float.pow x0 2.;
        r.(1) <- 2. *. x0;
        r.(2) <- 1.;
        r.(solution_idx - 4) <- -3. *. Float.pow xn 2.;
        r.(solution_idx - 3) <- -2. *. xn;
        r.(solution_idx - 2) <- -1.;
        incr row
      in
      (* second derivative of first and last point equal *)
      let r = m.(!row) in
      r.(0) <- 6. *. x0;
      r.(1) <- 2.;
      r.(solution_idx - 4) <- -6. *. xn;
      r.(solution_idx - 3) <- -2.
    | `Natural ->
      let r = m.(!row)
      and x0 = V2.x ps.(0)
      and xn = V2.x ps.(len - 1) in
      let () =
        r.(0) <- 6. *. x0;
        r.(1) <- 2.;
        incr row
      in
      let r = m.(!row) in
      r.(solution_idx - 4) <- 6. *. xn;
      r.(solution_idx - 3) <- 2.
  in
  let xmins = Array.make (len - 1) 0.
  and xmaxs = Array.make (len - 1) 0.
  and coefs = Array.make (len - 1) zero_coefs in
  rref m;
  for i = 0 to len - 2 do
    let idx = i * 4 in
    xmins.(i) <- V2.x ps.(i);
    xmaxs.(i) <- V2.x ps.(i + 1);
    coefs.(i)
      <- { a = m.(idx).(solution_idx)
         ; b = m.(idx + 1).(solution_idx)
         ; c = m.(idx + 2).(solution_idx)
         ; d = m.(idx + 3).(solution_idx)
         }
  done;
  { len = len - 1; xmins; xmaxs; coefs }

let extrapolate { len; xmins; xmaxs; coefs } x =
  let i = ref 0
  and y = ref None in
  while Option.is_none !y && !i < len do
    let idx = !i in
    if x >= xmins.(idx) && x <= xmaxs.(idx)
    then (
      let { a; b; c; d } = coefs.(idx) in
      y := Some ((a *. x *. x *. x) +. (b *. x *. x) +. (c *. x) +. d) )
    else incr i
  done;
  !y

let extrapolate_path t xs =
  List.filter_map (fun x -> Option.map (fun y -> v2 x y) (extrapolate t x)) xs

let interpolate_path ~fn t =
  let xmin = t.xmins.(0)
  and xmax = t.xmaxs.(t.len - 1) in
  let step = (xmax -. xmin) /. Float.of_int fn in
  let f i pts =
    let x = xmin +. (Float.of_int i *. step) in
    match extrapolate t x with
    | Some y -> v2 x y :: pts
    | None -> pts
  in
  List.rev @@ Util.fold_init (fn + 1) f []
