open V
open Mesh0

let cartesian_plot ~min_x ~x_steps ~max_x ~min_y ~y_steps ~max_y plot =
  let x_step = (max_x -. min_x) /. Float.of_int x_steps
  and y_step = (max_y -. min_y) /. Float.of_int y_steps in
  let max_x = max_x +. (0.001 *. x_step)
  and max_y = max_y +. (0.001 *. y_step)
  (* Stay above plane to guarantee manifold. Bottom always at 0. *)
  and min_plot = 0.0005 *. (x_step +. y_step) in
  let xs_rev =
    List.init (x_steps + 1) (fun i -> (Float.of_int (x_steps - i) *. x_step) +. min_x)
  in
  let edge y =
    v3 max_x y 0.
    :: v3 min_x y 0.
    :: List.fold_left (fun acc x -> v3 x y 0.0001 :: acc) [] xs_rev
  in
  let layers =
    let outer i layers =
      let y = (Float.of_int i *. y_step) +. min_y in
      let layer l x =
        let z = Float.max min_plot (plot ~x ~y) in
        v3 x y z :: l
      in
      (v3 max_x y 0. :: v3 min_x y 0. :: List.fold_left layer [] xs_rev) :: layers
    in
    Util.fold_init (y_steps + 1) outer [ edge (min_y -. (0.001 *. y_step)) ]
  in
  of_rows @@ (edge (max_y +. (0.001 *. y_step)) :: layers)

let polar_plot ?r_step ~max_r plot =
  let r_step, a_steps =
    match r_step with
    | Some s -> s, Float.(to_int (ceil (max_r *. 2. *. pi /. s /. 8.))) * 8
    | None   -> 2. *. Float.pi *. max_r /. 360., 360
  in
  let r_steps = Float.(to_int @@ ceil (max_r /. r_step)) in
  let r_step = (max_r -. (1e-6 *. r_step)) /. Float.of_int r_steps in
  let min_plot = 0.001 *. r_step
  and angles_rev =
    let step = 2. *. Float.pi /. Float.of_int a_steps in
    Util.fold_init (a_steps + 1) (fun i acc -> (Float.of_int i *. step) :: acc) []
  in
  let bot =
    let f ps a = Float.(v3 (max_r *. cos a) (max_r *. sin a) 0.) :: ps in
    List.fold_left f [] angles_rev
  in
  let f i layers =
    let r = max_r -. (Float.of_int i *. r_step) in
    let layer l a =
      let z = Float.max min_plot (plot ~r ~a) in
      Float.(v3 (r *. cos a) (r *. sin a) z) :: l
    in
    List.fold_left layer [] angles_rev :: layers
  in
  of_rows @@ Util.fold_init (r_steps + 1) f [ bot ]

let axial_plot ?(fn = 60) ~min_z ~z_steps ~max_z plot =
  let z_step = (max_z -. min_z) /. Float.of_int z_steps in
  let min_plot = 0.001 *. z_step
  and angles_rev =
    let step = 2. *. Float.pi /. Float.of_int fn in
    Util.fold_init (fn + 1) (fun i acc -> (Float.of_int i *. step) :: acc) []
  in
  let f i layers =
    let z = min_z +. (Float.of_int i *. z_step) in
    let layer l a =
      let r = Float.max min_plot (plot ~z ~a) in
      Float.(v3 (r *. cos a) (r *. sin a) z) :: l
    in
    List.fold_left layer [] angles_rev :: layers
  in
  of_rows @@ Util.fold_init (z_steps + 1) f []
