open V
include Bezier.Make (V2)

let line_intersection ~(line : V2.line) ps =
  let ps = Array.of_list ps in
  let n = Array.length ps - 1 in
  let bez_coefs = coefs' ps
  and normal = v2 (line.a.y -. line.b.y) (line.b.x -. line.a.x) in
  let f i =
    if i = n
    then V2.(dot (sub bez_coefs.(0) line.a) normal)
    else V2.dot bez_coefs.(n - i) normal
  in
  let roots = Math.real_roots @@ Array.init (n + 1) f in
  let f i acc = if roots.(i) >= 0. && roots.(i) <= 1. then roots.(i) :: acc else acc in
  Util.fold_init (Array.length roots) f []

let translate p t u = V2.translate p (t u)
let xtrans x t u = V2.xtrans x (t u)
let ytrans y t u = V2.ytrans y (t u)
let rotate ?about r t u = V2.rotate ?about r (t u)
let[@inline] zrot ?about r t u = rotate ?about r t u
let affine m t u = Affine2.transform m (t u)
let affine3 m t u = Affine3.transform m (V3.of_v2 (t u))
let quaternion ?about q t u = Quaternion.transform ?about q (V3.of_v2 (t u))
let axis_rotate ?about ax a = quaternion ?about (Quaternion.make ax a)
let scale s t u = V2.scale s (t u)
let mirror ax t u = V2.mirror ax (t u)
