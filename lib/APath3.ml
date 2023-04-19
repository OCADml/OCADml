open! V

let normal p =
  let len = Array.length p in
  if len < 3 then invalid_arg "Too few points to calculate path normal.";
  let area = ref V3.zero in
  for i = 2 to len - 1 do
    area := V3.(add !area @@ cross (sub p.(i - 1) p.(0)) (sub p.(i) p.(i - 1)))
  done;
  V3.(normalize @@ neg !area)

let translate p = Array.map (V3.translate p)
let xtrans x = Array.map (V3.xtrans x)
let ytrans y = Array.map (V3.ytrans y)
let ztrans z = Array.map (V3.ztrans z)
let rotate ?about r = Array.map (V3.rotate ?about r)
let xrot ?about r = Array.map (V3.xrot ?about r)
let yrot ?about r = Array.map (V3.yrot ?about r)
let zrot ?about r = Array.map (V3.zrot ?about r)
let quaternion ?about q = Array.map (Quaternion.transform ?about q)
let axis_rotate ?about ax r = quaternion ?about (Quaternion.make ax r)
let affine m = Array.map (Affine3.transform m)
let scale s = Array.map (V3.scale s)
let xscale x = Array.map (V3.xscale x)
let yscale y = Array.map (V3.yscale y)
let zscale z = Array.map (V3.zscale z)
let mirror ax = Util.rev_map_array (V3.mirror ax)
