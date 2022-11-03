open! V

let normal p =
  let len = Array.length p in
  if len < 3 then invalid_arg "Too few points to calculate path normal.";
  let area = ref V3.zero in
  for i = 2 to len - 1 do
    area := V3.(add !area @@ cross (sub p.(i - 1) p.(0)) (sub p.(i) p.(i - 1)))
  done;
  V3.(normalize @@ neg !area)
