open V

let clockwise_sign ?(eps = Util.epsilon) (ps : V2.t array) =
  let len = Array.length ps
  and sum = ref 0. in
  for i = 0 to len - 1 do
    let p1 = ps.(Util.index_wrap ~len i)
    and p2 = ps.(Util.index_wrap ~len (i + 1)) in
    sum := !sum +. ((p1.x -. p2.x) *. (p1.y +. p2.y))
  done;
  if Math.approx ~eps !sum 0. then 0. else Float.(of_int @@ compare !sum 0.)

let is_clockwise ps = Float.equal 1. (clockwise_sign ps)

let self_intersections ?(eps = Util.epsilon) ?(closed = false) path =
  let len = Array.length path in
  if len < 3
  then []
  else (
    let intersects = ref []
    and w = Util.index_wrap ~len in
    for i = 0 to len - if closed then 3 else 4 do
      let l1 = V2.{ a = path.(i); b = path.(i + 1) } in
      let seg_normal =
        let d = V2.sub l1.b l1.a in
        V2.(normalize (v (-.d.y) d.x))
      in
      let ref_v = V2.dot path.(i) seg_normal
      and last_signal = ref 0
      and start = i + 2 in
      for j = 0 to len - start - if closed then 1 else 2 do
        let v = V2.dot path.(j + start) seg_normal -. ref_v in
        if Float.abs v >= eps
        then (
          let signal = Int.of_float @@ Math.sign v in
          if signal * !last_signal <= 0
          then (
            let l2 = V2.{ a = path.(j + start); b = path.(w (j + start + 1)) } in
            let intersect =
              V2.line_intersection ~eps ~bounds1:(true, true) ~bounds2:(true, true) l1 l2
            in
            Option.iter (fun p -> intersects := p :: !intersects) intersect );
          last_signal := signal )
      done
    done;
    !intersects )

let is_simple ?eps ?(closed = false) path =
  let len = Array.length path in
  if len < 3
  then true
  else (
    let reversal = ref false
    and i = ref 0
    and last = len - if closed then 1 else 2 in
    while (not !reversal) && !i < last do
      let v1 = V2.sub path.(!i + 1) path.(!i)
      and v2 = V2.sub path.(Util.index_wrap ~len (!i + 2)) path.(!i + 1) in
      reversal := Math.approx V2.(dot v1 v2 /. norm v1 /. norm v2) (-1.);
      incr i
    done;
    if !reversal then false else List.length (self_intersections ?eps path) = 0 )
