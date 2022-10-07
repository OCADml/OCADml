module type S = sig
  type vec

  (** Ball Tree structure. *)
  type t

  (** [t.%(idx)]

       Indexed access to vectors/points used to build the ball tree [t]. *)
  val ( .%() ) : t -> int -> vec

  (** [points t]

       Return a list of the vectors/points used to build the ball tree [t]. *)
  val points : t -> vec list

  (** [points' t]

       Return a copied array of the vectors/points used to build the ball tree
       [t]. *)
  val points' : t -> vec array

  (** [make ?leaf_size pts]

      Build a ball tree from the list of vectors [pts]. Recursive construction
      of the tree will cease branching when a node holds a number of points less
      than or equal to [leaf_size]. *)
  val make : ?leaf_size:int -> vec list -> t

  (** [make' ?leaf_size pts]

      Build a ball tree from the array of vectors [pts]. Recursive construction
      of the tree will cease branching when a node holds a number of points less
      than or equal to [leaf_size]. *)
  val make' : ?leaf_size:int -> vec array -> t

  (** [search_idxs ?radius t p]

       Search through the ball tree [t] for points that are within a distance
       [radius] of the target point [p]. Matches are returned as an arbitrarily
       ordered list of indices (into the point array used to construct [t]). *)
  val search_idxs : ?radius:float -> t -> vec -> int list

  (** [search_points ?radius t p]

       Search through the ball tree [t] for points that are within a distance
       [radius] of the target point [p]. Matches are returned as an arbitrarily
       ordered (not sorted from nearest to furthest) list of points. *)
  val search_points : ?radius:float -> t -> vec -> vec list
end

module type Projection = sig
  type vec

  val proj : vec array -> int array -> float array
end

module Proj2 = struct
  let proj points idxs =
    let mn = ref points.(idxs.(0))
    and mx = ref points.(idxs.(0))
    and len = Array.length idxs in
    for i = 1 to len - 1 do
      let V2.{ x; y } = points.(idxs.(i)) in
      (mn := Float.{ x = min !mn.x x; y = min !mn.y y });
      mx := Float.{ x = max !mx.x x; y = max !mx.y y }
    done;
    let V2.{ x = dx; y = dy } = V2.sub !mx !mn in
    let project = if dx >= dy then V2.get_x else V2.get_y in
    Array.map (fun idx -> project points.(idx)) idxs
end

module Proj3 = struct
  let proj points idxs =
    let mn = ref points.(idxs.(0))
    and mx = ref points.(idxs.(0))
    and len = Array.length idxs in
    for i = 1 to len - 1 do
      let V3.{ x; y; z } = points.(idxs.(i)) in
      (mn := Float.{ x = min !mn.x x; y = min !mn.y y; z = min !mn.z z });
      mx := Float.{ x = max !mx.x x; y = max !mx.y y; z = max !mx.z z }
    done;
    let V3.{ x = dx; y = dy; z = dz } = V3.sub !mx !mn in
    let project =
      match Float.(compare dx dy, compare dx dz, compare dy dz) with
      | 1, 1, _   -> V3.get_x
      | -1, _, 1  -> V3.get_y
      | _, -1, -1 -> V3.get_z
      | _         -> V3.get_x
    in
    Array.map (fun idx -> project points.(idx)) idxs
end

module Make (V : V.S) (P : Projection with type vec := V.t) : S with type vec := V.t =
struct
  type tree =
    | Leaf of int array
    | Node of
        { pivot : int
        ; radius : float
        ; left : tree
        ; right : tree
        }

  type t =
    { points : V.t array
    ; tree : tree
    }

  let ( .%() ) t i = t.points.(i)
  let points t = List.init (Array.length t.points) (fun i -> t.points.(i))
  let points' t = Array.copy t.points

  let make'' ?(leaf_size = 25) points =
    let rec aux idxs =
      let len = Array.length idxs in
      if len <= leaf_size
      then Leaf idxs
      else (
        let projected = P.proj points idxs in
        let mean_proj =
          Array.fold_left (fun sum p -> sum +. p) 0. projected /. Float.of_int len
        in
        let local_pivot =
          let idx = ref 0
          and min = ref Float.max_float in
          for i = 0 to len - 1 do
            let d = Float.abs (projected.(i) -. mean_proj) in
            if d < !min
            then (
              min := d;
              idx := i )
          done;
          !idx
        in
        let pivot = idxs.(local_pivot) in
        let radius =
          let p = points.(pivot) in
          let f max idx = Float.max max V.(distance p points.(idx)) in
          Array.fold_left f 0. idxs
        in
        let left, right =
          let l = ref []
          and r = ref [] in
          for i = 0 to len - 1 do
            if i <> local_pivot
            then (
              if projected.(i) <= mean_proj then l := idxs.(i) :: !l;
              if projected.(i) > mean_proj then r := idxs.(i) :: !r )
          done;
          aux (Util.array_of_list_rev !l), aux (Util.array_of_list_rev !r)
        in
        Node { pivot; radius; left; right } )
    in
    { points; tree = aux (Array.init (Array.length points) Fun.id) }

  let make' ?leaf_size points = make'' ?leaf_size (Array.copy points)
  let make ?leaf_size points = make'' ?leaf_size (Array.of_list points)

  let search_idxs ?(radius = Util.epsilon) { points; tree } target =
    let rec aux = function
      | Leaf idxs ->
        let f acc i = if V.approx ~eps:radius points.(i) target then i :: acc else acc in
        Array.fold_left f [] idxs
      | Node tree ->
        if not @@ V.approx ~eps:(tree.radius +. radius) points.(tree.pivot) target
        then []
        else (
          let children = List.rev_append (aux tree.left) (aux tree.right) in
          if V.approx ~eps:radius points.(tree.pivot) target
          then tree.pivot :: children
          else children )
    in
    aux tree

  let search_points ?radius t target =
    List.map (fun i -> t.points.(i)) (search_idxs ?radius t target)
end
