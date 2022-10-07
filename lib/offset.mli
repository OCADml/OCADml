val offset
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Delta ]
  -> float
  -> V2.t list
  -> V2.t list

val offset_with_faces
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?flip_faces:bool
  -> ?start_idx:int
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Delta ]
  -> float
  -> V2.t list
  -> int * V2.t list * int list list
