val partition
  :  ?rev:bool
  -> ?lift:(V2.t -> V3.t)
  -> holes:V2.t list list
  -> V2.t list
  -> V3.t list * int list list
