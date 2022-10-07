module type Ops = sig
  type t

  (** The identity matrix. *)
  val id : t

  (** {1 Basic Matrix Operations} *)

  (** [mul a b]

      Multiply the matrices [a] and [b]. *)
  val mul : t -> t -> t

  (** [add a b]

      Element by element addition of the matrices [a] and [b]. *)
  val add : t -> t -> t

  (** [sub a b]

      Element by element subtraction of the matrix [b] from [a]. *)
  val sub : t -> t -> t

  (** [emul a b]

      Element by element multiplication of the matrices [a] and [b]. *)
  val emul : t -> t -> t

  (** [ediv a b]

      Element by element division of the matrix [a] by [b]. *)
  val ediv : t -> t -> t

  (** [smul t s]

      Multiply each element of the matrix [t] by the scalar [s]. *)
  val smul : t -> float -> t

  (** [sdiv t s]

      Divide each element of the matrix [t] by the scalar [s]. *)
  val sdiv : t -> float -> t

  (** [sadd t s]

      Add the scalar [s] to each element of the matrix [t]. *)
  val sadd : t -> float -> t

  (** [ssub t s]

      Subtract the scalar [s] to from each element of the matrix [t]. *)
  val ssub : t -> float -> t

  (** [transpose t]

      Transpose the rows and columns of [t]. *)
  val transpose : t -> t

  (** [map f t]

      Apply the function [f] to all elements of [t]. *)
  val map : (float -> float) -> t -> t

  (** [trace t]

       Sum the elements on the main diagonal (upper left to lower right) of [t]. *)
  val trace : t -> float

  (** [get t r c]

       Get the element at [r]ow and [c]olumn of [t]. Equivalent to [t.(r).(c)].
       Raises [Invalid_argument] if access is out of bounds. *)
  val get : t -> int -> int -> float

  (** [compose a b]

      Compose the affine transformations [a] and [b]. Equivalent to [mul b a],
      which when applied, will perform the transformation [a], then the
      transformation [b]. *)
  val compose : t -> t -> t

  (** [a %> b]

      Alias to {!compose}. *)
  val ( %> ) : t -> t -> t

  (** [a % b]

      Mathematical composition of affine transformations [a] and [b],
      equivalent to [mul a b]. *)
  val ( % ) : t -> t -> t
end
