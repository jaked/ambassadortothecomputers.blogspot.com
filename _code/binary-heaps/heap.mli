module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t
  val make : unit -> t
  val add : t -> elt -> unit
  val peek_min : t -> elt option
  val take_min : t -> elt
  val size : t -> int
end

module Make (O : OrderedType) : S with type elt = O.t
