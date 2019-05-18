module type S = sig
  type key
  type 'a t

  val empty : 'a t

  val insert : 'a t -> key -> 'a -> 'a t

  val lookup : 'a t -> key -> 'a option

  val keys : 'a t -> key list
end

module Make(C : Types.Ordered) : S with type key = C.t
