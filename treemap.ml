module type Ordered = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t

  val insert : 'a t -> key -> 'a -> 'a t

  val lookup : 'a t -> key -> 'a option
end

module Map1(C : Ordered) = struct
  module StdMap = Map.Make(C)
  type key = C.t
  type 'a t = 'a StdMap.t
  let empty = StdMap.empty
  let insert map key value = StdMap.add key value map
  let lookup map key = StdMap.find_opt key map
end


module Make(C : Ordered) = Map1(C)

