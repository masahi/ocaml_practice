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

  (* val keys : 'a t -> key list
   *
   * val length : 'a t -> int *)
end

module Make(C : Ordered) : S with type key = C.t
