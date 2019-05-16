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

  val keys : 'a t -> key list

  val length : 'a t -> int
end

(* module Map1(C : Ordered) = struct
 *   module StdMap = Map.Make(C)
 *   type key = C.t
 *   type 'a t = 'a StdMap.t
 *   let empty = StdMap.empty
 *   let insert map key value = StdMap.add key value map
 *   let lookup map key = StdMap.find_opt key map
 * end *)

module MapCore(C : Ordered) = struct
  open Base
  module Comp = struct
    module T = struct
      type t = C.t
      let compare = C.compare
      let sexp_of_t _ =
        let open Sexp in
        List []
    end
    include T
    include Comparator.Make(T)
  end

  type key = C.t
  type 'a t = (key, 'a, Comp.comparator_witness) Map.t
  let empty = Map.empty (module Comp)
  let insert map key data = Map.set map ~key ~data
  let lookup = Map.find
  let keys = Map.keys
  let length = Map.length
end

module Make(C : Ordered) = MapCore(C)

