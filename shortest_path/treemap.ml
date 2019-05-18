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
end

module Map23(C : Ordered) = struct
  type key = C.t
  type 'a pair = key * 'a

  type 'a t =
    | Leaf
    | Two of 'a t * 'a pair * 'a t
    | Three of 'a t * 'a pair * 'a t * 'a pair * 'a t

  type 'a kicked =
    | Up of 'a t * 'a pair * 'a t
    | Done of 'a t

  let empty = Leaf

  let insert_upward_two (w_key, w_val) w_left w_right (x_key, x_val) x_other =
    let cmp = C.compare w_key x_key in
    if cmp <= 0 then Done (Three (w_left, (w_key, w_val), w_right, (x_key, x_val), x_other))
    else Done (Three (x_other, (x_key, x_val), w_left, (w_key, w_val), w_right))

  let insert_upward_three w w_left w_right x y other_left other_right =
    let (w_key, _) = w in
    let (x_key, _) = x in
    let (y_key, _) = y in
    let cmp1 = C.compare w_key x_key in
    let cmp2 = C.compare w_key y_key in
    if cmp1 <= 0 then
      Up (Two (w_left, w, w_right), x, Two (other_left, y, other_right))
    else if cmp2 <= 0 then
      Up (Two (other_left, x, w_left), w, Two (w_right, y, other_right))
    else
      Up (Two (other_left, x, other_right), y, Two (w_left, w, w_right))

  let rec insert_downward tree key data =
    match tree with
    | Leaf -> Up (Leaf, (key, data), Leaf)
    | Two(left,n,right) -> insert_downward_two (key, data) n left right
    | Three(left,n1,middle,n2,right) -> insert_downward_three (key, data) n1 n2 left middle right

  and insert_downward_two (k, v) (k1,v1) left right =
    let cmp = C.compare k k1 in
    if cmp < 0 then
      match insert_downward left k v with
      | Done d -> Done (Two (d, (k1, v1), right))
      | Up (l, p, r) -> insert_upward_two p l r (k1, v1) right
    else if cmp = 0 then
      Done (Two (left, (k, v), right))
    else
      match insert_downward right k v with
      | Done d -> Done (Two (left, (k1, v1), d))
      | Up (l, p, r) -> insert_upward_two p l r (k1, v1) left

  and insert_downward_three (k,v) (k1,v1) (k2,v2) left middle right =
    let cmp1 = C.compare k k1 in
    let cmp2 = C.compare k k2 in
    if cmp1 < 0 then
      match insert_downward left k v with
      | Done d -> Done (Three (d, (k1, v1), middle, (k2, v2), right))
      | Up (l, p, r) -> insert_upward_three p l r (k1, v1) (k2, v2) middle right
    else if cmp1 = 0 then
      Done (Three (left, (k, v), middle, (k2, v2), right))
    else if cmp2 < 0 then
      match insert_downward middle k v with
      | Done d -> Done (Three (left, (k1, v1), d, (k2, v2), right))
      | Up (l, p, r) -> insert_upward_three p l r (k1, v1) (k2, v2) left right
    else if cmp2 = 0 then
      Done (Three (left, (k1, v1), middle, (k, v), right))
    else
      match insert_downward right k v with
      | Done d -> Done (Three (left, (k1, v1), middle, (k2, v2), d))
      | Up (l, p, r) -> insert_upward_three p l r (k1, v1) (k2, v2) left middle

  let insert tree key data =
    match insert_downward tree key data with
    | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
    | Done x -> x

  let rec lookup tree k =
    match tree with
    | Leaf -> None
    | Two (left, (key, v), right) ->
      let cmp = C.compare k key in
      if cmp < 0 then lookup left k
      else if cmp = 0 then Some v
      else lookup right k
    | Three (left, (k1, v1), mid, p2, right) ->
      let cmp = C.compare k k1 in
      if cmp < 0 then lookup left k
      else if cmp = 0 then Some v1
      else lookup (Two (mid, p2, right)) k

  let rec keys =function
    | Leaf -> []
    | Two (left, (k, _), right) ->
      keys left @ [k] @ keys right
    | Three (left, (k1, _), mid, (k2, _), right) ->
      keys left @ [k1] @ keys mid @ [k2] @ keys right
end

(* module MapCore(C : Ordered) = struct
 *   open Base
 *   module Comp = struct
 *     module T = struct
 *       type t = C.t
 *       let compare = C.compare
 *       let sexp_of_t _ =
 *         let open Sexp in
 *         List []
 *     end
 *     include T
 *     include Comparator.Make(T)
 *   end
 *
 *   type key = C.t
 *   type 'a t = (key, 'a, Comp.comparator_witness) Map.t
 *   let empty = Map.empty (module Comp)
 *   let insert map key data = Map.set map ~key ~data
 *   let lookup = Map.find
 *   let keys = Map.keys
 *   let length = Map.length
 * end *)

module Make(C : Ordered) = Map23(C)

