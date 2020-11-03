open Codelib
(** Question 2(a)(ii) *)

(** Peano numbers.*)
type z = Z and _ s = S

type _ nat = Z : z nat | S : 'n nat -> 'n s nat

(** Balanced binary trees.*)
type ('a, _) bal =
    Leaf : 'a -> ('a, z) bal
  | Branch : ('a, 'n) bal * ('a, 'n) bal -> ('a, 'n s) bal


module type Domain =
sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val zero: t
  val primitive_root_power : int -> int -> t
end

module type StagedDomain =
sig
  type t_sta

  type t =
      Sta  : t_sta -> t
    | Dyn : t_sta code -> t
  val dyn_t : t -> t_sta code

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val zero: t
  val primitive_root_power : int -> int -> t_sta
end

module ComplexDomain = struct
  type t = Complex.t
  let add = Complex.add
  let sub = Complex.sub
  let mul = Complex.mul
  let zero = { Complex.re = 0.0; Complex.im = 0.0 }

  let primitive_root_power n j =
    let open Complex in
    let pi = 3.14159265358979 in
    let re r = { re = r; im = 0.0 } in
    exp (div (mul (re (-. 2. *. pi *. float j)) Complex.i) (re (float n)))

end

module ComplexStagedDomain = struct
  type t_sta = Complex.t

  type t =
      Sta  : t_sta -> t
    | Dyn : t_sta code -> t

  let dyn_t : t -> t_sta code = function
      Sta { Complex.re; im } -> .< { Complex.re = re; im = im }>.
    | Dyn v -> v

  let add l r = Dyn (genlet .<Complex.add .~(dyn_t l) .~(dyn_t r)>.)
  let sub l r = Dyn (genlet .< Complex.sub .~(dyn_t l) .~(dyn_t r) >.)

  let mul l r =
    match l, r with
    | Sta v1, Sta v2 -> Sta (Complex.mul v1 v2)
    | Sta ({re; im}), Dyn _ when re = 1.0 && im = 0.0 -> r
    | Dyn _,  Sta ({re; im}) when re = 1.0 && im = 0.0 -> l
    | _ -> Dyn (genlet .< Complex.mul .~(dyn_t l) .~(dyn_t r) >.)

  let zero = Sta (ComplexDomain.zero)
  let primitive_root_power = ComplexDomain.primitive_root_power
end


module type ARRAY =
sig
  (** The element type of the array *)
  type elem

  (** The type of the array, indexed by the exponent: a value of type
      [n t] is an array of length [2^n]. *)
  type _ t

  (** Split an array into even and odd components.  For example,

       split [| x_0; x_1; ... x_{n-1} |]

      is

       [| x_0; x_2; ... x_{n-2} |], [| x_1; x_3; ... x_{n-1} |]      *)
  val split : ('n s) t -> 'n t * 'n t

  (** Append one array to another.  For example,

       append [| x_0; x_1 |] [| y_0; y_1 |]

      is

       [| x_0; x_1; y_0; y_1 |]          *)
  val append : 'n t -> 'n t -> ('n s) t

  (** The exponent in the length of the array. *)
  val explength : 'n t -> 'n nat

  (** The length of the array. *)
  val length : _ t -> int

  (** A kind of map function over two arrays which makes both the
      index and the two elements in corresponding positions in the
      arrays available to each call to the user-supplied function. *)
  val map2i : (int -> elem -> elem -> elem * elem) ->
    'n t -> 'n t -> 'n t * 'n t

end

(** An implementation of the ARRAY interface based on balanced binary
    trees. *)
module Make_arr (S: sig type elem end) =
struct
  type elem = S.elem
  type 'n t = (elem, 'n) bal

  let rec split : type n. (n s) t -> n t * n t = function
      Branch (Leaf l, Leaf r) -> (Leaf l, Leaf r)
    | Branch ((Branch _ as l), (Branch _ as r)) ->
      let a, b = split l and c, d = split r in
      Branch (a, c), Branch (b, d)

  let rec explength : type n. n t -> n nat = function
      Leaf _ -> Z
    | Branch (l, _) -> S (explength l)

  let rec length : type n. n t -> int = function
      Leaf _ -> 1
    | Branch (l, _) -> 2 * length l

  let append l r = Branch (l, r)

  let rec map2i' : type n. (int -> elem -> elem -> elem * elem) ->
                          int -> n t  -> n t  -> n t  * n t * int =
    fun f j l1 l2 ->
      match l1, l2 with
      | Leaf l, Leaf r -> let l', r' = f j l r in (Leaf l', Leaf r', succ j)
      | Branch (a, b), Branch (c, d) ->
        let a', c', j'  = map2i' f j  a c in
        let b', d', j'' = map2i' f j' b d in
        Branch (a', b'), Branch (c', d'), j''

  let map2i f l1 l2 = let l1', l2', _ = map2i' f 0 l1 l2 in l1', l2'

end
