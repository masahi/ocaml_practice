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
