open Codelib
(** Question 2(a)(ii) *)

(** Possibly-static values, specialized for complex numbers. *)
type complex_sd =
    Sta  : Complex.t -> complex_sd
  | Dyn : Complex.t code -> complex_sd

(** Build a code value from a possibly-static complex number.*)
let dyn_complex : complex_sd -> Complex.t code = function
    Sta { Complex.re; im } -> .< { Complex.re = re; im = im }>.
  | Dyn v -> v

(** Peano numbers.*)
type z = Z and _ s = S

type _ nat = Z : z nat | S : 'n nat -> 'n s nat

(** Balanced binary trees.*)
type ('a, _) bal =
    Leaf : 'a -> ('a, z) bal
  | Branch : ('a, 'n) bal * ('a, 'n) bal -> ('a, 'n s) bal
