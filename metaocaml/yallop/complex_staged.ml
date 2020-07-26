open Fft_types

type t = complex_sd

(** Question 2(a)(i) *)

let i = Dyn .< Complex.i >.

let add l r = Dyn .< Complex.add .~(dyn_complex l) .~(dyn_complex r) >.

let sub l r = Dyn .< Complex.sub .~(dyn_complex l) .~(dyn_complex r) >.

let mul l r = Dyn .< Complex.mul .~(dyn_complex l) .~(dyn_complex r) >.

let div l r = Dyn .< Complex.div .~(dyn_complex l) .~(dyn_complex r) >.

let exp z = Dyn .< Complex.exp .~(dyn_complex z) >.
