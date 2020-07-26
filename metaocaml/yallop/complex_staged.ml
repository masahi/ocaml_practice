open Codelib
open Fft_types

type t = complex_sd

(** Question 2(a)(i) *)

let i = Dyn .< Complex.i >.

let add l r = Dyn (genlet .<Complex.add .~(dyn_complex l) .~(dyn_complex r)>.)

let sub l r = Dyn (genlet .< Complex.sub .~(dyn_complex l) .~(dyn_complex r) >.)

let mul l r =
   match l, r with
   | Sta v1, Sta v2 -> Sta (Complex.mul v1 v2)
   | Sta ({re; im}), Dyn _ when re = 1.0 && im = 0.0 -> r
   | Dyn _,  Sta ({re; im}) when re = 1.0 && im = 0.0 -> l
   | _ -> Dyn (genlet .< Complex.mul .~(dyn_complex l) .~(dyn_complex r) >.)
