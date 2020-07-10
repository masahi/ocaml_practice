(* Abstracting over numbers
   For the sake of exercise, our DSL is minimalist, with just
   enough features for the exercise.
   Although it all boils to adding and multiplying numbers, an abstraction
   helps.
*)

(* Could have used names like ( +% ) and ( *% ) or even ( + ) and ( * )
*)

open Codelib

module type RING = sig
  type t				(* abstract *)
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
end

(* Example *)
module ExArith(R:RING) = struct
  open R
  let xsq1 x = add (mul x x) one
end
(*
  module ExArith : functor (R : RING) -> sig val xsq1 : R.t -> R.t end
*)

(* The complicated functor notation can be avoided: *)
let xsq1 (type a): (module RING with type t = a) -> (a->a) =
  fun (module R) -> let open R in
    fun x -> add (mul x x) one
;;
(*
 val xsq1 : (module RING with type t = 'a) -> 'a -> 'a = <fun>
*)


(* Floats form the ring (to a good approximation)
*)

module RingFloat = struct
  type t = float
  let zero = 0.
  let one  = 1.
  let add = Pervasives.( +. )
  let sub = Pervasives.( -. )
  let mul = Pervasives.( *. )
end;;

let _ =
  let f = let module M = ExArith(RingFloat) in M.xsq1
  in f 2.0

let _ =
  let f = xsq1 (module RingFloat)
  in f 2.0

(* But code of floats is also a RING *)

module RingFloatCode = struct
  type t = float code
  let zero = .<0.>.
  let one  = .<1.>.
  let add = fun x y -> .<.~x +. .~y>.
  let sub = fun x y -> .<.~x -. .~y>.
  let mul = fun x y -> .<.~x *. .~y>.
end;;

(* Now, exactly the same ExArith example is interepreted differently *)

let _ =
  let f = let module M = ExArith(RingFloatCode) in M.xsq1
  in .<fun x -> .~(f .<x>.) >.
(*
      - : (float -> float) code = .<fun x_1  -> (x_1 *. x_1) +. 1.>.
*)

let _ =
  let f = xsq1 (module RingFloatCode)
  in .<fun x -> .~(f .<x>.) >.
(*
    - : (float -> float) code = .<fun x_2  -> (x_2 *. x_2) +. 1.>.
*)

(* let _ =
 *   let f = xsq1 (module RingFloatCode) in
 *   let ret = Runnative.run .<fun x -> .~(f .<x>.) >. 2.0 in
 *   Printf.printf "%f\n" ret *)

;;
