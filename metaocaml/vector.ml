open Codelib
(* Abstracting over vectors
 *)

(* A generic input vector: pull vector *)
type ('i,'a) vec = Vec of 'i *			(* length *)
                          ('i -> 'a)
;;

(* A generic output vector *)
type ('i,'a,'w) ovec = OVec of 'i * 		(* length *)
                                ('i -> 'a -> 'w)
;;

(* To get a better feel for abstract vectors, let's see how OCaml
   arrays can be represented as a vector: actually, a pair of an
   input and an output vector.
*)

let vec_of_array : 'a array -> ( (int,'a) vec * (int,'a,unit) ovec ) = fun a ->
 let n = Array.length a in
 ( Vec  (n, fun i   -> a.(i)),
   OVec (n, fun i v -> a.(i) <- v)
 )
;;


(* Truly generic function on vectors (element-wise) *)
(* QQQ check boundaries *)

(* element-wise operation on a single vector *)
let vec_map : ('a -> 'b) -> ('i,'a) vec -> ('i,'b) vec =
 fun tf (Vec (n, f)) -> Vec (n, fun i -> tf (f i))
;;

(* element-wise binary vector operation *)
let zip_with : ('a -> 'b ->'c) -> ('i,'a) vec -> ('i,'b) vec -> ('i,'c) vec =
  fun tf (Vec (n1,f1)) (Vec (_,f2)) ->
    Vec (n1, fun i -> tf (f1 i) (f2 i))
;;

(* A variation of zip_with, when the first argument is an output vector *)
let vec_assign : ('i,'a,'w) ovec -> ('i,'a) vec -> ('i,'w) vec =
 fun (OVec (no,fo)) (Vec (_,fi)) ->
   Vec(no, fun i -> fo i (fi i))
;;


(* Array operations that depend on a particular index representation *)
(* This vector is a bit simplified, just enough for our examples.
   See mvmult_full.ml for a bigger VEC
   Here iter is a specialized version of left fold (or, reduce).
   |unt| is the type representing the updated vector. It is assumed to be
   monoid.
*)
module type VEC = sig
  type idx                           (* index *)
  type unt                           (* unit type *)
  val iter : (idx, unt) vec -> unt
end;;

(* One implementation, for vectors representing ordinary arrays *)

module VecSta = struct
  type idx = int
  type unt = unit

  let iter (Vec (n,body)) =
    for i=0 to n-1 do
      body i
    done
end;;

(* int-code indexed vector: next-stage vectors *)
module VecDyn = struct
  type idx = int code
  type unt = unit code

  let iter (Vec (n,body)) =             (* staged VecSta.iter *)
    .<for i=0 to (.~n)-1 do
      .~(body .<i>.)
    done>.
end;;

(* ------------------------------------------------------------------------
   Very simple BLAS Level 1: vector assignment and element-wise multiplications
 *)

open Ring

(* Higher-level vector/matrix operations *)
module BLAS1(R: RING)(A: VEC) = struct
  open R
  open A
  let ( := ) vout vin = iter (vec_assign vout vin)

  let ( *. ) v1 v2 = zip_with mul v1 v2	     (* element-wise mul *)
end;;
