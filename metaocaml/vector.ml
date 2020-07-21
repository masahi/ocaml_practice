open Codelib

type ('i, 'a) vec = Vec of 'i * ('i -> 'a)

type ('i, 'a, 'w) ovec = OVec of 'i * ('i -> 'a -> 'w)

let vec_of_array = fun a ->
  let n = Array.length a in
  (Vec (n, fun i -> a.(i)),
   OVec (n, fun i v -> a.(i) <- v))

let vec_map : ('a -> 'b) -> ('i,'a) vec -> ('i,'b) vec =
 fun tf (Vec (n, f)) -> Vec (n, fun i -> tf (f i))

let zip_with : ('a -> 'b ->'c) -> ('i,'a) vec -> ('i,'b) vec -> ('i,'c) vec =
  fun tf (Vec (n1,f1)) (Vec (_,f2)) ->
    Vec (n1, fun i -> tf (f1 i) (f2 i))

let vec_assign : ('i,'a,'w) ovec -> ('i,'a) vec -> ('i,'w) vec =
 fun (OVec (no,fo)) (Vec (_,fi)) ->
   Vec(no, fun i -> fo i (fi i))

open Ring

module type VEC = sig
  type idx
  type unt
  val iter : (idx, unt) vec -> unt
end

module BLAS1(R: RING)(A: VEC) = struct
  open R
  open A
  let ( := ) vout vin = iter (vec_assign vout vin)

  let ( *. ) v1 v2 = zip_with mul v1 v2
end

module VecSta = struct
  type idx = int
  type unt = unit

  let iter (Vec (n,body)) =
    for i=0 to n-1 do
      body i
    done
end

(* int-code indexed vector: next-stage vectors *)
module VecDyn = struct
  type idx = int code
  type unt = unit code

  let iter (Vec (n,body)) =
    .<for i=0 to (.~n)-1 do
      .~(body .<i>.)
    done>.
end
