open Fft_types
open Util

let pi = 3.14159265358979

module Arr :
sig
  include ARRAY with type elem = Complex.t

  val mk : 'n nat -> elem array -> 'n t

  val unmk : 'n t -> elem array
end =
struct
  include Make_arr(struct type elem = Complex.t end)

  (* Question 2(b)(i) *)
  let mk : type n. n nat -> Complex.t array -> n t = fun num arr ->
    let rec mk_helper: type n. n nat -> Complex.t array -> int -> int -> n t =
      fun num arr left_end right_end ->
        match num with
        | Z -> Leaf(arr.(left_end))
        | S(n) ->
          let left = mk_helper n arr left_end (left_end + (right_end - left_end)/ 2) in
          let right = mk_helper n arr (left_end + (right_end - left_end) / 2) right_end in
          Branch(left, right)
    in
    let exponent = nat_to_int num in
    let length = pow 2 exponent in
    mk_helper num arr 0 length

  (* Question 2(b)(i) *)
  let rec unmk : type n. n t -> Complex.t array = function
    | Leaf(v) -> Array.init 1 (fun _ -> v)
    | Branch(left, right) ->
      let left_cde = unmk left in
      let right_cde = unmk right in
      Array.append left_cde right_cde
end

(** The jth of the n nth roots of unity. *)
let w n j =
  let open Complex in
  let re r = { re = r; im = 0.0 } in
  exp (div (mul (re (-. 2. *. pi *. float j)) i) (re (float n)))

(** The merge step of the FFT algorithm. *)
let merge l1 l2 =
  let open Complex in
  let n = 2 * Arr.length l1 in
  let a, b = Arr.map2i
      (fun j x y ->
         let z1 = mul (w n j) y in
         let zx = add x z1 in
         let zy = sub x z1 in
         zx, zy)
      l1 l2
  in Arr.append a b

(** The FFT algorithm. *)
let rec fft : type n. n Arr.t -> n Arr.t =
  fun arr -> match Arr.explength arr with
      Z -> arr
    | S _ -> let (evens,odds) = Arr.split arr in
             merge (fft evens) (fft odds)
