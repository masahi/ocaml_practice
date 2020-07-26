open Codelib
open Fft_types

let rec nat_to_int: type n. n nat -> int = function
  | Z -> 0
  | S(n) -> 1 + (nat_to_int n)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

module Arr :
sig
  include Fft_unstaged.ARRAY with type elem = complex_sd

  type elem_ = Complex.t

  (** Build an [Arr.t] value from an [array code] value. *)
  val mk : 'n nat -> elem_ array code -> 'n t

  (** Build an [array code] value from an [Arr.t] value. *)
  val dyn : 'n t -> elem_ array code
end =
struct
  include Fft_unstaged.Make_arr(struct type elem = complex_sd end)
  type elem_ = Complex.t

  (* Question 2(b)(i) *)
  let rec mk : type n. n nat -> Complex.t array code -> n t = fun num cde_arr ->
    match num with
    | Z -> Leaf(Dyn(.<(.~cde_arr).(0)>.))
    | S(n) ->
      let exponent = nat_to_int n in
      let length = pow 2 exponent in
      let half = (length / 2) in
      let left_arr = .<Array.init half (fun i -> (.~cde_arr).(i))>. in
      let right_arr = .<Array.init half (fun i -> (.~cde_arr).(i + half))>. in
      let left = mk n left_arr in
      let right = mk n right_arr in
      Branch(left, right)

  (* Question 2(b)(i) *)
  let rec dyn : type n. n t -> Complex.t array code = function
    | Leaf(Sta(v)) -> .<Array.init 1 (fun _ -> .~(dyn_complex (Sta v)))>.
    | Leaf(Dyn(cde)) -> .<Array.init 1 (fun _ -> .~cde)>.
    | Branch(left, right) ->
      let left_cde = dyn left in
      let right_cde = dyn right in
      .<Array.append .~left_cde .~right_cde>.
end

let w n j = Sta (Fft_unstaged.w n j)

let merge l1 l2 =
  let open Complex_staged in
  let n = 2 * Arr.length l1 in
  let a, b = Arr.map2i
      (fun j x y ->
         let z1 = mul (w n j) y in
         let zx = add x z1 in
         let zy = sub x z1 in
         zx, zy)
      l1 l2
  in Arr.append a b

let rec fft : type n. n Arr.t -> n Arr.t =
  fun arr -> match Arr.explength arr with
      Z -> arr
    | S _ -> let (evens,odds) = Arr.split arr in
             merge (fft evens) (fft odds)

(* Question 2(b)(ii) *)
(** A code generator for building an FFT implementation specialized to
    a particular array size. *)
let mk : type n. n nat -> (Complex.t array -> Complex.t array) code =
  fun n ->
  .<fun inp_arr -> .~(
    let arr = Arr.mk n .<inp_arr>. in
    let res = fft arr in
    Arr.dyn res)>.

let _ =
  let exponent = S(S(Z)) in
  (* let arr_length = pow 2 (nat_to_int exponent) in *)
  let fft_cde = mk exponent in
  print_code Format.std_formatter fft_cde; print_newline ()