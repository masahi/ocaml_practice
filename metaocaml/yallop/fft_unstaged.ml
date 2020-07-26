open Fft_types
open Util

let pi = 3.14159265358979

(** A signature for arrays of length 2 to the power n, with the
    operations used in the implementation of the FFT. *)
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
          let left = mk_helper n arr left_end (right_end / 2) in
          let right = mk_helper n arr (right_end / 2) right_end in
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
