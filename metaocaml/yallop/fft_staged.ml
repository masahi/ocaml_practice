open Codelib
open Fft_types
open Util

let convert_array: 'a code array -> 'a array code =
  let rec convert_list: 'a code list -> 'a list code = function
    | hd :: tl ->
      let tl_code = convert_list tl in
      .<.~hd :: .~tl_code>.
    | [] -> .<[]>. in
  fun arr ->
    let lst = List.init (Array.length arr) (fun i -> arr.(i)) in
    let lst_code = convert_list lst in
    .<Array.of_list .~lst_code >.


module MakeFFT(D: StagedDomain) = struct
  module Arr :
  sig
    include ARRAY with type elem = D.t

    type elem_ = D.t_sta

    (** Build an [Arr.t] value from an [array code] value. *)
    val mk : 'n nat -> elem_ array code -> 'n t

    (** Build an [array code] value from an [Arr.t] value. *)
    val dyn : 'n t -> elem_ array code
  end =
  struct
    include Make_arr(struct type elem = D.t end)
    type elem_ = D.t_sta

    (* Question 2(b)(i) *)
    let mk : type n. n nat -> elem_ array code -> n t = fun num cde_arr ->
      let rec mk_helper: type n. n nat -> elem_ array code -> int -> int -> n t =
        fun num cde_arr left_end right_end ->
          match num with
          | Z -> Leaf(D.Dyn(.<(.~cde_arr).(left_end)>.))
          | S(n) ->
            let left = mk_helper n cde_arr left_end (left_end + (right_end - left_end)/ 2) in
            let right = mk_helper n cde_arr (left_end + (right_end - left_end)/ 2) right_end in
            Branch(left, right)
      in
      let exponent = nat_to_int num in
      let length = pow 2 exponent in
      mk_helper num cde_arr 0 length

    (* Question 2(b)(i) *)
    let dyn : type n. n t -> elem_ array code = fun arr ->
      let rec dyn_helper : type n. n t -> elem_ code array = function
        | Leaf(D.Sta(v)) -> Array.init 1 (fun _ -> D.dyn_t (Sta v))
        | Leaf(Dyn(cde)) -> Array.init 1 (fun _ -> cde)
        | Branch(left, right) ->
          let left_cde = dyn_helper left in
          let right_cde = dyn_helper right in
          Array.append left_cde right_cde in
      let cde_arr = dyn_helper arr in
      convert_array cde_arr

  end

  let w n j = D.Sta (D.primitive_root_power n j)

  let merge l1 l2 =
    let open D in
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
  let mk : type n. n nat -> (Arr.elem_ array -> Arr.elem_ array) code =
    fun n ->
    .<fun inp_arr -> .~(
        let arr = Arr.mk n .<inp_arr>. in
        let res = fft arr in
        Arr.dyn res)>.
end

module ComplexFFT = MakeFFT(ComplexStagedDomain)

let test exponent do_bench =
  let fft_cde = ComplexFFT.mk exponent in
  let arr_length = pow 2 (nat_to_int exponent) in
  let open Complex in
  let inp = Array.init arr_length (fun _ -> {re=Random.float 1.0; im=Random.float 1.0}) in
  let f = Runnative.run fft_cde in
  let arr = Fft_unstaged.Arr.mk exponent inp in

  if do_bench then
    (* let open Core_bench in
     * [Bench.Test.create ~name:"staged" (fun () -> ignore(f inp));
     *  Bench.Test.create ~name:"unstaged" (fun () -> ignore(Fft_unstaged.fft arr))]
     * |> Bench.bench *)
    ()
  else begin
    print_code Format.std_formatter fft_cde; print_newline ();
    let res = f inp in
    Array.iter (fun {re;im} -> Printf.printf "Staged: (%f, %f)\n" re im) res;
    print_newline ();
    let res = Fft_unstaged.fft arr in
    Array.iter (fun {re;im} -> Printf.printf "Unstaged: (%f, %f)\n" re im) (Fft_unstaged.Arr.unmk res)
  end


let fft_16 =
  let exponent = S(S(S(S(Z)))) in
  let fft_cde = ComplexFFT.mk exponent in
  Runnative.run fft_cde


(* let _ =
 *   let exponent = S(S(S(S(Z)))) in
 *   test exponent false *)
