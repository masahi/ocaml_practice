open Codelib

open Ring
open Vector
(* open Lifts *)


module type VECR = sig
  include VEC
  type t
  val reduce : (t -> t -> t) -> t -> (idx, t) vec -> t
end

module BLAS2(R: RING)(A: VECR with type t = R.t) = struct
  open R
  open A
  include BLAS1(R)(A)
  let dot v1 v2 = reduce add zero (v1 *. v2)
  let ( * ) a v = vec_map (dot v) a
end

module MVMULT(R: RING)(A: VECR with type t = R.t) = struct
  module L = BLAS2(R)(A)
  open L
  let mvmult vout a v =
    vout := a * v
end

module VecRSta(T:sig type t end) = struct
  include T
  include VecSta

  let reduce plus zero (Vec (n,body))  =
    let sum = ref zero in
    for i = 0 to n-1 do
	  sum := plus !sum (body i)
    done;
    !sum
end


let mvmult_p : float array -> float array array -> float array -> unit =
  fun vout a v ->
    let n = Array.length vout and m = Array.length v in
      (* vector representations *)
    let vout = OVec (n,fun i v -> vout.(i) <- v) in
    let v    = Vec  (m,fun j -> v.(j)) in
    let a    = Vec  (n,fun i -> Vec (m, fun j -> a.(i).(j))) in

    let module MV = MVMULT(RingFloat)(VecRSta(RingFloat)) in
    MV.mvmult vout a v

let a = [|
  [| 0.5; 0.; 0.;  0.5; 0. |];
  [| 0.;  0.; 1.;  0.;  0. |];
  [| 0.;  1.; 0.;  0.;  0. |];
  [| 0.;  0.; 0.2; 0.3; 0.5 |];
  [| 0.;  0.; 0.3; 0.;  0.7 |]
 |]

(* Unit test *)
let v1 = [| 1.; 2.; 3.; 4.; 5. |]
let v1out =
  let vout = Array.make 5 0. in
  mvmult_p vout a v1; vout

module VecRDyn(T: sig type t end) = struct
  include T
  include VecDyn

  let reduce : ('a code -> 'a code -> 'a code) -> 'a code -> (int code, 'a code) vec -> 'a code =
    fun plus zero (Vec (n, body)) ->
    .<let sum = ref .~zero in
      for i = 0 to (.~n) - 1 do
        sum := .~(plus .<!sum>. (body .<i>.))
      done;
      !sum>.

end

let mvmult_c : (float array -> float array array -> float array -> unit) code =
  .<fun vout a v ->
    let n = Array.length vout and m = Array.length v in
    .~(let vout = OVec (.<n>., fun i v -> .<vout.(.~i) <- .~v>.) in
       let v = Vec (.<m>., fun j -> .<v.(.~j)>.) in
       let a = Vec (.<n>., fun i ->
           Vec (.<m>., fun j -> .<a.(.~i).(.~j)>.)) in
       let module MV = MVMULT(RingFloatCode)(VecRDyn(RingFloatCode)) in
       MV.mvmult vout a v)
      >.

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run mvmult_c) vout a v1;
  print_code Format.std_formatter mvmult_c; print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout


module VecRStaDim(T: sig type t end) = struct
  include T
  module M = VecRSta(T)
  type idx = int
  type unt = unit code

  let reduce = M.reduce

  let seq e1 e2 = .<.~e1; .~e2>.
  let iter arr = reduce seq .<()>. arr
end


let mvmult_nc : int -> int -> (float array -> float array array -> float array -> unit) code =
  fun n m -> .<fun vout a v ->
    assert (n = Array.length vout && m = Array.length v);
    .~(let vout = OVec (n, fun i v -> .<vout.(i) <- .~v>.) in
       let v = Vec (m, fun j -> .<v.(j)>.) in
       let a = Vec (n, fun i ->
           Vec (m, fun j -> .<a.(i).(j)>.)) in
       let module MV = MVMULT(RingFloatCode)(VecRStaDim(RingFloatCode)) in
       MV.mvmult vout a v)
      >.

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run (mvmult_nc 5 5)) vout a v1;
  print_code Format.std_formatter (mvmult_nc 5 5); print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout
