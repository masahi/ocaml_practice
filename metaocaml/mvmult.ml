open Codelib

(* Stepwise development of optimal specialized matrix-vector multiplications *)

(* Solving the full version of first Shonan challenge problem: generate
   code for optimal matrix-vector multiplication with a statically
   known and sparse matrix. If a matrix row is
   sparse enough (below a given threshold), its dot-product with
   the input vector should be completely unrolled.
   Algebraic simplifications like 0 + x = x, 1 * x = x should
   be carried out.
   The challenge problem has been abstracted from the real-life
   Hidden Markov Model programs.
   https://github.com/StagedHPC/shonan-challenge/tree/master/problems/hmm
 *)

open Ring                               (* Reuse the earlier code *)
open Vector                             (* Reuse the earlier code *)
open Lifts                              (* From MetaOCaml stdlib  *)

(* ------------------------------------------------------------------------
   Part 1: Extending the earlier BLAS1 specification to deal with
   Matrix-Vector multiplications (to BLAS 2)
   We treat a matrix as a vector of vectors, most generally.
 *)

(* A Vector with reductions *)
module type VECR = sig
  include VEC
  type t                                (* element type *)
  (* a.k.a left fold *)
  val reduce : (t -> t -> t) -> t -> (idx, t) vec -> t
  (* QQQ Why can't we make reduce polymorphic in the element type,
         like the following?
  val reduce : ('a -> 'a -> 'a) -> 'a -> (idx, 'a) vec -> 'a
  *)
end;;

(* BLAS 2: Matrix-vector operations *)
module BLAS2(R: RING)(A: VECR with type t = R.t) = struct
  open R
  open A
  include BLAS1(R)(A)
  let dot v1 v2 = reduce add zero (v1 *. v2)
  let ( * ) a v = vec_map (dot v) a      (* matrix-vector mul *)
end;;

(* matrix-vector multiplication problem itself *)
module MVMULT(R: RING)(A: VECR with type t = R.t) = struct
  module L = BLAS2(R)(A)
  open L
  let mvmult vout a v =
    vout := a * v
end
;;

(*  That was very general, textbook code, which we write once and for all.
    After that, we instantiate it in various ways, as below.
*)


(* ========================================================================
   Implementations and specializations
*)

(* Extending earlier defined VEC to VECR *)
module VecRSta(T:sig type t end) = struct
  include T
  include VecSta

  let reduce plus zero (Vec (n,body))  =
    let sum = ref zero in
      for i = 0 to n-1 do
	sum := plus !sum (body i)
      done;
      !sum
  (* Functional code is better. Alas, the challenge requires imperative
     loops. After all, in the future we translate to C...
    let rec loop acc i =
      if i >= n then acc else loop (plus acc (v i)) (i+1)
    in loop zero 0
  *)
end;;

(* standard mvmult  *)

let mvmult_p : float array -> float array array -> float array -> unit =
  fun vout a v ->
    let n = Array.length vout and m = Array.length v in
      (* vector representations *)
    let vout = OVec (n,fun i v -> vout.(i) <- v) in
    let v    = Vec  (m,fun j -> v.(j)) in
    let a    = Vec  (n,fun i -> Vec (m, fun j -> a.(i).(j))) in

    let module MV = MVMULT(RingFloat)(VecRSta(RingFloat)) in
    MV.mvmult vout a v
;;

(* Sample array *)
let a = [|
  [| 0.5; 0.; 0.;  0.5; 0. |];
  [| 0.;  0.; 1.;  0.;  0. |];
  [| 0.;  1.; 0.;  0.;  0. |];
  [| 0.;  0.; 0.2; 0.3; 0.5 |];
  [| 0.;  0.; 0.3; 0.;  0.7 |]
 |];;

(* Unit test *)
let v1 = [| 1.; 2.; 3.; 4.; 5. |];;
let v1out =
  let vout = Array.make 5 0. in
  mvmult_p vout a v1; vout
;;

let _ = v1out;;


(* Likewise we can define complex mvmult: just replace the ring... *)

(* ------------------------------------------------------------------------
   Generating code for the ordinary matrix-vector multiplication
*)

(* int-code indexed vector: next-stage vectors *)
module VecRDyn(T:sig type t end) = struct
  include T
  include VecDyn

  let reduce : ('a code -> 'a code -> 'a code) -> 'a code ->
               (int code, 'a code) vec -> 'a code =
    fun plus zero (Vec (n,body)) ->
    .<let sum = ref .~zero in
      for i = 0 to (.~n)-1 do
	sum := .~(plus .<!sum>. (body .<i>.))
      done;
      !sum>.
end;;

(* Code for mvmult. The difference from mvmult is in the signature *)
let mvmult_c : (float array -> float array array -> float array -> unit) code =
 .<fun vout a v ->
    let n = Array.length vout and m = Array.length v in
      (* vector representations *)
    .~(let vout = OVec (.<n>.,fun i v -> .<vout.(.~i) <- .~v>.) in
       let v    = Vec  (.<m>.,fun j -> .<v.(.~j)>.) in
       let a    = Vec  (.<n>.,fun i ->
	                      Vec (.<m>., fun j -> .<a.(.~i).(.~j)>.)) in

       let module MV = MVMULT(RingFloatCode)(VecRDyn(RingFloatCode)) in
       MV.mvmult vout a v)		(* This line is the same *)
  >.
;;

(*
val mvmult_c : (float array -> float array array -> float array -> unit) code =
  .<
  fun vout_4  ->
    fun a_5  ->
      fun v_6  ->
        let n_7 = Array.length vout_4

        and m_8 = Array.length v_6
         in
        for i_9 = 0 to n_7 - 1 do
          vout_4.(i_9) <-
            (let sum_10 = Pervasives.ref 0.  in
             for i_11 = 0 to m_8 - 1 do
               sum_10 :=
                 ((! sum_10) +. ((v_6.(i_11)) *. ((a_5.(i_9)).(i_11))))
             done;
             ! sum_10)
        done>.
*)

(* Check the result is the same *)
let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run mvmult_c) vout a v1;
  print_code Format.std_formatter mvmult_c; print_newline()
;;

module VecRStaDim(T: sig type t end) = struct
  include T
  module M = VecRSta(T)
  type idx = int
  type unt = unit code

  let reduce = M.reduce

	(* quite like reduce, actually *)
  let seq e1 e2 = .<.~e1; .~e2>.
  let iter arr = reduce seq .<()>. arr
end;;

let mvmult_nc : int -> int ->
  (float array -> float array array -> float array -> unit) code =
 fun n m -> .<fun vout a v ->
    assert (n = Array.length vout && m = Array.length v);
    .~(let vout = OVec (n,fun i v -> .<vout.(i) <- .~v>.) in
       let v    = Vec  (m,fun j -> .<v.(j)>.) in
       let a    = Vec  (n,fun i -> Vec (m, fun j -> .<a.(i).(j)>.)) in
       let module MV = MVMULT(RingFloatCode)(VecRStaDim(RingFloatCode)) in
       MV.mvmult vout a v)
  >.
;;

let mvmult_5c = mvmult_nc 5 5;;

(* Really complete unroll *)
(*
val mvmult_5c :
  (float array -> float array array -> float array -> unit) code = .<
  fun vout_38  ->
    fun a_39  ->
      fun v_40  ->
        assert ((5 = (Array.length vout_38)) && (5 = (Array.length v_40)));
        ((((();
            vout_38.(0) <-
              (((((0. +. (((a_39.(0)).(0)) *. (v_40.(0)))) +.
                    (((a_39.(0)).(1)) *. (v_40.(1))))
                   +. (((a_39.(0)).(2)) *. (v_40.(2))))
                  +. (((a_39.(0)).(3)) *. (v_40.(3))))
                 +. (((a_39.(0)).(4)) *. (v_40.(4)))));
           vout_38.(1) <-
             (((((0. +. (((a_39.(1)).(0)) *. (v_40.(0)))) +.
                   (((a_39.(1)).(1)) *. (v_40.(1))))
                  +. (((a_39.(1)).(2)) *. (v_40.(2))))
                 +. (((a_39.(1)).(3)) *. (v_40.(3))))
                +. (((a_39.(1)).(4)) *. (v_40.(4)))));
          vout_38.(2) <-
            (((((0. +. (((a_39.(2)).(0)) *. (v_40.(0)))) +.
                  (((a_39.(2)).(1)) *. (v_40.(1))))
                 +. (((a_39.(2)).(2)) *. (v_40.(2))))
                +. (((a_39.(2)).(3)) *. (v_40.(3))))
               +. (((a_39.(2)).(4)) *. (v_40.(4)))));
         vout_38.(3) <-
           (((((0. +. (((a_39.(3)).(0)) *. (v_40.(0)))) +.
                 (((a_39.(3)).(1)) *. (v_40.(1))))
                +. (((a_39.(3)).(2)) *. (v_40.(2))))
               +. (((a_39.(3)).(3)) *. (v_40.(3))))
              +. (((a_39.(3)).(4)) *. (v_40.(4)))));
        vout_38.(4) <-
          (((((0. +. (((a_39.(4)).(0)) *. (v_40.(0)))) +.
                (((a_39.(4)).(1)) *. (v_40.(1))))
               +. (((a_39.(4)).(2)) *. (v_40.(2))))
              +. (((a_39.(4)).(3)) *. (v_40.(3))))
             +. (((a_39.(4)).(4)) *. (v_40.(4))))>.

*)

let _ =
  let vout = Array.make 5 0.0 in
  print_code Format.std_formatter mvmult_5c; print_newline();
  Runnative.run mvmult_5c vout a v1; vout

type 'a pv = Sta of 'a | Dyn of 'a code
;;

(* Forget the static knowledge
   We need to make sure that the value is liftable...
 *)
module Dyn(L:lift) = struct
  let dyn : L.t pv -> L.t code = function
  | Sta x -> L.lift x
  | Dyn x -> x
end
;;

(* Frequently used specializations (the former is for indices) *)
let dyni : int pv -> int code = let module M=Dyn(Lift_int) in M.dyn;;
let dynf : float pv -> float code = let module M=Dyn(Lift_float) in M.dyn;;

(* Operations on partially known values. Lift rings to partial values *)
(* Why we need L.lift and why it is generally a good idea to have it *)
module RingPV(STA:RING)(DYN:RING with type t = STA.t code)
             (L:lift with type t = STA.t) = struct
  type t = STA.t pv
  include Dyn(L)

  let zero = Sta STA.zero
  let one  = Sta STA.one
  let add x y =
    match (x,y) with
    | (Sta x, Sta y) -> Sta STA.(add x y)
    | (x, y) -> Dyn DYN.(add (dyn x) (dyn y))
  let sub x y =
    match (x,y) with
    | (Sta x, Sta y) -> Sta STA.(sub x y)
    | (x, y) -> Dyn DYN.(sub (dyn x) (dyn y))
  let mul x y =
    match (x,y) with
    | (Sta x, Sta y) -> Sta STA.(mul x y)
    | (x, y) -> Dyn DYN.(mul (dyn x) (dyn y))
end;;

module RingFloatPCode =
 RingPV(RingFloat)(RingFloatCode)(Lift_float);;

(* Vector with partially-known indices and elements *)
module VecRStaDyn(T: lift) = struct
  type t = T.t pv
  include Dyn(T)
  type idx = int pv
  type unt = unit code

  module VSta = VecRStaDim(T)
  module VDyn = VecRDyn(T)

    (* If the dimensions are statically known, use VecStaDim, otherwise
       treat the array as fully dynamic, VecDyn.
       We re-use previously written code.
    *)
  let iter = function
    | Vec (Sta n,body) ->
      VSta.iter (Vec (n,fun i -> body (Sta i)))
    | Vec (Dyn n,body) ->
      VDyn.iter (Vec (n,fun i -> body (Dyn i)))

  let reduce plus zero = function
    | Vec (Sta n,v) ->
        VSta.reduce plus zero (Vec (n, fun i -> v (Sta i)))
    | Vec (Dyn n,v) ->
      Dyn (VDyn.reduce (fun x y -> dyn (plus (Dyn x) (Dyn y))) (dyn zero)
	     (Vec (n,fun i -> dyn (v (Dyn i)))))
end;;


let mvmult_ac : float array array -> (float array -> float array -> unit) code =
 fun a ->
   let n  = Array.length a in
   let m  = Array.length a.(0) in
   let a  = Vec (Sta n, fun i -> Vec (Sta m,
    (fun j ->
      match (i,j) with
      | (Sta i, Sta j) -> Sta a.(i).(j)
      | (Sta i, Dyn j) -> Dyn .<a.(i).(.~j)>.
      | (i, j) -> Dyn .<a.(.~(dyni i)).(.~(dyni j))>.))) in
   .<fun vout v ->
    assert (n = Array.length vout && m = Array.length v);
    .~(let vout = OVec (Sta n, fun i v -> .<vout.(.~(dyni i)) <- .~(dynf v)>.)in
       let v    = Vec  (Sta m, fun j -> Dyn .<v.(.~(dyni j))>.) in
       let module MV = MVMULT(RingFloatPCode)(VecRStaDyn(Lift_float)) in
       MV.mvmult vout a v)
  >.
;;

(* However, we will be using various instances of MVMULT, so we abstract
   This abstraction is cheap since it is at the generation time.
   The full type is too complex to specify...
*)

type amat = {n: int; m: int; a: (int pv, (int pv, float pv) vec) vec}


let mvmult_abs : _ ->
 amat -> (float array -> float array -> unit) code =
 fun mvmult -> fun {n;m;a} ->
   .<fun vout v ->
    assert (n = Array.length vout && m = Array.length v);
   .~(let vout = OVec (Sta n, fun i v -> .<vout.(.~(dyni i)) <- .~(dynf v)>.) in
      let v    = Vec  (Sta m, fun j -> Dyn .<v.(.~(dyni j))>.) in
      mvmult vout a v)
  >.
;;

let amat1 : amat =
  let n  = Array.length a and m  = Array.length a.(0) in
  {n=n; m=m;
   a  = Vec (Sta n, fun i -> Vec (Sta m,
    (fun j ->
      match (i,j) with
      | (Sta i, Sta j) -> Sta a.(i).(j)
      | (Sta i, Dyn j) -> Dyn .<a.(i).(.~j)>.
      | (i, j) -> Dyn .<a.(.~(dyni i)).(.~(dyni j))>.)))
 }

let mvmult_ac1 =
  mvmult_abs
   (let module MV = MVMULT(RingFloatPCode)(VecRStaDyn(Lift_float)) in MV.mvmult)
   amat1;;

(*
The matrix elements are inlined. Clearly lots of opprotunites for
optimization.

val mvmult_ac1 : (float array -> float array -> unit) code = .<
  fun vout_23  ->
    fun v_24  ->
      assert ((5 = (Array.length vout_23)) && (5 = (Array.length v_24)));
      ((((();
          vout_23.(0) <-
            (((((0. +. ((v_24.(0)) *. 0.5)) +. ((v_24.(1)) *. 0.)) +.
                 ((v_24.(2)) *. 0.))
                +. ((v_24.(3)) *. 0.5))
               +. ((v_24.(4)) *. 0.)));
         vout_23.(1) <-
           (((((0. +. ((v_24.(0)) *. 0.)) +. ((v_24.(1)) *. 0.)) +.
                ((v_24.(2)) *. 1.))
               +. ((v_24.(3)) *. 0.))
              +. ((v_24.(4)) *. 0.)));
        vout_23.(2) <-
          (((((0. +. ((v_24.(0)) *. 0.)) +. ((v_24.(1)) *. 1.)) +.
               ((v_24.(2)) *. 0.))
              +. ((v_24.(3)) *. 0.))
             +. ((v_24.(4)) *. 0.)));
       vout_23.(3) <-
         (((((0. +. ((v_24.(0)) *. 0.)) +. ((v_24.(1)) *. 0.)) +.
              ((v_24.(2)) *. 0.2))
             +. ((v_24.(3)) *. 0.3))
            +. ((v_24.(4)) *. 0.5)));
      vout_23.(4) <-
        (((((0. +. ((v_24.(0)) *. 0.)) +. ((v_24.(1)) *. 0.)) +.
             ((v_24.(2)) *. 0.3))
            +. ((v_24.(3)) *. 0.))
           +. ((v_24.(4)) *. 0.7))>.


*)

let _ =
  let vout = Array.make 5 0.0 in
  print_code Format.std_formatter mvmult_ac1; print_newline();
  Runnative.run mvmult_ac1 vout v1; vout

(* Adding algebraic simplifications *)
(* This is as simple as extending RingFloatPCode and overriding
   add and sub and mul *)
module RingFloatOPCode = struct
  include RingFloatPCode
      (* inherit overrides *)
  let add x y =
    match (x,y) with
    | (Sta 0.,y) -> y
    | (x,Sta 0.) -> x
    | (x,y)      -> RingFloatPCode.add x y (* default case *)
  let sub x y =
    match (x,y) with
    | (x,Sta 0.) -> x
    | (x,y)      -> RingFloatPCode.sub x y (* default case *)
  let mul x y =
    match (x,y) with
    | (Sta 0.,_) -> Sta 0.
    | (_,Sta 0.) -> Sta 0.
    | (Sta 1.,y) -> y
    | (x,Sta 1.) -> x
    | (x,y)      -> RingFloatPCode.mul x y
end;;

(*
 QQQ Challenge: How to formulate the optimizations in terms of
                more general RingPV
*)

(* The only change is the replacement of RingFloatPCode with RingFloatOPCode *)
let mvmult_opt =
 mvmult_abs
  (let module MV = MVMULT(RingFloatOPCode)(VecRStaDyn(Lift_float)) in MV.mvmult)
  amat1;;

(*
val mvmult_opt : (float array -> float array -> unit) code = .<
  fun vout_25  ->
    fun v_26  ->
      assert ((5 = (Array.length vout_25)) && (5 = (Array.length v_26)));
      (((((); vout_25.(0) <- (((v_26.(0)) *. 0.5) +. ((v_26.(3)) *. 0.5)));
         vout_25.(1) <- (v_26.(2)));
        vout_25.(2) <- (v_26.(1)));
       vout_25.(3) <-
         ((((v_26.(2)) *. 0.2) +. ((v_26.(3)) *. 0.3)) +. ((v_26.(4)) *. 0.5)));
      vout_25.(4) <- (((v_26.(2)) *. 0.3) +. ((v_26.(4)) *. 0.7))>.

*)

let _ =
  let vout = Array.make 5 0.0 in
  Runnative.run mvmult_opt vout v1; vout

module VecRStaOptDynFloat = struct
  module R = RingFloatOPCode
  module M = VecRStaDyn(Lift_float)
  include M
  let threshold = 3			(* density threshold *)

  let count_non_zeros n vecf =
    let rec loop acc i =
      if i >= n then acc else
      let acc = if vecf (Sta i) = Sta 0. then acc else acc + 1 in
      loop acc (i+1)
    in loop 0 0

  let reduce plus zero = function
    | (Vec (Sta n,vecf)) as vec ->
	if count_non_zeros n vecf < threshold then
	 M.reduce plus zero vec
	else
         (* By making the vector length dynamic we switch off
            the loop unrolling. *)
	 M.reduce plus zero (Vec (Dyn .<n>.,vecf))
    | vec -> M.reduce plus zero vec
end;;

let mvmult_roll =
  mvmult_abs
   (let module MV = MVMULT(RingFloatOPCode)(VecRStaOptDynFloat) in MV.mvmult)
   amat1;;

(*
val mvmult_roll : (float array -> float array -> unit) code = .<
  fun vout_19  ->
    fun v_20  ->
      assert ((5 = (Array.length vout_19)) && (5 = (Array.length v_20)));
      (((((); vout_19.(0) <- (((v_20.(0)) *. 0.5) +. ((v_20.(3)) *. 0.5)));
         vout_19.(1) <- (v_20.(2)));
        vout_19.(2) <- (v_20.(1)));
       vout_19.(3) <-
         ((let sum_21 = Pervasives.ref 0.  in
           for i_22 = 0 to 5 - 1 do
             sum_21 :=
               ((! sum_21) +. ((v_20.(i_22)) *. (((* CSP a *).(3)).(i_22))))
           done;
           ! sum_21)));
      vout_19.(4) <- (((v_20.(2)) *. 0.3) +. ((v_20.(4)) *. 0.7))>.
*)

(* let _ =
 *   let vout = Array.make 5 0.0 in
 *   Runnative.run mvmult_roll vout v1; vout *)

type copy_row_t = float array -> (int code -> float code)

let amatcopy : copy_row_t -> amat = fun copy_row ->
  let n  = Array.length a and m  = Array.length a.(0) in
  {n=n; m=m;
   a  = Vec (Sta n, fun i -> Vec (Sta m,
    (fun j ->
      match (i,j) with
      | (Sta i, Sta j) -> Sta a.(i).(j)
      | (Sta i, Dyn j) -> let deref = copy_row a.(i) in (* matrix row *)
	                  Dyn (deref j)
      | _ -> failwith "not implemented yet: exercise")))
 }

(*
 QQQ implement the missing interface (hint: need to essentially copy
 all the rows of the matrix. But don't copy those that have been
 copied with copy_row.
*)


(* The first version  *)
let copy_row1 : copy_row_t = fun v ->
  let vcode =
    .<Array.of_list
      .~(List.fold_right (fun h t -> .<.~h :: .~t>.)
                (List.map Lift_float.lift
                 (Array.to_list v))
                .<[]>.)>.
  in fun idx -> .<.~vcode.(.~idx)>.
;;



let mvmult_let1 =
  mvmult_abs
  (let module MV = MVMULT(RingFloatOPCode)(VecRStaOptDynFloat) in
   MV.mvmult)
   (amatcopy copy_row1);;
(*
val mvmult_let1 : (float array -> float array -> unit) code = .<
  fun vout_23  ->
    fun v_24  ->
      assert ((5 = (Array.length vout_23)) && (5 = (Array.length v_24)));
      (((((); vout_23.(0) <- (((v_24.(0)) *. 0.5) +. ((v_24.(3)) *. 0.5)));
         vout_23.(1) <- (v_24.(2)));
        vout_23.(2) <- (v_24.(1)));
       vout_23.(3) <-
         ((let sum_25 = Pervasives.ref 0.  in
           for i_26 = 0 to 5 - 1 do
             sum_25 :=
               ((! sum_25) +.
                  ((v_24.(i_26)) *.
                     ((Array.of_list [0.; 0.; 0.2; 0.3; 0.5]).(i_26))))
           done;
           ! sum_25)));
      vout_23.(4) <- (((v_24.(2)) *. 0.3) +. ((v_24.(4)) *. 0.7))>.
*)

(* What is the problem with this code? *)

(* That's why we need genlet *)

let copy_row_let : copy_row_t = fun v ->
  let vcode = genlet
    .<Array.of_list
      .~(List.fold_right (fun h t -> .<.~h :: .~t>.)
                (List.map Lift_float.lift
                 (Array.to_list v))
                .<[]>.)>.
  in fun idx -> .<.~vcode.(.~idx)>.
;;

(* QQQ How to make copy_row generic, not just for floats?
*)

let mvmult_let2 =
  mvmult_abs
  (let module MV = MVMULT(RingFloatOPCode)(VecRStaOptDynFloat) in
   MV.mvmult)
   (amatcopy copy_row_let);;

(*
val mvmult_let2 : (float array -> float array -> unit) code = .<
  let lv_31 = Array.of_list [0.; 0.; 0.2; 0.3; 0.5]  in
  fun vout_27  ->
    fun v_28  ->
      assert ((5 = (Array.length vout_27)) && (5 = (Array.length v_28)));
      (((((); vout_27.(0) <- (((v_28.(0)) *. 0.5) +. ((v_28.(3)) *. 0.5)));
         vout_27.(1) <- (v_28.(2)));
        vout_27.(2) <- (v_28.(1)));
       vout_27.(3) <-
         ((let sum_29 = Pervasives.ref 0.  in
           for i_30 = 0 to 5 - 1 do
             sum_29 := ((! sum_29) +. ((v_28.(i_30)) *. (lv_31.(i_30))))
           done;
           ! sum_29)));
      vout_27.(4) <- (((v_28.(2)) *. 0.3) +. ((v_28.(4)) *. 0.7))>.

*)

let _ =
  let vout = Array.make 5 0.0 in
  Runnative.run mvmult_let2 vout v1; vout

let copy_row_hash : copy_row_t = fun v ->
  let n = Array.length v in
  let open Hashtbl in
  let rowv = genlet .<let row = create n in
  .~(snd @@ Array.fold_left (fun (i,acc) x -> (succ i,
    if x = 0. then acc else .<add row i x; .~acc>.)) (0,.<row>.) v)>. in
  fun idx -> .<try find .~rowv .~idx with Not_found -> 0.>.
;;


(*
 QQQ (larger) detect that a matrix is accessed sequentially and
 do something smarter to simplify the access.
*)

let mvmult_let3 =
  mvmult_abs
  (let module MV = MVMULT(RingFloatOPCode)(VecRStaOptDynFloat) in
   MV.mvmult)
   (amatcopy copy_row_hash);;

(*
val mvmult_let3 : (float array -> float array -> unit) code = .<
  let lv_37 =
    let row_36 = Hashtbl.create ?random:None 5  in
    Hashtbl.add row_36 4 0.5;
    Hashtbl.add row_36 3 0.3;
    Hashtbl.add row_36 2 0.2;
    row_36  in
  fun vout_32  ->
    fun v_33  ->
      assert ((5 = (Array.length vout_32)) && (5 = (Array.length v_33)));
      (((((); vout_32.(0) <- (((v_33.(0)) *. 0.5) +. ((v_33.(3)) *. 0.5)));
         vout_32.(1) <- (v_33.(2)));
        vout_32.(2) <- (v_33.(1)));
       vout_32.(3) <-
         ((let sum_34 = Pervasives.ref 0.  in
           for i_35 = 0 to 5 - 1 do
             sum_34 :=
               ((! sum_34) +.
                  ((v_33.(i_35)) *.
                     ((try Hashtbl.find lv_37 i_35 with | Not_found  -> 0.))))
           done;
           ! sum_34)));
      vout_32.(4) <- (((v_33.(2)) *. 0.3) +. ((v_33.(4)) *. 0.7))>.

*)


(* Note how only non-zero elements are inserted into the hash table *)

let _ =
  let vout = Array.make 5 0.0 in
  Runnative.run mvmult_let3 vout v1; vout
