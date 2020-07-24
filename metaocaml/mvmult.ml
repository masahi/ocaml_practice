open Codelib

open Ring
open Vector
open Lifts


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

type 'a pv = Sta of 'a | Dyn of 'a code

module Dyn(L:lift) = struct
  let dyn : L.t pv -> L.t code = function
    | Sta x -> L.lift x
    | Dyn x -> x
end

let dyni : int pv -> int code =
  let module M = Dyn(Lift_int) in
  M.dyn

let dynf : float pv -> float code =
  let module M = Dyn(Lift_float) in
  M.dyn

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

end

module RingFloatPCode =
  RingPV(RingFloat)(RingFloatCode)(Lift_float)

module VecRStaDyn(T: lift) = struct
  type t = T.t pv
  include Dyn(T)
  type idx = int pv
  type unt = unit code

  module VSta = VecRStaDim(T)
  module VDyn = VecRDyn(T)

  let iter = function
    | Vec (Sta n, body) ->
      VSta.iter (Vec (n, fun i -> body (Sta i)))
    | Vec (Dyn n, body) ->
      VDyn.iter (Vec (n, fun i -> body (Dyn i)))

  let reduce plus zero = function
    | Vec (Sta n,v) ->
        VSta.reduce plus zero (Vec (n, fun i -> v (Sta i)))
    | Vec (Dyn n,v) ->
      Dyn (VDyn.reduce (fun x y -> dyn (plus (Dyn x) (Dyn y))) (dyn zero)
	     (Vec (n,fun i -> dyn (v (Dyn i)))))

end

let mvmult_ac : float array array -> (float array -> float array -> unit) code =
 fun a ->
   let n  = Array.length a in
   let m  = Array.length a.(0) in
   let a  = Vec (Sta n, fun i -> Vec (Sta m,
                                      (fun j ->
                                         match i, j with
                                         | Sta i, Sta j -> Sta a.(i).(j)
                                         | _ -> assert false)))
   in
   .<fun vout v ->
     assert (n = Array.length vout && m = Array.length v);
     .~(let vout = OVec (Sta n, fun i v -> .<vout.(.~(dyni i)) <- .~(dynf v)>.)
        in
        let v = Vec (Sta m, fun j -> Dyn .<v.(.~(dyni j))>.) in
        let module MV = MVMULT(RingFloatPCode)(VecRStaDyn(Lift_float)) in
        MV.mvmult vout a v)
       >.

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run (mvmult_ac a)) vout v1;
  print_code Format.std_formatter (mvmult_ac a); print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout

type amat = {n: int; m: int; a: (int pv, (int pv, float pv) vec) vec}

let mvmult_abs : _ -> amat -> (float array -> float array -> unit) code =
  fun mvmult -> fun {n; m; a} ->
   .<fun vout v ->
     assert (n = Array.length vout && m = Array.length v);
     .~(let vout = OVec (Sta n, fun i v -> .<vout.(.~(dyni i)) <- .~(dynf v)>.)
        in
        let v = Vec (Sta m, fun j -> Dyn .<v.(.~(dyni j))>.) in
        mvmult vout a v)
       >.

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
   amat1

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run mvmult_ac1) vout v1;
  print_code Format.std_formatter mvmult_ac1; print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout

module RingFloatOPCode = struct
  include RingFloatPCode
  let add x y =
    match (x,y) with
    | (Sta 0.,y) -> y
    | (x,Sta 0.) -> x
    | (x,y)      -> RingFloatPCode.add x y
  let sub x y =
    match (x,y) with
    | (x,Sta 0.) -> x
    | (x,y)      -> RingFloatPCode.sub x y
  let mul x y =
    match (x,y) with
    | (Sta 0.,_) -> Sta 0.
    | (_,Sta 0.) -> Sta 0.
    | (Sta 1.,y) -> y
    | (x,Sta 1.) -> x
    | (x,y)      -> RingFloatPCode.mul x y
end

let mvmult_opt =
  mvmult_abs
   (let module MV = MVMULT(RingFloatOPCode)(VecRStaDyn(Lift_float)) in MV.mvmult)
   amat1

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run mvmult_opt) vout v1;
  print_code Format.std_formatter mvmult_opt; print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout

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
   amat1

let _ =
  (* let vout = Array.make 5 0.0 in *)
  (* (Runnative.run mvmult_roll) vout v1; *)
  print_code Format.std_formatter mvmult_roll; print_newline()
  (* Array.iter (fun v -> Printf.printf "%f\n" v ) vout *)

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

let copy_row1: copy_row_t = fun v ->
  let vcode =
    .<Array.of_list
    .~(List.fold_right (fun h t -> .<.~h :: .~t>.)
         (List.map Lift_float.lift
            (Array.to_list v))
         .<[]>.)>.
  in
  fun idx -> .<.~vcode.(.~idx)>.

let mvmult_let1 =
  mvmult_abs
   (let module MV = MVMULT(RingFloatOPCode)(VecRStaOptDynFloat) in MV.mvmult)
   (amatcopy copy_row1)

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run mvmult_let1) vout v1;
  print_code Format.std_formatter mvmult_let1; print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout

let copy_row_let: copy_row_t = fun v ->
  let vcode = genlet
    .<Array.of_list
    .~(List.fold_right (fun h t -> .<.~h :: .~t>.)
         (List.map Lift_float.lift
            (Array.to_list v))
         .<[]>.)>.
  in
  fun idx -> .<.~vcode.(.~idx)>.

let mvmult_let2 =
  mvmult_abs
   (let module MV = MVMULT(RingFloatOPCode)(VecRStaOptDynFloat) in MV.mvmult)
   (amatcopy copy_row_let)

let _ =
  let vout = Array.make 5 0.0 in
  (Runnative.run mvmult_let1) vout v1;
  print_code Format.std_formatter mvmult_let2; print_newline();
  Array.iter (fun v -> Printf.printf "%f\n" v ) vout
