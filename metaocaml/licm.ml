(* Loop-invariant code motion with generic let: the example in the talk
   See the annotated slides in the talk for explanation.
*)

(* We will be using delimited control. So we load it up and set up *)
(*
#directory "/home/oleg/Cache/ncaml4/caml-shift/";;
#directory "/usr/local/src/ncaml4/caml-shift/";;
#load "delimcc.cma";;
#use "gengenlet.ml"
*)

open Codelib
open Gengenlet
(* The example is matrix-matrix multiplication: C = A * B
   We will write in generically, in a simple DSL for linear
   algebra defined below.
   See Shonan1.html for a more elaborated DSL.
*)

module type LINALG = sig
  type tdom                             (* type of scalars *)
  type tind                             (* type of the index *)
  type tunit                            (* the unit type in our DSL *)
  type tmatrix                          (* the matrix type *)
  val ( + ) : tdom -> tdom -> tdom      (* operations on scalars *)
  val ( * ) : tdom -> tdom -> tdom
  val mat_dim : tmatrix -> tind * tind  (* get the dimensions of a matrix *)
  val mat_get : tmatrix -> tind -> tind -> tdom
  val mat_incr :                        (* increment an element *)
      tmatrix -> tind -> tind ->  tdom -> tunit
          (* loop n (fun i -> body): iterate i in [0..n-1] *)
  val loop : tind -> (tind -> tunit) -> tunit
end

(* The multiplication itself:
   Compute C = A * B assuming the matrix C is zeroed out
*)
module MMUL0(S: LINALG) = struct
   open S
   let mmul a b c =
   loop (fst (mat_dim a)) @@ fun i ->
     loop (snd (mat_dim b)) @@ fun j ->
       loop (fst (mat_dim b)) @@ fun k ->
         mat_incr c i j @@ mat_get a i k * mat_get b k j
end

(* One implementation of LINANG: present-stage code for int matrix
   multiplication
*)
module LAint = struct
  type tdom = int
  type tind = int
  type tunit = unit
  type tmatrix = int array array
  let ( + )  = Pervasives.( + )
  let ( * )  = Pervasives.( * )
  let mat_dim a = (Array.length a, Array.length a.(0))
  let mat_get a i j = a.(i).(j)
  let mat_incr a i j v = a.(i).(j) <- a.(i).(j) + v
  let loop n body =
   for i=0 to n-1 do body i done
end

(* Testing *)

(* Sample matrices *)
let a = Array.make_matrix 5 10 0;;
let dimx a = Array.length a;;
let dimy a = Array.length a.(0);;

for i=0 to dimx a - 1 do
 for j=0 to dimy a - 1 do
  a.(i).(j) <- i + j
done done;;

let b = Array.make_matrix 10 7 0;;
for i=0 to Array.length b - 1 do
 for j=0 to Array.length b.(0) - 1 do
  b.(i).(j) <- i + j + 1
done done;;

let c_result =
 [|[|330; 375; 420; 465; 510; 555; 600|];
   [|385; 440; 495; 550; 605; 660; 715|];
   [|440; 505; 570; 635; 700; 765; 830|];
   [|495; 570; 645; 720; 795; 870; 945|];
   [|550; 635; 720; 805; 890; 975; 1060|]|];;

let c = Array.make_matrix 5 7 0 in
 let module M = MMUL0(LAint) in M.mmul a b c; assert (c = c_result)


(* interchange two loops to improve locality *)
module MMUL(S: LINALG) = struct
   open S
   let mmul a b c =
   loop (fst (mat_dim a)) @@ fun i ->
     loop (fst (mat_dim b)) @@ fun k ->
       loop (snd (mat_dim b)) @@ fun j ->
         mat_incr c i j @@ mat_get a i k * mat_get b k j
end;;

let c = Array.make_matrix 5 7 0 in
 let module M = MMUL(LAint) in M.mmul a b c; assert (c = c_result)

(* Implementation to generate code *)
module LAintcode = struct
  type tdom = int code
  type tind = int code
  type tunit = unit code
  type tmatrix = int array array code
  let ( + )  = fun x y -> .<.~x + .~y>.
  let ( * )  = fun x y -> .<.~x * .~y>.
  let mat_dim a = (.<Array.length .~a>.,
                   .<Array.length (.~a).(0)>.)
  let mat_get a i j = .<(.~a).(.~i).(.~j)>.
  let mat_incr a i j v =
   .<(.~a).(.~i).(.~j) <- .~(mat_get a i j + v)>.
  let loop n body =
   .<for i=0 to .~n-1 do .~(body .<i>.) done>.
end

let smmul1 : (int array array -> int array array ->
              int array array -> unit) code =
 .<fun a b c ->
   .~(let module M = MMUL(LAintcode) in
      M.mmul .<a>. .<b>. .<c>.)
   >.
;;

(*
val smmul1 :
  (int array array -> int array array -> int array array -> unit) code = .<
  fun a_1  b_2  c_3  ->
    for i_4 = 0 to (Array.length a_1) - 1 do
      for i_5 = 0 to (Array.length b_2) - 1 do
        for i_6 = 0 to (Array.length (b_2.(0))) - 1 do
          (c_3.(i_4)).(i_6) <-
          ((c_3.(i_4)).(i_6)) + (((a_1.(i_4)).(i_5)) * ((b_2.(i_5)).(i_6)))
        done
      done
    done>.

*)

let c = Array.make_matrix 5 7 0 in
 print_code Format.std_formatter smmul1; print_newline();
 Runnative.run smmul1 a b c; assert (c = c_result)

(* Let's partially unroll loops by a given factor
   (This is what SPIRAL does)
*)
module LAintcode_unroll (S:sig val unroll_factor : int end) = struct
  include LAintcode
  let loop n body =
    let uf1 = S.unroll_factor - 1 in
   .<let i = ref 0 in
     while (!i < .~n - uf1) do
      .~Pervasives.(let rec unroll acc j =
        if j > S.unroll_factor - 1 then acc
        else unroll .<(.~acc; .~(body .<!i+j>.))>. (j+1)
        in unroll (body .<!i>.) 1);
      i := Pervasives.(!i + S.unroll_factor)
     done;
   for i1 = !i to .~n-1 do .~(body .<i1>.) done
>.
end

let smmul2 : (int array array -> int array array ->
              int array array -> unit) code =
 .<fun a b c ->
   .~(let module M = MMUL(LAintcode_unroll(struct let unroll_factor = 2 end)) in
      M.mmul .<a>. .<b>. .<c>.)
   >.
;;
(* The generated code is a mess. Let's just test it *)

let c = Array.make_matrix 5 7 0 in
 print_code Format.std_formatter smmul2; print_newline();
 Runnative.run smmul2 a b c; assert (c = c_result)

(* Adding genlet *)

(* Implementation with genlet movement *)
module LAintcode_opt = struct
  include LAintcode
  let mat_get a i j = genlet @@ LAintcode.mat_get a i j
  (* Indicate that the places before and after the loop are
     good locations to insert the code at.
  *)
  let loop n body =
    let_locus (fun () ->
      LAintcode.loop n
        (fun i -> let_locus (fun () -> body i)))
end

(* doing let-insertion *)
let smmul3 : (int array array -> int array array ->
              int array array -> unit) code =
 .<fun a b c ->
   .~(let module M = MMUL(LAintcode_opt) in
      M.mmul .<a>. .<b>. .<c>.)
   >.
;;

(*
val smmul3 :
  (int array array -> int array array -> int array array -> unit) code = .<
  fun a_49  b_50  c_51  ->
    for i_52 = 0 to (Array.length a_49) - 1 do
      for i_53 = 0 to (Array.length b_50) - 1 do
        let t_56 = (a_49.(i_52)).(i_53) in
        for i_54 = 0 to (Array.length (b_50.(0))) - 1 do
          let t_55 = (b_50.(i_53)).(i_54) in
          (c_51.(i_52)).(i_54) <- ((c_51.(i_52)).(i_54)) + (t_56 * t_55)
        done
      done
    done>.
*)

let c = Array.make_matrix 5 7 0 in
  print_code Format.std_formatter smmul3; print_newline();
  Runnative.run smmul3 a b c; assert (c = c_result)
