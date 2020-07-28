(* Meta-programming with delimited continuations *)
(* let-insertion via shift *)
(* Generating optimal specialized code for the longest common subsequence *)

(* The following code relies on the delimcc library:
   http://okmij.org/ftp/continuations/
   Please make sure dlldelimcc.so is in your LD_LIBRARY_PATH
   or in ld.conf-described paths.
*)
open Codelib
open Delimcc

(* The original longest common subsequence (lcs) algorithm *)
(* for the two sequences, x and y. The function is to be invoked as
     lcs x y ((length x - 1),(length y - 1))
 *)
(* The direct code is from the dynamic programming staging benchmark: *)
(* http://www.metaocaml.org/examples/dp/ *)

let rec lcs x y (i,j)  =
  if i=0 || j=0 then 0
  else if x.(i) = y.(j) then 1 + lcs x y (i-1, j-1)
  else max (lcs x y (i, j-1)) (lcs x y (i-1,j))
;;

(* Problem instance, large *)
let s1L =    [| 'X';
                'a';'.';'.';'.';'b';
                '.';'.';'c';'.';'.';
                '.';'.';'.';'d';'.';
                'e';'.';'.';'.';'.';
                '.';'.';'.';'.';'f'
                |]

let s2L =[| 'X';
            'a';'_';'b';'_';'_' ; '_';
            'c';'_';'_';'_';'_' ; '_';
            'd';'_';'e';'f';'_' ; '_';
            'a';'_';'b';'_';'_' ; '_';
            'c';'_';'_';'_';'_' ; '_';
            'd';'_';'e';'f' |]
let l1L = Array.length s1L - 1
let l2L = Array.length s2L - 1;;

(* Problem instance, small *)
let s1S =    [| 'a';'b';'r';'a';'c';
                'a';'d';'a';'b';'r';
                'a'
                |]
let s2S =    [| 'X';'b';'r';'a';'c';
                'a';'d';'a';'b';'r'
                |]
let l1S = Array.length s1S - 1
let l2S = Array.length s2S - 1;;

let test_lcs = lcs s1S s2S (l1S,l2S);; (* 9 *)


(* re-writing in the open recursion style *)
(* The pair (i,j) is the memoization key *)

let lcs x y self (i,j)  =
  if i=0 || j=0 then 0
  else if x.(i) = y.(j) then 1 + self (i-1, j-1)
  else max (self (i, j-1)) (self (i-1, j))
;;


(* the simple y combinator *)
let rec y_simple f n = f (y_simple f) n
;;

let test_s1 = y_simple (lcs s1S s2S) (l1S,l2S);;
(* 9 *)


(* For the sake of the closest correspondence with circle-shift.elf,
   we use pairs to emulate 'a option data type. In the rest of the
   code, we may consider 'a option as an abstract data type.
*)
module Maybe :
 sig
   type 'a maybe
   val nothing   : 'a maybe
   val just      : 'a -> 'a maybe
   val ifnothing : 'a maybe -> bool
   val fromjust  : 'a maybe -> 'a
 end = struct
   type 'a maybe  = bool * (unit -> 'a)
   let nothing    = (true,  fun () -> failwith "nothing")
   let just x     = (false, fun () -> x)
   let ifnothing  = fst
   let fromjust x = snd x ()
end;;
open Maybe;;

(* Memo table with Maybe *)
type 'a memo_sig = int -> 'a maybe;;

let empty_fn = fun _ -> nothing
let ext_fn table n v =
  fun key -> if key = n then just v else table key
;;

let top_fn thunk =
  let p = new_prompt () in
  (push_prompt p (fun () -> let v = thunk p in fun _ -> v)) empty_fn
;;

(* Memoizing combinator, identical to that in fib.ml *)
let y_memo_fn p f =
  let lookup n = shift p (fun k -> fun table -> k (table n) table) in
  let ext n v  = shift p (fun k -> fun table ->
                                   let table' = ext_fn table n v in
				   k v table')
  in
  let rec memo n =
    let r = lookup n in 		(* do the lookup first *)
    if ifnothing r then
      let v = f memo n in ext n v
    else				(* value found *)
      fromjust r
  in f memo
;;


let test_fn_md1 = top_fn(fun p -> y_memo_fn p (lcs s1S s2S) (l1S,l2S));;
(* 9 *)
let test_fn_md2 = top_fn(fun p -> y_memo_fn p (lcs s1L s2L) (l1L,l2L));;
(* Without memoization, it takes too long ...*)
(* 6 *)

(* Specializing for the given sizes of the sequences *)

let slcs x y self (i,j)  =
  if i=0 || j=0 then .<0>.
  else
   .<if (.~x).(i) = (.~y).(j) then 1 + .~(self (i-1,j-1))
     else max .~(self (i,j-1)) .~(self (i-1,j))>.
;;


(* Staged memoizing combinator, identical to that in fib.ml *)
let y_ms p f =
  let lookup n  = shift p (fun k -> fun table -> k (table n) table) in
  let ext n v   = shift p (fun k -> fun table ->
                             .<let t = .~v in
                                 .~(let table' = ext_fn table n .<t>. in
				    k .<t>. table')>.)
  in
  let rec memo n =
    let r = lookup n in 		(* do the lookup first *)
    if ifnothing r then
      let v = f memo n in ext n v
    else				(* value found *)
      fromjust r
  in f memo
;;

let test_fn_smd lens =
  .<fun x y -> .~(top_fn(fun p -> y_ms p (slcs .<x>. .<y>.) lens))>.;;
let test_fn_smd1 () = test_fn_smd (l1S,l2S);;

let _ =
  let cde = test_fn_smd1 () in
  print_code Format.std_formatter cde; print_newline()
(*
val test_fn_smd1 : ('a, '_b array -> '_b array -> int) code =
  .<fun x_1 ->
   fun y_2 ->
    let t_3 = 0 in
    let t_4 = 0 in
    let t_5 = 0 in
    let t_6 = 0 in
    let t_7 = 0 in
    let t_8 = 0 in
    let t_9 = 0 in
    let t_10 = 0 in
    let t_11 = 0 in
    let t_12 = 0 in
    let t_13 = 0 in
    let t_14 = if (x_1.(1) = y_2.(1)) then (1 + t_13) else (max t_12 t_11) in
    let t_15 = if (x_1.(1) = y_2.(2)) then (1 + t_11) else (max t_14 t_10) in
    let t_16 = if (x_1.(1) = y_2.(3)) then (1 + t_10) else (max t_15 t_9) in
    let t_17 = if (x_1.(1) = y_2.(4)) then (1 + t_9) else (max t_16 t_8) in
    ...
    let t_110 =
     if (x_1.(10) = y_2.(7)) then (1 + t_99) else (max t_109 t_100) in
    let t_111 =
     if (x_1.(10) = y_2.(8)) then (1 + t_100) else (max t_110 t_101) in
    if (x_1.(10) = y_2.(9)) then (1 + t_101) else (max t_111 t_102)>.
*)


(* For comparison, here is the staged lcs in the monadic style
   Excerpted from http://www.metaocaml.org/examples/dp/

let lcs_mks f ((i,j), (x,y)) =
  if (i=0 || j=0) then ret .<0>.
  else
    bind (f ((i-1, j-1),(x,y))) (fun r1 ->
    bind (f ((i, j-1),(x,y)))   (fun r2 ->
    bind (f ((i-1, j),(x,y)))   (fun r3 ->
    ret .<if ((.~x).(i) = (.~y).(j)) then .~(r1) + 1
          else max .~(r2)  .~(r3)>.)))
*)
