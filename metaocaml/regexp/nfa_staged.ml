open Codelib
open Nfa

module CharSet = Set.Make(Char)

(** A 'letrec' code generator (see Lecture 16) that uses StateSet.t as
    the index type *)
let letrec rhs resolve = Letrec.letrec ~equal:StateSet.equal rhs resolve

(** Q 2(a)(i): implement 'splitc'. *)
let splitc : 'a. char code -> (char -> 'a code) -> 'a code =
  fun c k ->
  (* let char_list = List.init 256 Char.chr in
   * let rec inner lst base =
   *   match lst with
   *   | c_st :: rest -> .<if .~c = c_st then .~(k c_st) else .~(inner rest base)>.
   *   | [] -> base
   * in
   * inner char_list .<assert false>. *)
 .<match .~c with
 | 'a' -> .~(k 'a')
 | 'b' -> .~(k 'b')
 | 'c' -> .~(k 'c')
 | 'd' -> .~(k 'd')
 | 'e' -> .~(k 'e')
 | 'f' -> .~(k 'f')
 | 'h' -> .~(k 'h')
 | 'i' -> .~(k 'i')
 | 's' -> .~(k 's')
  | '*' -> .~(k '*')
  | _ ->
    Printf.printf "%c\n" .~c;
    assert false
    >.

let makeau:
  nfa ->
  (StateSet.t -> (char list -> bool) code) ->
  (StateSet.t -> (char list -> bool) code) =
  fun nfa self state_set ->
  let accept = StateSet.(not (is_empty (inter state_set nfa.finals))) in
  .<fun stream ->
  match stream with
    | hd :: tl ->
      .~(splitc .<hd>. (fun c ->
          let next_states = nextss state_set c nfa in
          self next_states)) tl
    | [] -> accept>.

(** Q 2(a)(ii): implement 'accept' using splitc *)

(** A staged version of the original naive NFA *)
let accept : Nfa.nfa -> (char list -> bool) code =
  fun nfa ->
    Letrec.letrec (makeau nfa) (fun test_fun -> test_fun (StateSet.singleton nfa.start))

(* (\** Q 2(b)(i):implement 'splitc2' *\)
 * let splitc2 : Nfa.nfa -> StateSet.t -> char code -> (char -> bool code) -> bool code =
 *   fun nfa cur c k -> (\* [ANSWER] *\) failwith "Nfa_staged.splitc2 not implemented"
 *
 * (\** Q 2(b)(ii): implement 'accept2' using 'splitc2' *\)
 * let accept2 : Nfa.nfa -> (char list -> bool) code =
 *   fun nfa -> (\* [ANSWER] *\) failwith "Nfa_staged.accept2 not implemented"
 *
 * (\** Q 2(c): implement 'accept3' *\)
 * let accept3 : Nfa.nfa -> (string -> bool) code =
 *   fun nfa -> (\* [ANSWER] *\) failwith "Nfa_staged.accept3 not implemented"
 *
 * (\** Q 2(d)(i): implement 'splitc3' *\)
 * let splitc3 :
 *   nfa -> StateSet.t -> char code -> (StateSet.t -> bool code) -> bool code =
 *   fun nfa cur c k ->
 *     (\* [ANSWER] *\) failwith "Nfa_staged.splitc3 not implemented"
 *
 * (\** Q 2(d)(ii): implement 'accept4' using 'splitc3' *\)
 * let accept4 : Nfa.nfa -> (string -> bool) code =
 *   fun nfa -> (\* [ANSWER] *\) failwith "Nfa_staged.accept4 not implemented" *)
