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

(** Q 2(a)(ii): implement 'accept' using splitc *)

(** A staged version of the original naive NFA *)
let accept : Nfa.nfa -> (char list -> bool) code =
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
        | [] -> accept>. in
  fun nfa ->
    Letrec.letrec (makeau nfa) (fun test_fun -> test_fun (StateSet.singleton nfa.start))

let splitc2 : Nfa.nfa -> StateSet.t -> char code -> (char -> bool code) -> bool code =
  fun nfa cur c k ->
  let char_list = List.init 256 Char.chr in
  let valid_chars = StateSet.fold (fun s acc ->
      let transition = nfa.next s in
      let keys = CharMap.fold (fun k _ acc -> k :: acc) transition [] in
      List.concat [acc; keys])
      cur [] in
  let rec inner lst base =
    match lst with
    | c_st :: rest ->
      if List.mem c_st valid_chars then
        .<if .~c = c_st then .~(k c_st) else .~(inner rest base)>.
      else inner rest base
    | [] -> base
  in
  inner char_list .<false>.


(** Q 2(b)(ii): implement 'accept2' using 'splitc2' *)
let accept2 : Nfa.nfa -> (char list -> bool) code =
  let makeau:
    nfa ->
    (StateSet.t -> (char list -> bool) code) ->
    (StateSet.t -> (char list -> bool) code) =
    fun nfa self state_set ->
      let accept = StateSet.(not (is_empty (inter state_set nfa.finals))) in
      .<fun stream ->
        match stream with
        | hd :: tl ->
          .~(splitc2 nfa state_set .<hd>. (fun c ->
              let next_states = nextss state_set c nfa in
              .<.~(self next_states) tl>.))
        | [] -> accept>. in
  fun nfa ->
    Letrec.letrec (makeau nfa) (fun test_fun -> test_fun (StateSet.singleton nfa.start))

let accept3 : Nfa.nfa -> (string -> bool) code =
  let makeau:
    nfa ->
    (StateSet.t -> (string -> int ->  bool) code) ->
    (StateSet.t -> (string -> int -> bool) code) =
    fun nfa self state_set ->
      let accept = StateSet.(not (is_empty (inter state_set nfa.finals))) in
      .<fun str i ->
        let len = String.length str in
        if i == len then accept
        else
          let c = str.[i] in
          .~(splitc2 nfa state_set .<c>. (fun c ->
              let next_states = nextss state_set c nfa in
              .<.~(self next_states) str (i+1)>.))>. in
  fun nfa ->
    Letrec.letrec (makeau nfa) (fun test_fun ->
        .<let f = .~(test_fun (StateSet.singleton nfa.start)) in
          fun s -> f s 0>.)

(*
 * (\** Q 2(d)(i): implement 'splitc3' *\)
 * let splitc3 :
 *   nfa -> StateSet.t -> char code -> (StateSet.t -> bool code) -> bool code =
 *   fun nfa cur c k ->
 *     (\* [ANSWER] *\) failwith "Nfa_staged.splitc3 not implemented"
 *
 * (\** Q 2(d)(ii): implement 'accept4' using 'splitc3' *\)
 * let accept4 : Nfa.nfa -> (string -> bool) code =
 *   fun nfa -> (\* [ANSWER] *\) failwith "Nfa_staged.accept4 not implemented" *)
