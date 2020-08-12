module CharSet = Set.Make(Char)
open Codelib

(** Q 1(a): implement 'mem_static'. *)
let mem_static : char code -> CharSet.t -> bool code =
  fun c cs ->
  let rec inner cs =
    match cs with
    | [] -> .<false>.
    | [c_st] -> .<.~c = c_st>.
    | c_st :: rest ->
      .<.~c = c_st || .~(inner rest)>.
  in
  let char_list = CharSet.fold (fun c acc -> c :: acc) cs [] in
  inner char_list

let _ =
  let cde = .<fun c -> .~(mem_static .<c>. (CharSet.of_list ['a'; 'b'; 'c'; '1'; '2'; '3'])) >. in
  print_code Format.std_formatter cde; print_newline ()

(* let char_set_to_intervals cs =
 *   let char_list = CharSet.fold (fun c acc -> c :: acc) cs [] in
 *   let sorted = List.sort Char.compare char_list in
 *   let rec inner intervals lo hi = function
 *     | hd :: rest ->
 *       if (Char.code hi) + 1 = Char.code hd then
 *         inner intervals lo hd rest
 *       else
 *         inner ((lo, hi) :: intervals) hd hd rest
 *     | [] -> (lo, hi) :: intervals
 *   in
 *   let rec initialize = function
 *     | c1 :: c2 :: rest ->
 *     if (Char.code c1 = Char.code c2)  || (Char.code c1 = (Char.code c2 - 1)) then
 *       [], Some(c1, c2, rest)
 *     else
 *       let intervals, ret = initialize rest in
 *       (c1, c1) :: (c2, c2) :: intervals, ret
 *   | [c] -> [(c, c)], None
 *   | [] -> [], None
 *   in
 *   let intervals, ret  = initialize sorted in
 *   match ret with
 *   | Some(lo, hi, rest) ->
 *     inner intervals lo hi rest
 *   | None -> intervals *)


let char_set_to_intervals =
  let add c = function
    | (l,h) :: tail when Char.code c = succ (Char.code h) -> (l, c) :: tail
    | l -> (c, c) :: l
  in fun c -> List.rev (CharSet.fold add c [])

let _ =
  let cs = CharSet.of_list ['a'; 'b'; 'c'; '1'; '2'; '3'; 'd'; 'f'] in
  let intervals = char_set_to_intervals cs in
  List.iter (fun (lo, hi) -> Printf.printf "(%c, %c)\n" lo hi) intervals

(** Q 1(b): implement 'mem_static_interval'. *)
let mem_static_interval : char code -> CharSet.t -> bool code =
  fun c cs ->
  let rec inner intervals =
    match intervals with
    | [] -> .<false>.
    | [(lo, hi)] -> .<lo <= .~c && .~c <= hi>.
    | (lo, hi) :: rest ->
     .<(lo <= .~c && .~c <= hi) || .~(inner rest)>.
  in
  let intervals = char_set_to_intervals cs in
  inner intervals

let _ =
  let cde = .<fun c -> .~(mem_static_interval .<c>. (CharSet.of_list ['a'; 'b'; 'c'; '1'; '2'; '3'])) >. in
  print_code Format.std_formatter cde; print_newline ()

module type PSChar =
sig
  type ps
  (** The type of partially-static characters *)

  val inj : char code -> ps
  (** Make a partially-static character from a dynamic character *)

  val in_range : ps -> char*char ->
    (ps -> 'a code) -> (ps -> 'a code) -> 'a code
  (** Test whether a character lies within a range *)
end

(** Q 1(c): implement PSChar *)
module PSChar : PSChar = struct
  type ps = char code * (char * char)
  let inj cde =
    (cde, (Char.chr 0, Char.chr 255))

  let in_range c (lo, hi) k1 k2 =
    let (c_dyn, (lo_known, hi_known)) = c in
    Printf.printf "(lo_known, hi_known): (%c, %c), (lo, hi): (%c, %c)\n" lo_known hi_known lo hi;
    if lo_known = lo && hi = hi_known then
      (k1 c)
    else if lo_known = lo && hi < hi_known then
      .<if .~c_dyn > hi then
          .~(k2 (c_dyn, (hi , hi_known)))
      else
         .~(k1 (c_dyn, (lo, hi)))
         >.
    else if lo_known < lo && hi = hi_known then
      .<if .~c_dyn < lo then
          .~(k2 (c_dyn, (lo_known, lo)))
      else
         .~(k1 (c_dyn, (lo, hi)))
         >.
    else if lo_known < lo && hi < hi_known then begin
      Printf.printf "foo1\n";
      .<if lo <= .~c_dyn && .~c_dyn <= hi then
        .~(k1 (c_dyn, (lo, hi)))
      else if lo > .~c_dyn then
          .~(k2 (c_dyn, (lo_known, lo)))
      else (* if .~c_dyn > hi then *)
         .~(k2 (c_dyn, (hi, hi_known)))
         >.
      end
    else if lo_known <= lo && hi > hi_known && lo < hi_known then begin
      Printf.printf "foo2\n";
      .<if lo <= .~c_dyn then begin
       .~(k1 (c_dyn, (lo, hi_known)))
     end
     else
       .~(k2 (c_dyn, (lo_known, lo)))
       >.
   end
    else if lo_known > lo && hi <= hi_known && lo_known < hi then begin
      Printf.printf "foo3\n";
     .<if hi <= .~c_dyn then
       .~(k2 (c_dyn, (hi, hi_known)))
     else
       .~(k1 (c_dyn, (lo_known, hi)))
       >.
        end
    else begin
      Printf.printf "foo4\n";
      k2 (c_dyn, (lo_known, hi_known))
    end

end

(** Q 1(d): implement mem_ps *)

let mem_ps : 'a. PSChar.ps -> CharSet.t ->
  (PSChar.ps -> 'a code) -> (PSChar.ps -> 'a code) -> 'a code =
  fun c cs k1 k2 ->
  (* let (c_dyn, (lo_known, hi_known)) = c in *)
  let intervals = char_set_to_intervals cs in
  List.iter (fun (lo, hi) -> Printf.printf "interval: %c, %c\n" lo hi) intervals;
  let rec inner c = function
    | [] -> k2 c
    | (lo, hi) :: rest ->
      let cde = PSChar.in_range c (lo, hi)
          (fun c_in_range -> k1 c_in_range)
          (fun c_out_range -> inner c_out_range rest) in
      cde
  in
  inner c intervals



let example =
  .< fun c -> .~(mem_ps (PSChar.inj .<c>.)
                   (CharSet.of_list ['a';'b';'c';'d';'e';'f'])
                   (fun p -> mem_ps p
                       (CharSet.of_list ['b';'d';'e';'f'])
                       (fun _ -> .<"w">.)
                       (fun _ -> .<"x">.))
                   (fun p -> mem_ps p
                       (CharSet.of_list ['c';'d';'e'])
                       (fun _ -> .<"y">.)
                       (fun _ -> .<"z">.))) >.

let _ =
  print_code Format.std_formatter example; print_newline ()

(* let example2 =
 *   .< fun c -> .~(mem_ps (PSChar.inj .<c>.)
 *                    (CharSet.of_list ['a';'b';'c';'d';'e';'f';'x';'y';'z'])
 *                    (fun _ -> .<true>.)
 *                    (fun _ -> .<false>.))>.
 * let _ =
 *   print_code Format.std_formatter example2; print_newline () *)
