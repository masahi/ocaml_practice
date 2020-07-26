open Codelib

let tests = [
    ("aab", "aaab", true);
    ("aab", "eaab", true);
    ("aab", "eaabf", true);
    ("aab", "aabf", true);
    ("aab", "abaab", true);

    ("aab", "ab", false);
    ("aab", "eaa", false);
    ("aab", "aacb", false);
    ("aab", "aa", false);
    ("aab", "a", false);
    ("aab", "", false);

    ("abacd", "abacabacd", true);
    ("abacd", "abacabacad", false);
  ]

let test_function name f =
  List.iter (fun (n, h, expected) ->
      match f n h = expected, expected with
      | true, _ -> ()
      | false, false ->
         Printf.ksprintf failwith "expected %s not to find %s in %s, but it did"
        name n h
      | false, true ->
         Printf.ksprintf failwith "expected %s to find %s in %s, but it did not"
        name n h) tests


(** A simple (some might say "naive") string search algorithm *)
let ssearch p s =
  let rec loop pi si =
    if pi = String.length p then true
    else if si + pi = String.length s then false
    else if p.[pi] = s.[si + pi] then loop (pi + 1) si
    else loop 0 (si + 1)
  in loop 0 0

let () = test_function "ssearch" ssearch


(** Let's rewrite the algorithm at a slightly higher level,
    using a sliding window to view the haystack.

    Using a window will allow us to avoid some index
    manipulations: we can make do with a single index
    for both needle and haystack. *)

module type window =
sig
  type t and repr and index and elem

  (* Create a view onto a string *)
  val view : repr -> index -> t

  (* Retrieve an element from a string *)
  val get : t -> index -> elem

  (* Slide the window along one character *)
  val slide : t -> t
end

(** A concrete implementation of sliding
    windows for standard-issue strings *)
module Window : window with type repr := string
                        and type index := int
                        and type elem := char option =
struct
  type t = string * int
  let view s i = (s, i)
  let get (s, i) j =
    if j < String.length s - i then Some (s.[i + j])
    else None
  let slide (s, i) = (s, i+1)
end



(** Here's the search procedure again, rewritten
    to use the sliding window.  The rest of our
    time will be spent staging this algorithm *)
let ssearch_window p s =
  let rec loop i w =
    if String.length p = i then true else
      match Window.get w i with
      | None -> false
      | Some c when c = p.[i] -> loop (i + 1) w
      | Some _ -> loop 0 (Window.slide w)
  in loop 0 (Window.view s 0)

let () = test_function "window" ssearch_window






(** Our plan is to specialize the algorithm with respect to
    the search pattern.

    We'll try a few different approaches, based around
    various ways of handling recursion
 *)


(** First, let's treat the pattern as static data.
    This amounts to unrolling the loop. *)

(** Here's a second implementation of windows, using
    dynamic (code) values *)
module WindowS : sig include window with type repr := string code
                        and type index := int code
                        and type elem := char option code

                     (** (We expose an extra function for converting back to code) *)
                     val cd : t -> (string * int) code
                 end =
struct
  type t = string code * int code
  let view s i = (s, i)
  let get (s, i) j = .< if .~j < String.length .~s - .~i
                        then Some (.~s).[.~i + .~j]
                        else None >.
  let slide (s, i) = (s, .<.~i+1>.)
  let cd (s, i) = .< (.~s, .~i) >.
end

let liftchr (c : char) : char code = .<c>.

(** Here's a first attempt.  We make the needle static
    and the haystack dynamic, then annotate accordingly.

    There's a problem here!  Can you spot it? *)
let ssearch_careless p = .< fun s ->
     .~(let rec loop i w =
        if String.length p = i then .<true>.
        else .<match .~(WindowS.get w .<i>.) with
               | None -> false
               | Some c when c = .~(liftchr p.[i]) -> .~(loop (i + 1) w)
               | Some _ -> .~(loop 0 (WindowS.slide w)) >.
      in loop 0 (WindowS.view .<s>. .<0>.)) >.

(* let () = test_function "ssearch_careless" (fun pat -> Runcode.run (ssearch_careless pat));; *)


(** Here's a second attempt, written with a little more care.

    In principle we have

       restart i = loop 0 (s, i)

    where 'restart' is static and 'loop' is dynamic.

    MetaOCaml doesn't support mutual recursion between static and dynamic
    functions, so we nest 'loop' within 'restart' instead.
 *)
let ssearch_2 p = .< fun s ->
    let rec restart i =
     .~(let rec loop (i : int) w =
        if String.length p = i then .<true>.
        else .<match .~(WindowS.get w .<i>.) with
               | None -> false
               | Some c when c = .~(liftchr p.[i]) -> .~(loop (i + 1) w)
               | Some _ -> restart (snd .~(WindowS.(cd (slide w)))) >.
      in loop 0 (WindowS.view .<s>. .<i>.))
    in restart 0 >.

let () =
  print_code Format.std_formatter (ssearch_2 "aab"); print_newline ();
  test_function "ssearch_2" (fun pat -> Runcode.run (ssearch_2 pat))

(** NB: try viewing the output. *)

(** Unrolling isn't the only approach.  We could instead
    try building a mutually-recursive group, indexing by
    pattern suffixes.

    For example, for the pattern "needle", we'll generate
    functions that match "needle", "eedle", "edle", etc.
 *)
let ssearch_3 p =
  .< fun s ->
    .~(Letrec.letrec (fun loop i -> .< fun (s,j) ->
        .~(let w = WindowS.(view .<s>. .<j>.) in
          (if String.length p = i then .<true>.
            else .<match .~WindowS.(get w .<i>.) with
                   | None -> false
                   | Some c when c = .~(liftchr p.[i]) ->  .~(loop (i + 1)) .~WindowS.(cd w)
                   | Some _ ->  .~(loop 0) .~WindowS.(cd (slide w)) >.))>.)
        (fun loop -> .< .~(loop 0) (s,0)>.))>.


let () =
  print_code Format.std_formatter (ssearch_3 "aab"); print_newline ();
  test_function "ssearch_3" (fun pat -> Runcode.run (ssearch_3 pat))

(** NB: view the generated code! *)









(** Finally, let's try a more sophisticated approach to indexing.

    Rather than indexing by the needle, let's index by the haystack.

    How can we index by a dynamic value?  Note that, although
    we don't know the full value of the haystack in advance,
    we do learn some information about it as we attempt to match.

    The information itself is only discovered at "runtime";
    however, we can work out some properties of the haystack
    string by where we are in the program.  For example,
    if we're in the 'then' branch of the test s.[i] = 'a'
    then we know that s.[i] will be 'a'.
 *)

(** Preliminaries: we'll keep a map of the characters
    in the string whose values we've learnt. *)
module Int : Map.OrderedType with type t = int = struct
  type t = int
  let compare = Pervasives.compare
end
module IntMap = Map.Make(Int)

(** Here's our main structure.
    The interface is essentially the same as the earlier 'window'
    but we have:
     - a mixed static-dynamic element type
     - additional functions 'refine' and 'info' for extending
       and retrieving the information about individual characters
 *)
module PSWindow : sig
  type t
  type repr = string code * char IntMap.t
  type index = int code
  type elem = [`Known of char | `Unknown of char option code]
  val view : repr -> index -> t
  val get : t -> int -> elem
  val slide : t -> t
  val refine : t -> int -> char -> t
  val info : t -> int code * char IntMap.t
end =
struct
  type repr = string code * char IntMap.t
  type t = repr * int code
  type index = int code
  type elem = [`Known of char | `Unknown of char option code]
  let view (repr : repr) (i : int code) = (repr, i)
  let get (((s,cs), i) : t) j =
    match IntMap.find j cs with
    | x -> `Known x
    | exception Not_found ->
       `Unknown .< if String.length .~s - .~i > j
                   then Some .~s.[.~i + j]
                   else None >.
  let slide ((s,m), i) =
    ((s, IntMap.fold (fun i c m -> if i - 1 >= 0 then IntMap.add (i - 1) c m else m) m IntMap.empty),
     .< .~i + 1 >.)
  let refine ((s,m), i) j c = ((s, IntMap.add j c m), i)
  let info ((_,m),i) = (i, m)
end

let liftstr : string -> string code = fun s -> .<s>.

(** Here's the key auxiliary function that replaces the
    fully-static or fully-dynamic 'match' from earlier
    attempts.

    One important thing to note is the argument of `Yes:
    if we match successfully then we may refine the
    information about the haystack using what we've learned.
 *)
let match_index : PSWindow.t -> int -> char ->
                  ([> `Invalid | `No | `Yes of PSWindow.t ] -> 'a code)
                  -> 'a code =
fun w i c k ->
  match PSWindow.get w i with
  | `Known c' when c = c' -> k (`Yes w)
  | `Known _ -> k `No
  | `Unknown co ->
     .< match .~co with None -> .~(k `Invalid)
                      | Some c' when c = c' -> .~(k (`Yes (PSWindow.refine w i c)))
                      | Some _ -> .~(k `No) >.

(** Finally, here's the algorithm again, staged using PSWindow.
    Note that, while we have some staging clutter such as
    conversions between static/dynamic/partially-static types,
    the structure of the algorithm is unchanged
*)

let ssearch_4 p = .< fun s ->
     .~(Letrec.letrec (fun restart sinf -> .< fun j -> .~(let w = PSWindow.view (.<s>., sinf) .<j>. in
        let rec loop i w =
           if String.length p = i then .<true>.
           else match_index w i p.[i] @@ function
                | `Invalid -> .<false>.
                | `Yes w -> loop (i + 1) w
                | `No -> let j, map = PSWindow.info (PSWindow.slide w) in .< .~(restart map) .~j >.
            in loop 0 w) >.)
       (fun restart -> .< .~(restart IntMap.empty) 0 >.)) >.

(** NB: look at the generated code! *)

let () =
  print_code Format.std_formatter (ssearch_4 "aab"); print_newline ();
  test_function "ssearch_4" (fun pat -> Runcode.run (ssearch_4  pat));;
