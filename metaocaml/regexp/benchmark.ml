module type REGEXP =
sig
  type t  (** regular expression *)
  type re (** compiled regular expression *)

  val compile : t -> re
  (** Compile a regular expression into an executable version that can be
      used to match strings, e.g. with {!exec}. *)

  val exec : re -> string -> bool
  (** [exec re str] is true iff the compiled expression [re] matches [str] *)

  val (<|>) : t -> t -> t       (** r₁ | r₂ *)
  val (>>>) : t -> t -> t       (** r₁r₂ *)
  val char : char -> t          (** c *)
  val str : string -> t         (** s *)
  val empty : t                 (** matches nothing *)
  val epsilon : t               (** ε *)
  val star : t -> t             (** r* *)
  val plus : t -> t             (** r+ *)
  val opt : t -> t              (** r? *)
  val dot : t                   (** . *)
  val range : char -> char -> t (** l..h *)
end

module Re_regexp :
  REGEXP with type t = Re.t and type re = Re.re =
struct
  type t = Re.t
  type re = Re.re
  let compile t = Re.(compile (seq [start; t; stop]))
  let exec r s = Re.execp r s
  let (<|>) x y = Re.alt [x; y]
  let (>>>) x y = Re.seq [x; y]
  let char c = Re.char c
  let str s = Re.str s
  let empty, epsilon = Re.(empty, epsilon)
  let star t = Re.rep t
  let plus t = Re.rep1 t
  let opt t = Re.opt t
  let dot = Re.any
  let range = Re.rg
end

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

module Re_unstaged :
  REGEXP with type t = Regex.t =
struct

  let str l =
    List.fold_right (fun c -> Regex.(seq (chr c)))
      (explode l)
      Regex.eps

  type t = Regex.t
  type re = char list -> bool
  let compile t = Nfa.accept (Regex.compile t)
  let exec r s = r (explode s)
  let (<|>) x y = Regex.alt x y
  let (>>>) x y = Regex.seq x y
  let char c = Regex.chr c
  let str s = str s
  let empty, epsilon = Regex.(empty, eps)
  let star t = Regex.star t
  let plus t = Regex.plus t
  let opt t = Regex.opt t
  let dot = Regex.any
  let range = Regex.range
end

module Re_staged :
  REGEXP with type t = Regex.t =
struct
  include (Re_unstaged :
             module type of Re_unstaged with type re := Re_unstaged.re)
  type re = string -> bool
  let compile t =
    let code = Nfa_staged.accept (Regex.compile t) in
    let f = Runnative.run (code) in
    fun str -> (f (explode str))
  let exec r s = r s
end

module Tests(R: REGEXP) =
struct
  open R
  let repeat n re =
    let rec build i acc =
      if i = n then acc else build (succ i) (re >>> acc)
    in build 0 epsilon

  (* a?ⁿaⁿ *)
  let a_opt_n_a_n n =
    compile (repeat n (opt (char 'a')) >>> repeat n (char 'a'))

  let test_a_star_a i =
    let r = a_opt_n_a_n i and s = String.make i 'a' in
    Core.Staged.stage (fun () -> assert (exec r s))

  let ab_star_c = compile (star (char 'a' >>> char 'b') >>> char 'c')
  let make_ab_star_c_string n =
    let rec loop acc i = if i = 0 then acc else loop ("ab" :: acc) (pred i)
    in String.concat "" (loop ["c"] n)

  let test_ab_star_c n =
    let s = make_ab_star_c_string n in
    Core.Staged.stage (fun () -> assert (exec ab_star_c s))

  let star_any = compile (star dot)
  let test_any n =
    let s = String.make n '?' in
    Core.Staged.stage (fun () -> assert (exec star_any s))

  let alnum = range 'a' 'z'
          <|> range 'A' 'Z'
          <|> range '0' '9'

  let star_alnum = compile (star alnum)

  let test_alnum n =
    let s = String.make n 'a' in
    Core.Staged.stage (fun () -> assert (exec star_alnum s))
end

module Re_regexp_tests = Tests(Re_regexp)
module Re_unstaged_tests = Tests(Re_unstaged)
(* TODO: uncomment when accept4 is implemented *)
(* module Re_staged_tests = Tests(Re_staged) *)

let args = [0; 1; 2; 4; 8; 16; 32; 64; 128; 256; 512]

open Core
open Core_bench.Std

let () =
  Command.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"re_regexp (a?ⁿaⁿ)" ~args
      Re_regexp_tests.test_a_star_a;

    Bench.Test.create_indexed ~name:"re_unstaged (a?ⁿaⁿ)" ~args
      Re_unstaged_tests.test_a_star_a;

    (* TODO: uncomment when accept4 is implemented *)
    (* Bench.Test.create_indexed ~name:"re_staged (a?ⁿaⁿ)" ~args *)
    (*   Re_staged_tests.test_a_star_a; *)

    Bench.Test.create_indexed ~name:"re_regexp ((ab)*c)" ~args
      Re_regexp_tests.test_ab_star_c;

    Bench.Test.create_indexed ~name:"re_unstaged ((ab)*c)" ~args
      Re_unstaged_tests.test_ab_star_c;

    (* TODO: uncomment when accept4 is implemented *)
    (* Bench.Test.create_indexed ~name:"re_staged ((ab)*c)" ~args *)
    (*   Re_staged_tests.test_ab_star_c; *)

    Bench.Test.create_indexed ~name:"re_regexp ( .* )" ~args
      Re_regexp_tests.test_any;

    Bench.Test.create_indexed ~name:"re_unstaged ( .* )" ~args
      Re_unstaged_tests.test_any;

    (* TODO: uncomment when accept4 is implemented *)
    (* Bench.Test.create_indexed ~name:"re_staged ( .* )" ~args *)
    (*   Re_staged_tests.test_any; *)

    Bench.Test.create_indexed ~name:"re_regexp ( [a-zA-Z0-9]* )" ~args
      Re_regexp_tests.test_alnum;

    Bench.Test.create_indexed ~name:"re_unstaged ( [a-zA-Z0-9]* )" ~args
      Re_unstaged_tests.test_alnum;

    (* TODO: uncomment when accept4 is implemented *)
    (* Bench.Test.create_indexed ~name:"re_staged ( [a-zA-Z0-9]* )" ~args *)
    (*   Re_staged_tests.test_alnum; *)
  ])
