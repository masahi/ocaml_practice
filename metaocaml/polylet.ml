open Codelib

(* An illustration of a simple implementation of staging:
   preprocessing to replace brackets and escapes with
   code combinators.

This code uses the delimcc library of delimited control.

#directory "/home/oleg/Cache/ncaml4/caml-shift";;
#load "delimcc.cma";;
*)

(* Mutation operation that we used for illustration and take
   to be a primitive
*)
let rset : 'a list ref -> 'a -> 'a list = fun r v ->
  let v' = v :: !r in
  r := v'; v'

(* Two implementations of dynamic binding, to be needed later *)
module type DynBind = sig
  type 'a dref                          (* dynamic variable *)
  type denv                             (* representation of the current env *)
  val dnew: unit -> 'a dref
  val dref: 'a dref -> 'a
  val dlet: denv -> 'a dref -> 'a -> (unit -> 'w) -> 'w
  val denv_get: unit -> denv            (* capture the current env *)
end

module DynBindRef : DynBind = struct
  type 'a dref = 'a option ref
  type slot = {setvar:   unit -> unit;
               resetvar: unit -> unit}
  type denv = slot list

  let denv = ref []                         (* initial environment *)
  let rewind () =
    List.iter (function {resetvar;_} -> resetvar ()) !denv;
    denv := []
  let denv_get () = !denv
  let wind : denv -> unit = fun d ->
    List.iter (function {setvar;_} -> setvar ()) (List.rev d);
    denv := d

  let dnew () = ref None
  let dref r = match !r with
  | Some x -> x
  |      _ -> failwith "Unbound dynvar"

  (* Hugely inefficient! But it is easy to check... *)
  let dlet d r x body =
    let dold = !denv in
    rewind ();
    let slot = {setvar   = (fun () -> r := Some x);
                resetvar = (fun () -> r := None)} in
    wind (slot::d);
    match body () with
    | exception e -> rewind (); wind dold; raise e
    | w           -> rewind (); wind dold; w
end

(*
module DynBindCC : DynBind = struct
  open Delimcc
  type 'a dref = ('a->unit) prompt
  let dnew = new_prompt
  let dlet p x body =
    let pexit = new_prompt () in
    push_prompt pexit (fun () ->
      push_prompt p (fun () -> abort pexit @@ body ()) x;
      failwith "unreachable")
  let dref p = shift0 p (fun k -> fun v -> k v v)
end
*)


(* Code combinators *)

module type Code = sig
  type +'a cod

  val int:  int -> int cod
  val str:  string -> string cod
  val add:  int cod -> int cod -> int cod
  val lam:  ('a cod -> 'b cod) -> ('a->'b) cod
  val app:  ('a->'b) cod -> ('a cod -> 'b cod)

  val pair: 'a cod -> 'b cod -> ('a * 'b) cod
  val nil:  'a list cod
  val cons: 'a cod -> 'a list cod -> 'a list cod

  val ref_:  'a cod -> 'a ref cod
  val rget:  'a ref cod -> 'a cod
  val rset:  'a list ref cod -> 'a cod -> 'a list cod

  val csp:   'a -> 'a cod (* CSP of local values *)
end

(*
      fun x -> .<fun y -> (y + 1) :: .~x>.;;
*)

module EX(C:Code) = struct
  open C
  let f = fun x -> lam (fun y -> cons (add y (int 1)) x)
  let res = f (cons (int 2) nil)
      (* testing generating code for polymorhic values and functions *)
  let polyv = cons nil nil
  (* The following does not generalize. However, if we used GADTs for
     representing the object languages, it would have:
       let polyf = Lam("f",fun x -> x)
     It doesn't help the main problem of let-insertion though...
   *)
  let polyf () = lam (fun x -> x)
end

(*
*)

(* A simple implementation of Code *)

module CodeString = struct
  type 'a cod = string

  let paren x = "(" ^ x ^ ")"
  let gensym =
     let r = ref 0 in
     fun name -> let v = name ^ string_of_int !r in
                 incr r; v

  let int   = string_of_int
  let str x = "\"" ^ String.escaped x ^ "\""
  let add x y = paren @@ x ^ " + " ^ y
  let lam body =
    let var = gensym "x" in
    "fun " ^ var ^ " -> " ^ body var
  let app f x = paren @@ f ^ " " ^ x

  let pair x y = paren @@ x ^ ", " ^ y
  let nil = "[]"
  let cons h t =
    if t.[0] = '[' then "[" ^ h ^ "; " ^ String.sub t 1 (String.length t-1)
    else paren @@ h ^ "::" ^ t

  let ref_ x = paren @@ "ref " ^ x
  let rget r = "!" ^ r
  let rset r x = paren @@ "rset " ^ r ^ " " ^ x

  (* Copying semantics, as behoves to serializing *)
  let csp x =
    let xs = Marshal.to_string x [Marshal.Closures] in
    Printf.sprintf "(Marshal.from_string \"%s\" 0)" (String.escaped xs)
end
;;

let module M = EX(CodeString) in M.res

(* Another implementation of Code, which actually proves type safety *)
module CodeReal = struct
  type 'a cod = unit -> 'a
  open DynBindRef

  let int x = fun () -> x
  let str x = fun () -> x
  let add x y = fun () -> x () + y ()
  let lam body =
    let r = dnew () in
    let b = body (fun () -> dref r) in
    fun () ->
      let denv = denv_get () in
      fun x -> dlet denv r x b
  let app f x = fun () -> f () (x ())

  let pair x y = fun () -> (x (), y ())
  let nil = fun () -> []
  let cons h t = fun () -> h () :: t ()

  let ref_ x = fun () -> ref (x ())
  let rget x = fun () -> !(x ())
  let rset r x = fun () -> rset (r ()) (x ())

  (* Sharing semantics of MetaOCaml *)
  let csp x = fun () -> x
end
;;

let module MReal = EX(CodeReal) in MReal.res () 10
(* - : int list = [11; 2] *)
;;

(* An example to test the translation on higher-order functions
   and CSP.
   Our language has no conditionals or recursion. That can be brought
   in through CSP.
*)
module ExFact(C:Code) = struct
  open C

  let rec fix f x = f (fix f) x
  let ifz n th el = if n = 0 then th () else el ()

  let factnr = lam (fun self -> lam (fun n ->
    app
      (app (app (csp ifz) n)
       (lam (fun _ -> int 1)))
         (lam (fun _ -> app (app (csp ( * )) n) @@
                                app self (add (int (-1)) n)))))

  let fact = app (csp fix) factnr
end
;;

let module M = ExFact(CodeReal) in M.fact () 5

(* - : int = 120 *)
;;

(* For the implementation in terms of code values of MetaOCaml,
   see CodeCode at the end of the file
*)


(* ------------------------------------------------------------------------
  Handling polymorphic let
*)

module type CodeLet = sig
  include Code
  type 'a scope
  val new_scope : ('w scope -> 'w cod) -> 'w cod
  val genlet : 'w scope -> 'a cod -> 'a cod
end

(* Translating

   .<let x = [] in (2::x,"3"::x)>.

The key part is that the future-stage let is translated into a
present-stage let
*)

module EXPoly1(C:CodeLet) = struct
  open C
  let res =
   new_scope @@ fun p ->
   let x = genlet p nil in
   pair (cons (int 2) x)
        (cons (str "3") x)
end
(*
module EXPoly1 :
  functor (C : CodeLet) -> sig val res : (int list * string list) C.cod end
*)

(*
  The code that should not type-check
  .<let x = ref [] in (rset x 2, rset x "3")>.

  is translated to
*)

(*
module EXPolyBad(C:CodeLet) = struct
  open C
  let res =
    new_scope @@ fun p ->
      let x = genlet p (ref_ nil) in
      pair (rset x (int 2))
           (rset x (str "3"))
end
*)
(* Does not type-check indeed *)

(* In principle, new_scope can be much `higher' in the expression tree
   than genlet. In practice, it immediately precedes genlet. That is, the
   usual pattern is
    new_scope (fun p ->
     let x = genlet p e in body)
The overall idea is to, informally, insert "let x_future = e" over
     let x = genlet p e ...
*)
module CodeLetString = struct
  include CodeString
  (* Actually, with text strings we don't even need to bother with
     continuations. Still, it's general.
   *)
  open Delimcc
  type 'a scope = 'a cod prompt
  let new_scope body =
   let p = new_prompt () in
   push_prompt p (fun () -> body p)
  let genlet p e =
   let tvar = gensym "t" in
   shift0 p (fun k -> "let " ^ tvar ^ " = " ^ e ^ " in " ^ k tvar)
end
;;

let module M = EXPoly1(CodeLetString) in M.res
(*
- : (int list * string list) CodeLetString.cod =
"let t0 = [] in ((2::t0), (\"3\"::t0))"

Type-check it!
let t0 = [] in ((2::t0), ("3"::t0))

### - : int list * string list = ([2], ["3"])

*)

module CodeLetReal = struct
  include CodeReal
  open Delimcc
  type 'a scope = 'a cod prompt
  let new_scope body =
   let p = new_prompt () in
   push_prompt p (fun () -> body p)
  let genlet p e =
   shift0 p (fun k -> let t = e () in k (fun () -> t))
end
;;
let module M = EXPoly1(CodeLetReal) in M.res ()

(* Alas, this approach doesn't work for the most interesting case:
  Translating
   .<let f = fun x -> x in (f 2, f "3")>.

module EXPoly20(C:CodeLet) = struct
  open C
  let res =
   new_scope (fun p ->
   let f = genlet p (lam (fun x -> x)) in
   pair (app f (int 1))
        (app f (str "3"))
   )
end

          (app f (str "3"))
                 ^^^^^^^^^
Error: This expression has type string C.cod
       but an expression was expected of type int C.cod
       Type string is not compatible with type int

The reason is not genlet, it is lam!
let f = lam (fun x -> x)
does not generalize because the type variable 'a is not covariant.

Meditate on the following:

let id = fun x -> x
let e1 = [id]
let e2 = id::[]
let cons h t = h :: t
let e3 = cons id []
let e4 = cons [] []
let e5 = cons [id] []

type +'a fn = Fn of (int -> 'a)
type +'a fn = Fn of ('a -> 'a)  (* error! *)

*)

let nogenf () =
   let r = ref None in
   fun x -> match !r with
            | None -> r := Some x; x
            | Some x -> x
;;
(*
val nogenf : '_a -> '_a = <fun>
*)


(* Suppose we do permit the generalization of ('a->'a) cod espressions,
   with teh following hack
*)
module type CodeW = sig
  include Code
  type (+'a,+'b) arr                    (* caution! *)

  val lam:  ('a cod -> 'b cod) -> ('a,'b) arr cod
  val app:  ('a,'b) arr cod -> ('a cod -> 'b cod)

  type 'a scope
  val new_scope : ('w scope -> 'w cod) -> 'w cod
  val genlet : 'w scope -> 'a cod -> 'a cod
end

(* Then our example with the polymorphic identity does type-check *)

(* Translating
   .<let f = fun x -> x in (f 2, f "3")>.
*)

module EXPoly21(C:CodeW) = struct
  open C
  let res =
   new_scope (fun p ->
   let f = genlet p (lam (fun x -> x)) in
   pair (app f (int 1))
        (app f (str "3"))
   )
end


(*
module EXPoly21 :
  functor (C : CodeW) -> sig val res : (int * string) C.cod end
*)


module CodeWString = struct
  include CodeLetString
  type (+'a,+'b) arr                    (* caution! *)
end
;;
let module M = EXPoly21(CodeWString) in M.res;;
(*
- : (int * string) CodeWString.cod =
"let t2 = fun x1 -> x1 in ((t2 1), (t2 \"3\"))"

Check it type checks

let t2 = fun x1 -> x1 in ((t2 1), (t2 "3"))

### - : int * string = (1, "3")

*)

(* Yet something else also type-checks:
   This approach effectively permitting the unrestricted
   generalization of ('a -> 'a) cod is unsound.


let f = let r = ref [] in
        fun x -> rset r x
  in (f 1, f "3");;

*)
module EXGenUnsound(C:CodeW) = struct
  open C
  let res =
    new_scope @@ fun p1 ->
      let f = genlet p1
        (new_scope @@ fun p2 ->
           let r = genlet p2 (ref_ nil) in
           lam (fun x -> rset r x)) in
      pair (app f (int 1)) (app f (str "3"))
end
;;
(* The generator type-checks and generates code. Bad code. *)
let module M = EXGenUnsound(CodeWString) in M.res;;

(*
- : (int list * string list) CodeWString.cod =
"let t5 = let t3 = (ref []) in fun x4 -> (rset t3 x4) in ((t5 1), (t5 \"3\"))"

It does not type-check! We failed to generate valid code!

let t5 = let t3 = (ref []) in fun x4 -> (rset t3 x4) in ((t5 1), (t5 "3"));;

*)


(*
So, we need sort of a value restriction.
*)

(* First, consider what is wrong with the naive translation
*)

module EXPoly21Naive(C:Code) = struct
  open C
  let res =
    let f = fun () -> (lam (fun x -> x)) in
    pair (app (f ()) (int 1))
      (app (f ()) (str "3"))
end

module M = EXPoly21Naive(CodeWString)

let _ =
  Printf.printf "%s\n" M.res

module EXPoly21Naive1(C:CodeW) = struct
  open C
  let res =
   new_scope @@ fun p ->
   let f = fun () -> genlet p (lam (fun x -> x)) in
   pair (app (f ()) (int 1))
        (app (f ()) (str "3"))
end

module M2 = EXPoly21Naive1(CodeWString)

let _ =
  Printf.printf "%s\n" M2.res;;

module type CodeFunLet = sig
  include CodeLet
  type 'w funscope
  val new_funscope : ('w funscope -> 'w cod) -> 'w cod
  val genletfun: 'w funscope -> ('a cod -> 'b cod) -> ('a->'b) cod
end


(*
  Translating
   .<let f = fun x -> x in (f 1, f (2,3))>.
*)

module EXPoly2(C:CodeFunLet) = struct
  open C
  let res =
   new_funscope @@ fun p ->
   let f = fun () -> genletfun p (fun x -> x) in
   pair (app (f ()) (int 1))
        (app (f ()) (str "3"))
end

(* Implementing CodeFunLet for CodeReal requires magic *)

module AddGenFun(C:CodeLet) = struct
  open C
  type afun = | AFun : ('a -> 'b) cod -> afun  (* existential: safer than *)
              | ANone : afun                   (* magic everywhere        *)
  type 'w funscope = 'w scope * afun ref
  let new_funscope body = new_scope (fun p -> body (p, ref ANone))
  let genletfun : 'w funscope -> ('a cod -> 'b cod) -> ('a->'b) cod =
    fun (p,r) body ->
    match !r with
    | ANone -> let fn = lam body in
               let x = genlet p fn in
               r := AFun x; x
    | AFun x -> Obj.magic x
end

(*
module EXPoly2 :
  functor (C : CodeFunLet) -> sig val res : (int * string) C.cod end
*)

module CodeFunLetString = struct
  include CodeLetString
  include AddGenFun(CodeLetString)
end

module M3 = EXPoly2(CodeFunLetString)
(*
- : (int * string) CodeFunLetString.cod =
"let t9 = fun x8 -> x8 in ((t9 1), (t9 \"3\"))"
*)
let _ =
  Printf.printf "%s\n" M3.res;;


module CodeFunLetReal = struct
  include CodeLetReal
  include AddGenFun(CodeLetReal)
end
;;

let module M = EXPoly2(CodeFunLetReal) in M.res ();;

(*
- : int * string = (1, "3")
*)

(* Translating an example where the result type of a function does not
   have covariant variables
.<let f = fun () -> ref [] in
     (rset (f ()) 2, rset (f ()) "3")
>.;;
*)
module EXPolyRef(C:CodeFunLet) = struct
  open C
  let res =
   new_funscope @@ fun p ->
    let f = fun () -> genletfun p (fun _ -> ref_ nil) in
    pair (rset (app (f ()) (csp ())) (int 1))
         (rset (app (f ()) (csp ())) (str "3"))
end
;;
(*
module EXPolyRef :
  functor (C : CodeFunLet) ->
    sig val res : (int list * string list) C.cod end
*)

let module M = EXPolyRef(CodeFunLetString) in M.res;;

let module M = EXPolyRef(CodeFunLetReal) in M.res ();;

(* Translating the problematic example that is unsound in MetaOCaml

.<let f = fun () -> .~(lift (ref [])) in
     (rset (f ()) 2, rset (f ()) "3")
>.
*)

module EXPolyRefBad(C:CodeFunLet) = struct
  open C
  let res =
   new_funscope @@ fun p ->
    let f = fun () -> genletfun p (fun _ -> csp (ref [])) in
    pair (rset (app (f ()) (csp ())) (int 1))
         (rset (app (f ()) (csp ())) (str "3"))
end
;;
(*  It also type-checks...

module EXPolyRefBad :
  functor (C : CodeFunLet) ->
    sig val res : (int list * string list) C.cod end
*)

let module M = EXPolyRefBad(CodeFunLetString) in M.res;;


let module M = EXPolyRefBad(CodeFunLetReal) in M.res ;;

(* And this is bad...
let module M = EXPolyRefBad(CodeFunLetReal) in M.res () ;;
*)


(* Trying to give another implementation for Code: into MetaOCaml *)
(* The above portion of the file should work on vanilla OCaml. The following
   requires MetaOCaml.
*)

module CodeCode = struct
  type 'a cod = 'a code

  let int (x:int)    = .<x>.
  let str (x:string) = .<x>.
  let add x y = .<.~x + .~y>.
  let lam body = .<fun x -> .~(body .<x>.)>.
  let app x y = .<.~x  .~y>.

  let pair x y = .<(.~x, .~y)>.
  let nil = .<[]>.
  let cons h t = .<.~h :: .~t>.

  let ref_ x = .<ref .~x>.
  let rget x = .<! (.~x)>.
  let rset r x = .<rset .~r .~x>.

  (* Sharing semantics of MetaOCaml *)
  let csp x = .<x>.
end
;;
let module M = EX(CodeCode) in M.res;;
(*
- : (int -> int list) CodeMeta.cod = .<fun x_1  -> [x_1 + 1; 2]>.
*)
let module M = EX(CodeCode) in Runnative.run M.res 10;;

module CodeLetCode = struct
  include CodeCode
  open Delimcc
  type 'a scope = 'a code prompt
  let new_scope body =
   let p = new_prompt () in
   push_prompt p (fun () -> body p)
  let genlet p e =
   shift0 p (fun k -> .<let t = .~e in .~(k .<t>.)>.)
end
;;

let module M = EXPoly1(CodeLetCode) in M.res;;
(*
- : (int list * string list) CodeLetCode.cod = .<
let t_5 = [] in ((2 :: t_5), ("3" :: t_5))>.
*)

module CodeFunLetCode = struct
  include CodeLetCode
  include AddGenFun(CodeLetCode)
end
;;

let module M = EXPoly2(CodeFunLetCode) in M.res;;

(*
- : (int * string) CodeFunLetCode.cod = .<
let t_4 x_3 = x_3 in ((t_4 1), (t_4 "3"))>.
*)

let module M = EXPolyRef(CodeFunLetCode) in M.res;;

let module M = EXPolyRefBad(CodeFunLetCode) in M.res;;
