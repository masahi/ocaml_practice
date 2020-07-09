open Codelib

type exp = Int of int | Var of string | App of string * exp
         | Add of exp * exp | Sub of exp * exp
         | Mul of exp * exp | Div of exp * exp | Ifz of exp * exp * exp

type def = Declaration of string * string * exp
type prog = Program of def list * exp

let fact = Program([Declaration
                      ("fact", "x", Ifz(Var "x",
                                        Int 1,
                                        Mul(Var "x",
                                            (App ("fact", Sub(Var "x", (Int 1)))))))
                   ],
                   App("fact", Int 10))

exception Yikes

let env0 = fun _ -> raise Yikes

let fenv0 = env0

let ext env x v = fun y -> if x=y then v else env y

let rec eval1 e env fenv =
  match e with
  | Int i -> i
  | Var s -> env s
  | App (s,e2) -> (fenv s) (eval1 e2 env fenv)
  | Add (e1,e2) -> (eval1 e1 env fenv) + (eval1 e2 env fenv)
  | Sub (e1,e2) -> (eval1 e1 env fenv) - (eval1 e2 env fenv)
  | Mul (e1,e2) -> (eval1 e1 env fenv) * (eval1 e2 env fenv)
  | Div (e1,e2) -> (eval1 e1 env fenv) / (eval1 e2 env fenv)
  | Ifz (e1,e2,e3) ->
    if (eval1 e1 env fenv) = 0 then eval1 e2 env fenv
    else eval1 e3 env fenv

let rec peval1 p env fenv =
  match p with
  | Program ([], e) -> eval1 e env fenv
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    let rec f x = eval1 e1 (ext env s2 x) (ext fenv s1 f)
    in peval1 (Program(tl, e)) env (ext fenv s1 f)

let rec eval2 e env fenv =
  match e with
  | Int i -> .<i>.
  | Var s -> env s
  | App (s,e2) -> .<.~(fenv s) .~(eval2 e2 env fenv)>.
  | Add (e1,e2) -> .<.~(eval2 e1 env fenv) + .~(eval2 e2 env fenv)>.
  | Sub (e1,e2) -> .<.~(eval2 e1 env fenv) - .~(eval2 e2 env fenv)>.
  | Mul (e1,e2) -> .<.~(eval2 e1 env fenv) * .~(eval2 e2 env fenv)>.
  | Div (e1,e2) -> .<.~(eval2 e1 env fenv) / .~(eval2 e2 env fenv)>.
  | Ifz (e1,e2,e3) ->
    .<if .~(eval2 e1 env fenv) = 0 then .~(eval2 e2 env fenv)
    else .~(eval2 e3 env fenv)>.

let rec peval2 p env fenv =
  match p with
  | Program ([], e) -> eval2 e env fenv
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    .<let rec f x = .~(eval2 e1 (ext env s2 .<x>.) (ext fenv s1 .<f>.))
    in .~(peval2 (Program(tl, e)) env (ext fenv s1 .<f>.))>.

let _ =
  let ret = peval1 fact env0 fenv0 in
  Printf.printf "%d\n" ret;
  let cde = peval2 fact env0 fenv0 in
  let ret2 = Runnative.run cde in
  Printf.printf "%d\n" ret2;
  let invoke = .<fun () -> .~cde>. in
  let invoke_f = Runnative.run invoke in
  print_code Format.std_formatter invoke; print_newline();
  let open Core_bench in
  [Bench.Test.create ~name:"unstaged" (fun () -> ignore(peval1 fact env0 fenv0));
   Bench.Test.create ~name:"staged1" (fun () -> ignore(invoke_f ()))]
  |> Bench.bench
