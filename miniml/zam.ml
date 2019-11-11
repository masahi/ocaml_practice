type zam_instr =
  | ZAM_Ldi of int
  | ZAM_Ldb of bool
  | ZAM_Access of int
  | ZAM_Closure of zam_code
  | ZAM_Let
  | ZAM_EndLet
  | ZAM_Test of zam_code * zam_code
  | ZAM_Add
  | ZAM_Eq
  | ZAM_Apply
  | ZAM_TailApply
  | ZAM_PushMark
  | ZAM_Grab
  | ZAM_Return
and zam_code = zam_instr list  (* コードは、命令の列である *)

type zam_value =
  | ZAM_IntVal  of int
  | ZAM_BoolVal of bool
  | ZAM_ClosVal of zam_code * zam_env
  | ZAM_Epsilon
and zam_stack = zam_value list
and zam_env = zam_value list

let rec access_env index env =
  match env with
  | [] -> failwith "env empty"
  | x::xs -> if index = 0 then x
             else access_env (index-1) xs

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec eval c env arg_stack ret_stack =
  match c, arg_stack, ret_stack with
  | ZAM_Ldi(n)::xs, _, _ -> eval xs env (ZAM_IntVal(n)::arg_stack) ret_stack
  | ZAM_Ldb(b)::xs, _, _ -> eval xs env (ZAM_BoolVal(b)::arg_stack) ret_stack
  | ZAM_Access(n)::xs, _, _ ->
    let value = access_env n env in
    eval xs env (value::arg_stack) ret_stack
  | ZAM_Closure(code)::xs, _, _ ->
    eval xs env (ZAM_ClosVal(code, env)::arg_stack) ret_stack
  | ZAM_Let::xs, v::xs2, _ -> eval xs (v::env) xs2 ret_stack
  | ZAM_EndLet::xs, _, _ ->
    begin match env with
      | _::env_rest -> eval xs env_rest arg_stack ret_stack
      | _ -> assert false
    end
  | ZAM_Test(c1, _)::xs, ZAM_BoolVal(true)::tl, _ -> eval (c1 @ xs) env tl ret_stack
  | ZAM_Test(_, c2)::xs, ZAM_BoolVal(false)::tl, _ -> eval (c2 @ xs) env tl ret_stack
  | ZAM_Add::xs, ZAM_IntVal(v1)::ZAM_IntVal(v2)::tl, _ -> eval xs env (ZAM_IntVal(v1 + v2) :: tl) ret_stack
  | ZAM_Eq::xs, ZAM_IntVal(v1)::ZAM_IntVal(v2)::tl, _ -> eval xs env (ZAM_BoolVal(v1 = v2) :: tl) ret_stack
  | ZAM_Apply::xs, ZAM_ClosVal(code, clo_env)::v::tl, _ ->
    eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl (ZAM_ClosVal(xs, env)::ret_stack)
  | ZAM_TailApply::_, ZAM_ClosVal
(code, clo_env)::v::tl, _ ->
    eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl ret_stack
  | ZAM_PushMark::xs, _, _ -> eval xs env (ZAM_Epsilon::arg_stack) ret_stack
  | ZAM_Grab::xs, ZAM_Epsilon::tl, ZAM_ClosVal(code, clo_env)::r -> eval code clo_env (ZAM_ClosVal(xs, env)::tl) r
  | ZAM_Grab::xs, v::tl, _ -> eval xs (v::ZAM_ClosVal(xs, env)::env) tl ret_stack
  | ZAM_Return::_, v::ZAM_Epsilon::tl, ZAM_ClosVal(code, clo_env)::r -> eval code clo_env (v::tl) r
  | ZAM_Return::_, ZAM_ClosVal(code, clo_env)::v::tl, _ -> eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl ret_stack
  | _ -> assert false

let rec compile e env =
  let open Syntax in
  let compile_binop op x y =
    let y_code = compile y env in
    let x_code = compile x env in
    match op with
      | Plus -> y_code @ x_code @ [ZAM_Add]
      (* | Minus -> CAM_Sub
       * | Times -> CAM_Mul *)
      | Eq -> y_code @ x_code @ [ZAM_Eq]
      | _ -> failwith "not implemented"
  in
  match e with
  | IntLit(n) -> [ZAM_Ldi(n)]
  | BoolLit(b) -> [ZAM_Ldb(b)]
  | Binop(op, x, y) -> compile_binop op x y
  | If(b, x, y) -> compile b env @ [ZAM_Test(compile x env, compile y env)]
  | Let(x, e1, e2) -> (compile e1 env) @ [ZAM_Let] @ compile e2 (x::env) @ [ZAM_EndLet]
  | Var(x) -> [ZAM_Access(position x env)]
  | Fun(x, e) -> [ZAM_Closure(compile e (x::x::env) @ [ZAM_Return])]
  | LetRec(f, x, e1, e2) ->
    [ZAM_Closure(compile e1 (x::f::env) @ [ZAM_Return])] @ [ZAM_Let] @ compile e2 (f::env) @ [ZAM_EndLet]
  | App(_) -> compile_app e env
  | _ -> failwith "not implemented"
  (* | Empty -> [ZAM_Ldl]
   * | Cons(hd, tl) -> compile hd env @ compile tl env @ [ZAM_Cons]
   * | Head(lst) -> compile lst env @ [ZAM_Head]
   * | Tail(lst) -> compile lst env @ [ZAM_Tail]
   * | Match(_, _) -> failwith "match found" *)
and compile_app e env =
  let open Syntax in
  let rec compile_app_inner e env =
    match e with
    | App(func, e1) -> compile e1 env @ compile_app_inner func env
    | _ -> compile e env
  in
  match e with
  | App(func, e1) ->
    let last_arg = compile e1 env in
    [ZAM_PushMark] @ last_arg @ compile_app_inner func env @ [ZAM_Apply]
  | _ -> assert false

let compile e = compile e []

let eval instrs =
  eval instrs [] [] [] |> fun x -> x
