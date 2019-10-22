type cam_instr =
  | CAM_Ldi of int
  | CAM_Ldb of bool
  | CAM_Ldl
  | CAM_Access of int
  | CAM_Closure of cam_code
  | CAM_Apply
  | CAM_Return
  | CAM_Let
  | CAM_EndLet
  | CAM_Test of cam_code * cam_code
  | CAM_Binop of binop
  | CAM_Cons
  | CAM_Head
  | CAM_Tail
and cam_code = cam_instr list
and binop =
  | CAM_Add
  | CAM_Sub
  | CAM_Mul
  | CAM_Eq

type cam_value =
  | CAM_IntVal  of int
  | CAM_BoolVal of bool
  | CAM_ListVal of cam_value list
  | CAM_ClosVal of cam_code * cam_env
and cam_stack = cam_value list
and cam_env = cam_value list

let rec access_env index env =
  match env with
  | [] -> failwith "env empty"
  | x::xs -> if index = 0 then x
             else access_env (index-1) xs

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec eval c env s =
  let eval_binop op env s next_instrs =
    let apply_op v1 v2 =
      match op with
      | CAM_Add -> CAM_IntVal(v1 + v2)
      | CAM_Sub -> CAM_IntVal(v1 - v2)
      | CAM_Mul -> CAM_IntVal(v1 * v2)
      | CAM_Eq -> CAM_BoolVal(v1 = v2)
    in
    match s with
    | CAM_IntVal(v1)::CAM_IntVal(v2)::tl -> eval next_instrs env (apply_op v1 v2 :: tl)
    | CAM_ListVal(lst1)::CAM_ListVal(lst2)::tl -> eval next_instrs env (CAM_BoolVal(lst1 = lst2) :: tl)
    | _ -> assert false
  in
  match c, s with
  | [], [v] -> v
  | CAM_Ldi(n)::xs, _ -> eval xs env (CAM_IntVal(n)::s)
  | CAM_Ldb(b)::xs, _ -> eval xs env (CAM_BoolVal(b)::s)
  | CAM_Ldl::xs, _ -> eval xs env (CAM_ListVal([])::s)
  | CAM_Access(n)::xs, _ ->
    let value = access_env n env in
    eval xs env (value::s)
  | CAM_Closure(code)::xs, _ -> eval xs env (CAM_ClosVal(code, env)::s)
  | CAM_Apply::xs, CAM_ClosVal(code, clo_env)::v::tl ->
    eval code (v::CAM_ClosVal(code, clo_env)::clo_env) (CAM_ClosVal(xs, env)::tl)
  | CAM_Return::_, v::CAM_ClosVal(code, clo_env)::tl -> eval code clo_env (v::tl)
  | CAM_Let::xs, v::tl -> eval xs (v::env) tl
  | CAM_EndLet::xs, _::tl -> eval xs tl s
  | CAM_Test(c1, _)::xs, CAM_BoolVal(true)::tl -> eval (c1 @ xs) env tl
  | CAM_Test(_, c2)::xs, CAM_BoolVal(false)::tl -> eval (c2 @ xs) env tl
  | CAM_Binop(op)::xs, _ -> eval_binop op env s xs
  | CAM_Cons::xs, CAM_ListVal(lst)::cam_val::tl -> eval xs env (CAM_ListVal(cam_val::lst) :: tl)
  | CAM_Head::xs, CAM_ListVal(hd::_)::s -> eval xs env (hd::s)
  | CAM_Tail::xs, CAM_ListVal(_::tl)::s -> eval xs env (CAM_ListVal(tl)::s)
  | _ -> assert false

let rec compile e env =
  let open Syntax in
  let compile_binop op x y =
    let cam_op = function
      | Plus -> CAM_Add
      | Minus -> CAM_Sub
      | Times -> CAM_Mul
      | Eq -> CAM_Eq
    in
    compile y env @ compile x env @ [CAM_Binop(cam_op op)]
  in
  match e with
  | IntLit(n) -> [CAM_Ldi(n)]
  | BoolLit(b) -> [CAM_Ldb(b)]
  | Binop(op, x, y) -> compile_binop op x y
  | If(b, x, y) -> compile b env @ [CAM_Test(compile x env, compile y env)]
  | Let(x, e1, e2) -> (compile e1 env) @ [CAM_Let] @ compile e2 (x::env) @ [CAM_EndLet]
  | Var(x) -> [CAM_Access(position x env)]
  | Fun(x, e) -> [CAM_Closure(compile e (x::x::env) @ [CAM_Return])]
  | App(e1, e2) -> compile e2 env @ compile e1 env @ [CAM_Apply]
  | LetRec(f, x, e1, e2) ->
    [CAM_Closure(compile e1 (x::f::env) @ [CAM_Return])] @ [CAM_Let] @ compile e2 (f::env) @ [CAM_EndLet]
  | Empty -> [CAM_Ldl]
  | Cons(hd, tl) -> compile hd env @ compile tl env @ [CAM_Cons]
  | Head(lst) -> compile lst env @ [CAM_Head]
  | Tail(lst) -> compile lst env @ [CAM_Tail]
  | Match(_, _) -> failwith "match found"

let rec convert_value cam_val =
  let open Syntax in
  match cam_val with
  | CAM_IntVal(n) -> IntVal(n)
  | CAM_BoolVal(b) -> BoolVal(b)
  | CAM_ListVal(lst) -> ListVal(List.map convert_value lst)
  | _ -> assert false

let compile e = compile e []

let eval instrs =
  eval instrs [] [] |> convert_value
