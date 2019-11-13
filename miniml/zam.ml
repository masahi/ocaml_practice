type zam_instr =
  | ZAM_Ldi of int
  | ZAM_Ldb of bool
  | ZAM_Ldl
  | ZAM_Access of int
  | ZAM_Closure of zam_code
  | ZAM_Let
  | ZAM_EndLet
  | ZAM_Test of zam_code * zam_code
  | ZAM_Binop of binop
  | ZAM_Apply
  | ZAM_TailApply
  | ZAM_PushMark
  | ZAM_Grab
  | ZAM_Return
  | ZAM_Cons
  | ZAM_Head
  | ZAM_Tail
and zam_code = zam_instr list
and binop =
  | ZAM_Add
  | ZAM_Sub
  | ZAM_Mul
  | ZAM_Eq

type zam_value =
  | ZAM_IntVal  of int
  | ZAM_BoolVal of bool
  | ZAM_ClosVal of zam_code * zam_env
  | ZAM_ListVal of zam_value list
  | ZAM_Epsilon
and zam_stack = zam_value list
and zam_env = zam_value list


let rec string_of_zam_instr indent instr =
  let indent_str = String.init indent (fun _ -> ' ') in
  let sep = "\n" in
  let string_of_binop = function
  | ZAM_Add -> "Add"
  | ZAM_Sub -> "Sub"
  | ZAM_Mul -> "Mul"
  | ZAM_Eq -> "Eq"
  in
  let str =
    match instr with
    | ZAM_Ldi(i) -> Printf.sprintf "Ldi %d" i
    | ZAM_Ldb(b) -> Printf.sprintf "Ldb %b" b
    | ZAM_Access(i) -> Printf.sprintf "Access %d" i
    | ZAM_Closure(code) ->
      let inner = List.map (fun instr -> string_of_zam_instr (indent + 1) instr) code |> String.concat sep in
      Printf.sprintf "%sClosure begin\n%s\n%sClosure end" indent_str inner indent_str
    | ZAM_Let -> "Let"
    | ZAM_EndLet -> "EndLet"
    | ZAM_Test(then_b, else_b) ->
      let then_inner = List.map (fun instr -> string_of_zam_instr (indent + 1) instr) then_b |> String.concat sep in
      let else_inner = List.map (fun instr -> string_of_zam_instr (indent + 1) instr) else_b |> String.concat sep in
      Printf.sprintf "Test then\n%s\n%sTest else\n%s\n%sTest end" then_inner indent_str else_inner indent_str
    | ZAM_Binop(op) -> Printf.sprintf "Binop %s" (string_of_binop op)
    | ZAM_Apply -> "Apply"
    | ZAM_TailApply -> "TailApply"
    | ZAM_PushMark -> "PushMark"
    | ZAM_Grab -> "Grab"
    | ZAM_Return -> "Return"
    | ZAM_Ldl -> "List empty"
    | ZAM_Cons -> "List cons"
    | ZAM_Head -> "List head"
    | ZAM_Tail -> "List tail"
  in
  String.concat "" [indent_str; str]


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
  let eval_binop op ev arg_s ret_s next_instrs =
    let apply_op v1 v2 =
      match op with
      | ZAM_Add -> ZAM_IntVal(v1 + v2)
      | ZAM_Sub -> ZAM_IntVal(v1 - v2)
      | ZAM_Mul -> ZAM_IntVal(v1 * v2)
      | ZAM_Eq -> ZAM_BoolVal(v1 = v2)
    in
    match arg_s with
    | ZAM_IntVal(v1)::ZAM_IntVal(v2)::tl -> eval next_instrs ev (apply_op v1 v2 :: tl) ret_s
    | ZAM_ListVal(lst1)::ZAM_ListVal(lst2)::tl -> eval next_instrs ev (ZAM_BoolVal(lst1 = lst2) :: tl) ret_s
    | _ -> assert false
  in
  match c, arg_stack, ret_stack with
  | [], [v], _ -> v
  | ZAM_Ldi(n)::xs, _, _ -> eval xs env (ZAM_IntVal(n)::arg_stack) ret_stack
  | ZAM_Ldb(b)::xs, _, _ -> eval xs env (ZAM_BoolVal(b)::arg_stack) ret_stack
  | ZAM_Ldl::xs, _, _ -> eval xs env (ZAM_ListVal([])::arg_stack) ret_stack
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
  | ZAM_Binop(op)::xs, _, _ -> eval_binop op env arg_stack ret_stack xs
  | ZAM_Apply::xs, ZAM_ClosVal(code, clo_env)::v::tl, _ ->
    eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl (ZAM_ClosVal(xs, env)::ret_stack)
  | ZAM_TailApply::_, ZAM_ClosVal(code, clo_env)::v::tl, _ ->
    eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl ret_stack
  | ZAM_PushMark::xs, _, _ -> eval xs env (ZAM_Epsilon::arg_stack) ret_stack
  | ZAM_Grab::xs, ZAM_Epsilon::tl, ZAM_ClosVal(code, clo_env)::r -> eval code clo_env (ZAM_ClosVal(xs, env)::tl) r
  | ZAM_Grab::xs, v::tl, _ -> eval xs (v::ZAM_ClosVal(xs, env)::env) tl ret_stack
  | ZAM_Return::_, v::ZAM_Epsilon::tl, ZAM_ClosVal(code, clo_env)::r -> eval code clo_env (v::tl) r
  | ZAM_Return::_, ZAM_ClosVal(code, clo_env)::v::tl, _ -> eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl ret_stack
  | ZAM_Cons::xs, ZAM_ListVal(lst)::zam_val::tl, _ -> eval xs env (ZAM_ListVal(zam_val::lst) :: tl) ret_stack
  | ZAM_Head::xs, ZAM_ListVal(hd::_)::s, _ -> eval xs env (hd::s) ret_stack
  | ZAM_Tail::xs, ZAM_ListVal(_::tl)::s, _ -> eval xs env (ZAM_ListVal(tl)::s) ret_stack
  | _ -> assert false

let compile e env =
  let open Syntax in
  let rec compile_C e env =
    match e with
    | IntLit(_) | BoolLit(_) | Var(_) | Binop(_) |
      Empty | Cons(_) | Head(_) | Tail(_) | Match(_)
      -> compile_generic e env compile_C
    | Let(_) -> compile_generic e env compile_C @ [ZAM_EndLet]
    | If(_) -> compile_generic e env compile_C
    | Fun(x, e) -> [ZAM_Closure(compile_T e (x::x::env))]
    | LetRec(f, x, e1, e2) ->
      [ZAM_Closure(compile_T e1 (x::f::env))] @ [ZAM_Let] @ compile_C e2 (f::env) @ [ZAM_EndLet]
    | App(_) -> [ZAM_PushMark] @ compile_app_inner e env @ [ZAM_Apply]
  and compile_T e env =
    match e with
    | IntLit(_) | BoolLit(_) | Var(_) | Binop(_) |
      Empty | Cons(_) | Head(_) | Tail(_) | Match(_)
      -> (compile_generic e env compile_T) @ [ZAM_Return]
    | Let(_) -> compile_generic e env compile_T
    | If(_) -> compile_generic e env compile_T
    | Fun(x, e) -> [ZAM_Grab] @ compile_T e (x::x::env)
    | LetRec(f, x, e1, e2) ->
      [ZAM_Closure(compile_T e1 (x::f::env))] @ [ZAM_Let] @ compile_T e2 (f::env)
    | App(_) -> compile_app_inner e env @ [ZAM_TailApply]
  and compile_binop op x y env =
    let y_code = compile_C y env in
    let x_code = compile_C x env in
    let zam_op = function
      | Plus -> ZAM_Add
      | Minus -> ZAM_Sub
      | Times -> ZAM_Mul
      | Eq -> ZAM_Eq
    in
    y_code @ x_code @ [ZAM_Binop(zam_op op)]
  and compile_generic e env compile_fun =
    match e with
    | IntLit(n) -> [ZAM_Ldi(n)]
    | BoolLit(b) -> [ZAM_Ldb(b)]
    | Var(x) -> [ZAM_Access(position x env)]
    | Binop(op, x, y) -> compile_binop op x y env
    | If(b, x, y) -> compile_C b env @ [ZAM_Test(compile_fun x env, compile_fun y env)]
    | Let(x, e1, e2) -> (compile_C e1 env) @ [ZAM_Let] @ compile_fun e2 (x::env)
    | Empty -> [ZAM_Ldl]
    | Cons(hd, tl) -> compile_fun hd env @ compile_fun tl env @ [ZAM_Cons]
    | Head(lst) -> compile_fun lst env @ [ZAM_Head]
    | Tail(lst) -> compile_fun lst env @ [ZAM_Tail]
    | Match(_, _) -> failwith "match found"
    | _ -> failwith "not implemented"
  and compile_app_inner e env =
    match e with
    | App(func, e1) -> compile_C e1 env @ compile_app_inner func env
    | _ -> compile_C e env
  in
  compile_C e env

let compile e = compile e []

let rec convert_value cam_val =
  let open Syntax in
  match cam_val with
  | ZAM_IntVal(n) -> IntVal(n)
  | ZAM_BoolVal(b) -> BoolVal(b)
  | ZAM_ListVal(lst) -> ListVal(List.map convert_value lst)
  | ZAM_ClosVal(_) -> FunVal("cam fun", Empty, [])
  | _ -> assert false

let eval instrs =
  Printf.printf "ZAM instructions:\n";
  List.iter (fun inst -> Printf.printf "%s\n" (string_of_zam_instr 0 inst)) instrs;
  eval instrs [] [] [] |> convert_value
