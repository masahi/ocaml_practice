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
  | ZAM_TailApply::_, ZAM_ClosVal(code, clo_env)::v::tl, _ ->
    eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl ret_stack
  | ZAM_PushMark::xs, _, _ -> eval xs env (ZAM_Epsilon::arg_stack) ret_stack
  | ZAM_Grab::xs, ZAM_Epsilon::tl, ZAM_ClosVal(code, clo_env)::r -> eval code clo_env (ZAM_ClosVal(xs, env)::tl) r
  | ZAM_Grab::xs, v::tl, _ -> eval xs (v::ZAM_ClosVal(xs, env)::env) tl ret_stack
  | ZAM_Return::_, v::ZAM_Epsilon::tl, ZAM_ClosVal(code, clo_env)::r -> eval code clo_env (v::tl) r
  | ZAM_Return::_, ZAM_ClosVal(code, clo_env)::v::tl, _ -> eval code (v::ZAM_ClosVal(code, clo_env)::clo_env) tl ret_stack
  | _ -> assert false
