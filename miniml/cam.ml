(* syntax.ml *)
type cam_instr =
  | CAM_Ldi of int
  | CAM_Ldb of bool
  | CAM_Access of int
  | CAM_Closure of cam_code
  | CAM_Apply
  | CAM_Return
  | CAM_Let
  | CAM_EndLet
  | CAM_Test of cam_code * cam_code
  | CAM_Add
  | CAM_Sub
  | CAM_Mul
  | CAM_Eq
and cam_code = cam_instr list

type cam_value =
  | CAM_IntVal  of int
  | CAM_BoolVal of bool
  | CAM_ClosVal of cam_code * cam_env
and cam_stack = cam_value list
and cam_env = cam_value list

let print_value = function
  | CAM_IntVal(n) -> Printf.printf "%d\n" n
  | CAM_BoolVal(b) -> Printf.printf "%b\n" b
  | CAM_ClosVal(_) -> Printf.printf "closure \n"

let rec access_env index env =
  match env with
  | [] -> failwith "env empty"
  | x::xs -> if index = 0 then x
             else access_env (index-1) xs

let rec eval c env s =
  match c with
  | [] -> (match s with
          | [] -> failwith "stack empty"
          | [v] -> v
           | _ -> failwith "stack contains more than two values")

  | x::xs -> (match x with
              | CAM_Ldi(n) -> eval xs env (CAM_IntVal(n)::s)
              | CAM_Ldb(b) -> eval xs env (CAM_BoolVal(b)::s)
              | CAM_Access(n) ->
                 let value = access_env n env in
                 eval xs env (value::s)
              | CAM_Closure(code) -> eval xs env (CAM_ClosVal(code, env)::s)
              | CAM_Apply -> (match s with
                              | [] -> failwith "Apply: stack empty"
                              | [_] -> failwith "Apply: stack contains only one value"
                              | clo::v::tl -> (match clo with
                                               | CAM_ClosVal(code, clo_env) -> eval code (v::clo::clo_env) (CAM_ClosVal(xs, env)::tl)
                                               | _ -> failwith "closure expected on top of the stack"))
              | CAM_Return -> (match s with
                               | [] -> failwith "stack empty"
                               | [_] -> failwith "stack contains only one value"
                               | v::clo::tl -> (match clo with
                                                | CAM_ClosVal(code, clo_env) -> eval code clo_env (v::tl)
                                                | _ -> failwith "closure expected on top of the stack"))
              | CAM_Let  -> (match s with
                             | [] -> failwith "stack empty"
                             | v::tl -> eval xs (v::env) tl)
              | CAM_EndLet -> (match env with
                               | [] -> failwith "stack empty"
                               | _::tl -> eval xs tl s)
              | CAM_Test(c1, c2) -> (match s with
                                     | [] -> failwith "stack empty"
                                     | b::tl -> (match b with
                                                 | CAM_BoolVal(true) -> eval (c1 @ xs) env tl
                                                 | CAM_BoolVal(false) -> eval (c2 @ xs) env tl
                                                 | _ -> failwith "bool expected"))
              | CAM_Add -> (match s with
                            | [] -> failwith "stack empty"
                            | [_] -> failwith "stack contains only one value"
                            | n1::n2::tl -> (match n1, n2 with
                                             | CAM_IntVal(n1_v), CAM_IntVal(n2_v) -> eval xs env (CAM_IntVal(n1_v+n2_v)::tl)
                                             | _, _ -> failwith "Two int expected"))
              | CAM_Sub -> (match s with
                            | [] -> failwith "stack empty"
                            | [_] -> failwith "stack contains only one value"
                            | n1::n2::tl -> (match n1, n2 with
                                             | CAM_IntVal(n1_v), CAM_IntVal(n2_v) -> eval xs env (CAM_IntVal(n1_v-n2_v)::tl)
                                             | _, _ -> failwith "Two int expected"))
              | CAM_Mul -> (match s with
                            | [] -> failwith "stack empty"
                            | [_] -> failwith "stack contains only one value"
                            | n1::n2::tl -> (match n1, n2 with
                                             | CAM_IntVal(n1_v), CAM_IntVal(n2_v) -> eval xs env (CAM_IntVal(n1_v*n2_v)::tl)
                                             | _, _ -> failwith "Two int expected"))
              | CAM_Eq -> (match s with
                            | [] -> failwith "stack empty"

                            | [_] -> failwith "stack contains only one value"
                            | n1::n2::tl -> (match n1, n2 with
                                             | CAM_IntVal(n1_v), CAM_IntVal(n2_v) -> eval xs env (CAM_BoolVal(n1_v=n2_v)::tl)
                                             | _, _ -> failwith "Two int expected")))

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1

let rec compile e env =
  let open Syntax in
  match e with
  | IntLit(n) -> [CAM_Ldi(n)]
  | BoolLit(b) -> [CAM_Ldb(b)]
  | Plus(x, y) -> compile y env @ compile x env @ [CAM_Add]
  | Minus(x, y) -> compile y env @ compile x env @ [CAM_Sub]
  | Times(x, y) -> compile y env @ compile x env @ [CAM_Mul]
  | Eq(x, y) -> compile y env @ compile x env @ [CAM_Eq]
  | If(b, x, y) -> compile b env @ [CAM_Test((compile x env), (compile y env))]
  | Let(x, e1, e2) -> (compile e1 env) @ [CAM_Let] @ compile e2 (x::env) @ [CAM_EndLet]
  | Var(x) -> [CAM_Access(position x env)]
  | Fun(x, e) -> [CAM_Closure(compile e (x::x::env) @ [CAM_Return])]
  | App(e1, e2) -> compile e2 env @ compile e1 env @ [CAM_Apply]
  | LetRec(f, x, e1, e2) -> [CAM_Closure(compile e1 (x::f::env) @ [CAM_Return])] @ [CAM_Let] @ compile e2 (f::env) @ [CAM_EndLet]
  | _ -> failwith "not supported"
