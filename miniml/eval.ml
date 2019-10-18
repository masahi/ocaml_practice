open Syntax

let emptyenv () = []

let ext env x v = (x,v) :: env

let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable: " ^ x)
  | (y,v)::tl -> if x=y then v
    else lookup x tl

let rec eval e env =
  let binop f e1 e2 env =
    match (eval e1 env, eval e2 env) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in
  match e with
  | Var(x)       -> lookup x env
  | IntLit(n)    -> IntVal(n)
  | Plus(e1,e2)  -> binop (+) e1 e2 env
  | Minus(e1,e2)  -> binop (-) e1 e2 env
  | Eq(e1,e2) ->
      begin
	match (eval e1 env, eval e2 env) with
	  | (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
	  | (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
	  | _ -> failwith "wrong value"
      end
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | If(e1,e2,e3) ->
    begin
      match (eval e1 env) with
      | BoolVal(true)  -> eval e2 env
      | BoolVal(false) -> eval e3 env
      | _ -> failwith "wrong value"
    end
  | Let(x,e1,e2) ->
    let env1 = ext env x (eval e1 env)
    in eval e2 env1
  | Fun(x,e1) -> FunVal(x, e1, env)
  | App(e1,e2) ->
    let funpart = (eval e1 env) in
    let arg = (eval e2 env) in
    begin
      match funpart with
      | FunVal(x,body,env1) ->
        let env2 = (ext env1 x arg) in
        eval body env2
      | RecFunVal(f,x,body,env1) ->
        let env2 = (ext (ext env1 x arg) f funpart) in
        eval body env2
      | _ -> failwith "wrong value in App"
    end
  | LetRec(f,x,e1,e2) ->
    let env1 = ext env f (RecFunVal (f, x, e1, env))
    in eval e2 env1
  | _ -> failwith "unknown expression"
