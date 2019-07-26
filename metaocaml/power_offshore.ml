open OffshoringIR

let square x = x *. x

let rec power n x =
  if n = 0 then 1.
  else if n mod 2 = 0 then square (power (n/2) x)
  else x *. (power (n-1) x)

let rec spower n x =
  if n = 0 then .<1.>.
  else if n mod 2 = 0 then
    .<let x' = .~x *. .~x in
    .~(spower (n/2) .<x'>.)>.
  else .<.~x *. .~(spower (n-1) x)>.

let spowern n = .<fun x -> .~(spower n .<x>.)>.

let string_of_typ = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TDouble -> "float"
  | TVariable -> "var"
  | _ -> "unknown"

let pprint_vname vname = (vname : varname :> string)

let rec pprint_exp = function
  | Const (Const_float x) -> x
  | LocalVar (x,_) -> pprint_vname x
  | Let({id; ty; bind; body; _}) ->
    let rhs = pprint_exp bind in
    let body = pprint_exp body in
    Printf.sprintf "let %s:%s = %s in \n %s" (pprint_vname id) (string_of_typ ty) rhs body
  | FunCall (name,args) ->
    let args_str = List.map pprint_exp args in
    Printf.sprintf "app(%s, %s)" (pprint_vname name) (String.concat ", " args_str)
  | _ ->  failwith "not yet implemented"

let pprint_fun args typ exp =
  print_string "Fun(";
  List.iter (fun (vname, ty) -> Printf.printf "%s: %s" (pprint_vname vname) (string_of_typ ty)) args;
  let body = pprint_exp exp in
  Printf.printf "){\n %s\n}\n" body

let pprint_proc args cmd =
  Printf.printf "Proc\n"

let pprint_offshore_ir = function
 | Fun(args, typ, exp) -> pprint_fun args typ exp
 | Proc(args, cmd) -> pprint_proc args cmd

let _ =
  let llcontext = Llvm.global_context () in
  let llmodule = Llvm.create_module llcontext "power jit" in
  ignore(Llvm_executionengine.initialize ());
  let ee = Llvm_executionengine.create llmodule in
  let fpm = Llvm.PassManager.create_function llmodule in
  Llvm_scalar_opts.add_memory_to_register_promotion fpm ;
  let go n _ =
    let power_staged = spowern n in
    let proc = offshore (module DefaultConv) power_staged in
    pprint_offshore_ir proc
  in
  go 100000 1.00001
