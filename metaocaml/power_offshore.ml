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

let string_of_vname vname = (vname : varname :> string)

let rec pprint_exp = function
  | Const (Const_float x) -> x
  | LocalVar (x,_) -> string_of_vname x
  | Let({id; ty; bind; body; _}) ->
    let rhs = pprint_exp bind in
    let body = pprint_exp body in
    Printf.sprintf "let %s:%s = %s in \n %s" (string_of_vname id) (string_of_typ ty) rhs body
  | FunCall (name,args) ->
    let args_str = List.map pprint_exp args in
    Printf.sprintf "app(%s, %s)" (string_of_vname name) (String.concat ", " args_str)
  | _ ->  failwith "not yet implemented"

let pprint_fun args typ exp =
  print_string "Fun(";
  List.iter (fun (vname, ty) -> Printf.printf "%s: %s" (string_of_vname vname) (string_of_typ ty)) args;
  let body = pprint_exp exp in
  Printf.printf "){\n %s\n}\n" body

let pprint_proc args cmd =
  Printf.printf "Proc\n"

let pprint_offshore_ir = function
 | Fun(args, typ, exp) -> pprint_fun args typ exp
 | Proc(args, cmd) -> pprint_proc args cmd

open Base

let llcontext = Llvm.global_context ()
let llmodule = Llvm.create_module llcontext "power jit"
let builder = Llvm.builder llcontext
let double_type = Llvm.double_type llcontext
let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String)

let create_entry_block_alloca the_function var_name =
  let builder =
    Llvm.builder_at llcontext (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca double_type var_name builder

let rec gen_expr the_function = function
  | Const (Const_float x) ->
    Llvm.const_float double_type (Float.of_string x)
  | LocalVar (name,_) ->
    begin match Base.Hashtbl.find named_values (string_of_vname name) with
      | None -> failwith "Variable not found"
      | Some(v) -> Llvm.build_load v (string_of_vname name) builder
    end
  | Let({id; ty; bind; body; _}) ->
    let rhs = gen_expr the_function bind in
    let alloca = create_entry_block_alloca the_function (string_of_vname id) in
    Llvm.build_store rhs alloca builder |> ignore ;
    Hashtbl.add_exn named_values ~key:(string_of_vname id) ~data:alloca;
    gen_expr the_function body
  | FunCall (name,args) ->
    let open Base in
    let args = List.map ~f:(gen_expr the_function) args in
    begin match (string_of_vname name) with
      | "*." ->
        assert(List.length args = 2);
        Llvm.build_fmul (List.nth_exn args 0) (List.nth_exn args 1) "multmp" builder
      | _ -> failwith "not yet implemented"
    end
  | _ ->  failwith "not yet implemented"

let gen_func args body fpm =
  let doubles = Array.create ~len:(List.length args) double_type in
  let ft = Llvm.function_type double_type doubles in
  let the_function = Llvm.declare_function "func" ft llmodule in
  Array.iteri (Llvm.params the_function) ~f:(fun i a ->
      let (vname, ty) = List.nth_exn args i in
      let name = string_of_vname vname in
      Llvm.set_value_name name a;
      Hashtbl.add_exn named_values ~key:name ~data:a);
  let bb = Llvm.append_block llcontext "entry" the_function in
  Llvm.position_at_end bb builder ;
  Array.iteri (Llvm.params the_function) ~f:(fun i ai ->
      let (vname, ty) = List.nth_exn args i in
      let var_name = string_of_vname vname in
      let alloca = create_entry_block_alloca the_function var_name in
      Llvm.build_store ai alloca builder |> ignore;
      Hashtbl.set named_values ~key:var_name ~data:alloca);
  let return_val = gen_expr the_function body in
  let _  = Llvm.build_ret return_val builder in
  Llvm.dump_value the_function;
  let _  = Llvm.PassManager.run_function the_function fpm in
  match Llvm_analysis.verify_function the_function with
  | true -> the_function
  | false -> assert false

let gen_llvm fpm = function
 | Fun(args, typ, exp) -> gen_func args exp fpm
 | Proc(args, cmd) -> assert false

let _ =
  Llvm_executionengine.initialize () |> ignore;
  let ee = Llvm_executionengine.create llmodule in
  let fpm = Llvm.PassManager.create_function llmodule in
  Llvm_scalar_opts.add_memory_to_register_promotion fpm ;
  Llvm.PassManager.initialize fpm |> ignore;
  let go n x =
    let power_staged = spowern n in
    let func = offshore (module DefaultConv) power_staged in
    pprint_offshore_ir func;
    flush stdout;
    let llval = gen_llvm fpm func in
    Llvm.dump_value llval;
    Llvm_executionengine.add_module llmodule ee;
    let fp =
      Llvm_executionengine.get_function_address "func"
        (Foreign.funptr Ctypes.(double @-> returning double))
        ee
    in
    Stdlib.Printf.printf "Evaluated to %.20f\n" (fp x) ;
    Llvm_executionengine.remove_module llmodule ee
  in
  go 100000 1.00001
