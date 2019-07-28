open OffshoringIR

let string_of_vname vname = (vname : varname :> string)

module PPrint = struct
  let string_of_typ = function
    | TUnit -> "unit"
    | TInt -> "int"
    | TBool -> "bool"
    | TDouble -> "float"
    | TVariable -> "var"
    | _ -> "unknown"

  let rec string_of_exp = function
    | Const (Const_float x) -> x
    | LocalVar (x,_) -> string_of_vname x
    | Let({id; ty; bind; body; _}) ->
      let rhs = string_of_exp bind in
      let body = string_of_exp body in
      Printf.sprintf "let %s:%s = %s in \n %s" (string_of_vname id) (string_of_typ ty) rhs body
    | FunCall (name,args) ->
      let args_str = List.map string_of_exp args in
      Printf.sprintf "app(%s, %s)" (string_of_vname name) (String.concat ", " args_str)
    | _ ->  failwith "not yet implemented"

  let pprint_fun args exp =
    print_string "Fun(";
    List.iter (fun (vname, ty) -> Printf.printf "%s: %s" (string_of_vname vname) (string_of_typ ty)) args;
    let body = string_of_exp exp in
    Printf.printf "){\n %s\n}\n" body;
    flush stdout

  let pprint_offshore_ir = function
    | Fun(args, _, exp) -> pprint_fun args exp
    | Proc(_) -> assert false
end

module LLVM_gen = struct
  open Base
  let llcontext = Llvm.global_context ()
  let llmodule = Llvm.create_module llcontext "power jit"
  let builder = Llvm.builder llcontext
  let double_type = Llvm.double_type llcontext
  let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String)
  let func_name = "func"

  let create_entry_block_alloca ll_func var_name =
    let builder =
      Llvm.builder_at llcontext (Llvm.instr_begin (Llvm.entry_block ll_func))
    in
    Llvm.build_alloca double_type var_name builder

  let rec gen_expr ll_func = function
    | Const (Const_float x) -> Llvm.const_float double_type (Float.of_string x)
    | LocalVar (name,_) ->
      let vname = string_of_vname name in
      begin match Hashtbl.find named_values vname with
        | None -> failwith "Variable not found"
        | Some(v) -> Llvm.build_load v vname builder
      end
    | Let({id; bind; body; _}) ->
      let rhs = gen_expr ll_func bind in
      let vname = string_of_vname id in
      let alloca = create_entry_block_alloca ll_func vname in
      Llvm.build_store rhs alloca builder |> ignore;
      Hashtbl.add_exn named_values ~key:vname ~data:alloca;
      gen_expr ll_func body
    | FunCall (name,args) ->
      let args = List.map ~f:(gen_expr ll_func) args in
      begin match (string_of_vname name) with
        | "*." ->
          assert(List.length args = 2);
          Llvm.build_fmul (List.nth_exn args 0) (List.nth_exn args 1) "multmp" builder
        | _ -> failwith "not yet implemented"
      end
    | _ ->  failwith "not yet implemented"

  let gen_func args body =
    let doubles = Array.create ~len:(List.length args) double_type in
    let ft = Llvm.function_type double_type doubles in
    let ll_func = Llvm.declare_function func_name ft llmodule in
    let bb = Llvm.append_block llcontext "entry" ll_func in
    Llvm.position_at_end bb builder;
    Array.iteri (Llvm.params ll_func) ~f:(fun i a ->
        let (vname, _) = List.nth_exn args i in
        let name = string_of_vname vname in
        let alloca = create_entry_block_alloca ll_func name in
        Llvm.build_store a alloca builder |> ignore;
        Hashtbl.set named_values ~key:name ~data:alloca);
    let return_val = gen_expr ll_func body in
    let _  = Llvm.build_ret return_val builder in
    Llvm.dump_value ll_func;
    ll_func

  let get func =
    Llvm_executionengine.initialize () |> ignore;
    let fpm = Llvm.PassManager.create_function llmodule in
    Llvm_scalar_opts.add_memory_to_register_promotion fpm;
    Llvm.PassManager.initialize fpm |> ignore;
    let ll_func = match func with
    | Fun(args, _, exp) -> gen_func args exp
    | Proc(_) -> assert false
    in
    let _  = Llvm.PassManager.run_function ll_func fpm in
    match Llvm_analysis.verify_function ll_func with
    | true -> ll_func
    | false -> assert false

  let get_function_pointer () =
    let ee = Llvm_executionengine.create llmodule in
    Llvm_executionengine.add_module llmodule ee;
    Llvm_executionengine.get_function_address func_name
      (Foreign.funptr Ctypes.(double @-> returning double))
      ee

  let emit_asm file =
    Llvm_X86.initialize ();
    let tgt =
      match Llvm_target.Target.by_name "x86-64" with
      | Some(t) -> t
      | None -> failwith "target not found"
    in
    let triple = Llvm_target.Target.default_triple () in
    let machine = Llvm_target.TargetMachine.create tgt ~triple:triple in
    Llvm_target.TargetMachine.emit_to_file llmodule Llvm_target.CodeGenFileType.AssemblyFile file machine

end

let rec spower n x =
  if n = 0 then .<1.>.
  else if n mod 2 = 0 then
    .<let x' = .~x *. .~x in
    .~(spower (n/2) .<x'>.)>.
  else .<.~x *. .~(spower (n-1) x)>.

let spowern n = .<fun x -> .~(spower n .<x>.)>.

let _ =
  let go n x =
    let power_staged = spowern n in
    let func = offshore (module DefaultConv) power_staged in
    PPrint.pprint_offshore_ir func;
    let llval = LLVM_gen.get func in
    Llvm.dump_value llval;
    let fp = LLVM_gen.get_function_pointer () in
    Printf.printf "jit: power(%d, %f) = %.20f\n" n x (fp x);
    let fname = Printf.sprintf "power_%d.s" n in
    LLVM_gen.emit_asm fname
    (* let open Core_bench in
     * [Bench.Test.create ~name:"llvm jit" (fun () -> ignore(fp x))] |> Bench.bench; (\* why so slow?  *\) *)
  in
  go 100000 1.00001;
