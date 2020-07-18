module type Plugin = sig
  type base_type
  type change_type
  type value_type
  type prim
  type prim_arg_type =
    | Base of base_type (* given by user *)
    | Change of change_type (* given by user *)
    | PrimOutput of value_type (* previous output from other primitive*)
  val apply_prim: prim -> prim_arg_type list -> value_type
  val prim_derivative: prim -> prim
  val unwrap_value: value_type -> int
end

module Bag = struct
  module IntMap = Map.Make(Int)

  type base_type = int IntMap.t
  type change_type = int IntMap.t
  type value_type =
    | IntVal of int
    | BagVal of base_type

  type prim_arg_type =
    | Base of base_type
    | Change of change_type
    | PrimOutput of value_type

  type prim = FoldBag | DFoldBag | MergeBag | DMergeBag

  let unwrap_value = function
    | IntVal(v) -> v
    | _ -> failwith "int expected"

  let fold_bag b = IntMap.fold (fun i multiplicity acc -> acc + i * multiplicity) b 0

  let dfold_bag b b_change = IntMap.fold (fun i multiplicity acc -> acc + i * multiplicity) b_change 0

  let merge_bags b1 b2 =
    let merge_fun i a_opt b_opt =
      match a_opt, b_opt with
      | Some(a), Some(b) -> Some(a + b)
      | Some(_), _ -> a_opt
      | _, Some(_) -> b_opt
      | _ -> None
    in
    IntMap.merge merge_fun b1 b2

  let dmerge_bags b1 b1_change b2 b2_change = merge_bags b1_change b2_change

  let from_list lst =
    List.fold_left (fun acc (k, v) -> IntMap.add k v acc) IntMap.empty lst

  let apply_prim p args =
    let get_bag = function
      | PrimOutput(BagVal(b)) -> b
      | Base(b) -> b
      | Change(b) -> b
      | _ -> failwith "bag expected" in
    let args = List.map get_bag args in
    match p, args with
    | FoldBag, [b] -> IntVal(fold_bag b)
    | DFoldBag, [b; b_change] -> IntVal(dfold_bag b b_change)
    | MergeBag, [b1; b2] -> BagVal(merge_bags b1 b2)
    | DMergeBag, [b1; b1_change; b2; b2_change] -> BagVal(dmerge_bags b1 b1_change b2 b2_change)
    | _ -> failwith "unknown primitive or bad argument"

  let prim_derivative = function
    | FoldBag -> DFoldBag
    | MergeBag -> DMergeBag
    | _ -> failwith "unknown primitive"

end

module ILC(P: Plugin) = struct
  type term =
    | Var  of string
    | Lam  of string * term
    | App  of term * term
    | Prim of P.prim
    | BaseLit of P.base_type
    | ChangeLit of P.change_type

  type value =
    | BaseVal of P.base_type
    | ChangeVal of P.change_type
    | OutputVal of P.value_type
    | PrimVal of P.prim
    | FunVal of string * term * env
  and
    env = (string * value) list

  let unwrap = function
    | OutputVal(v) -> P.unwrap_value v
    | _ -> failwith "expects OutputVal"

  let rec lookup var_name = function
    | [] -> failwith "context empty"
    | (name, v) :: rest -> if name = var_name then v
      else lookup var_name rest

  let concat_string s1 s2 = String.concat "" [s1; s2]

  let app f x = App(f, x)

  let app2 f x y = App(App(f, x), y)

  let app4 f x y w z= app2 (app2 f x y) w z

  let rec eval env = function
    | Var(x) -> lookup x env
    | Lam(x, body) -> FunVal(x, body, env)
    | Prim(p) -> PrimVal(p)
    | BaseLit(b) -> BaseVal(b)
    | ChangeLit(b) -> ChangeVal(b)
    | App(t1, t2) ->
      let arg2 = eval env t2 in
      match extract_prim_and_args env t1 with
      | Some(p, args) ->
        apply_prim p (List.concat [args; [arg2]])
      | _ -> begin match eval env t1 with
          | FunVal(x, body, env') ->
            let new_env = (x, arg2) :: env' in
            eval new_env body
          | _ -> failwith "expects function"
        end
  and extract_prim_and_args env = function
    | App(t1, t2) -> begin match extract_prim_and_args env t1 with
        | Some(p, args) -> Some(p, List.concat [args; [eval env t2]])
        | _ -> None
      end
    | Prim(p) -> Some(p, [])
    | _ -> None

  and apply_prim p args =
    let ty_convert = function
      | BaseVal(b) -> P.Base(b)
      | ChangeVal(b) -> P.Change(b)
      | OutputVal(v) -> P.PrimOutput(v)
      | _ -> failwith "bad argument"
    in
    OutputVal (List.map ty_convert args |> P.apply_prim p)

  let rec derive = function
    | Var(x) -> Var(concat_string "d" x)
    | Lam(x, body) -> Lam(x, Lam(concat_string "d" x, derive body))
    | App(t1, t2) -> app2 (derive t1) t2 (derive t2)
    | Prim(p) -> Prim(P.prim_derivative p)
    | BaseLit(b) -> BaseLit(b)
    | ChangeLit(b) -> ChangeLit(b)

end

module Test = ILC(Bag)

open Test

let _ =
  let b = Bag.from_list [(1, 1); (2, -1); (3, 1)] in
  let b_change = Bag.from_list [(4, 1); (3, -1)] in
  let sum_bag = Lam("x", app (Prim FoldBag) (Var "x")) in
  let deriv = derive sum_bag in
  let sum = app sum_bag (BaseLit b) |> eval [] |> unwrap in
  let sum_change = app2 deriv (BaseLit b) (ChangeLit b_change) |> eval [] |> unwrap in
  let res_scratch = app sum_bag (BaseLit (Bag.merge_bags b b_change)) |> eval [] |> unwrap in
  Printf.printf "%d, %d, %d\n" res_scratch sum sum_change;
  assert (res_scratch = sum + sum_change)

let _ =
  let b1 = Bag.from_list [(1, 1); (2, -1); (3, 1)] in
  let b2 = Bag.from_list [(5, -1); (6, 2); (7, 1)] in
  let b1_change = Bag.from_list [(1, -1); (3, 2)] in
  let b2_change = Bag.from_list [(5, 1); (4, 2); (7, -1)] in
  let grand_total = Lam("x", Lam("y", app (Prim FoldBag) (app2 (Prim MergeBag) (Var "x") (Var "y")))) in
  let deriv = derive grand_total in
  let sum = app2 grand_total (BaseLit b1) (BaseLit b2) |> eval [] |> unwrap in
  let sum_change = app4 deriv (BaseLit b1) (ChangeLit b1_change) (BaseLit b2) (ChangeLit b2_change) |> eval [] |> unwrap in
  let res_scratch = app2 grand_total (BaseLit (Bag.merge_bags b1 b1_change)) (BaseLit (Bag.merge_bags b2 b2_change)) |> eval [] |> unwrap in
  Printf.printf "%d, %d, %d\n" res_scratch sum sum_change;
  assert (res_scratch = sum + sum_change);
  deriv
