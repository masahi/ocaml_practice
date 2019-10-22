open Base

type exp =
  | Var of string         (* variable e.g. x *)
  | IntLit of int         (* integer literal e.g. 17 *)
  | BoolLit of bool       (* boolean literal e.g. true *)
  | If of exp * exp * exp (* if e then e else e *)
  | Let of string * exp * exp   (* let x=e in e *)
  | LetRec of string * string * exp * exp   (* letrec f x=e in e *)
  | Fun of string * exp   (* fun x -> e *)
  | App of exp * exp      (* function application i.e. e e *)
  | Binop of binop * exp * exp
  | Empty                 (* [ ] *)
  | Cons of exp * exp     (* e :: e *)
  | Head of exp           (* List.hd e *)
  | Tail of exp           (* List.tl e *)
  | Match of exp * ((exp * exp) list)    (* match e with e->e | ... *)
and binop =
  | Plus
  | Minus
  | Times
  | Eq

type value =
  | IntVal  of int        (* integer value e.g. 17 *)
  | BoolVal of bool       (* booleanvalue e.g. true *)
  | ListVal of value list (* list value e.g. [1;2;3] *)
  | FunVal  of string * exp * env
                          (* function value e.g. \x. x+1 with env *)
  | RecFunVal of string * string * exp * env
                          (* recursive function value: solution-1 *)
                          (* let rec f x = e1 in e2 *)
and
  env = (string * value) list

let to_string = function
  | IntVal(n) -> Int.to_string n
  | BoolVal(b) -> Bool.to_string b
  | _ -> assert false

let print_value = function
  | IntVal(n) -> Stdio.printf "%d\n" n
  | BoolVal(b) -> Stdio.printf "%b\n" b
  | ListVal(lst) ->
    List.map ~f:to_string lst |> String.concat ~sep:", " |> Stdio.printf "[%s]\n"
  | FunVal(_) | RecFunVal(_) -> Stdio.printf "fun val\n"
