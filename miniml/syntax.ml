open Base

type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | If of exp * exp * exp
  | Let of string * exp * exp
  | LetRec of string * string * exp * exp
  | Fun of string * exp
  | App of exp * exp
  | Binop of binop * exp * exp
  | Empty
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | Match of exp * ((exp * exp) list)
and binop =
  | Plus
  | Minus
  | Times
  | Eq

type value =
  | IntVal  of int
  | BoolVal of bool
  | ListVal of value list
  | FunVal  of string * exp * env
  | RecFunVal of string * string * exp * env
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
