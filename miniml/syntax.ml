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

let string_of_value = function
  | IntVal(n) -> Printf.sprintf "%d" n
  | BoolVal(b) -> Printf.sprintf "%b" b
  | ListVal(lst) ->
    List.map ~f:to_string lst |> String.concat ~sep:", " |> Printf.sprintf "[%s]"
  | FunVal(_) | RecFunVal(_) -> "fun val"
