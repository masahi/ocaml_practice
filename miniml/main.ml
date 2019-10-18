open Syntax

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)

let _ =
  let parsed =
    parse "let rec fact x = if x = 0 then 1 else x * (fact (x-1)) in fact 5" in
  let open Eval in
  let env = emptyenv () in
  match eval parsed env with
  | IntVal(x) -> Printf.printf "%d\n" x
  | _ -> assert false
