open Syntax

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)

let read_file file =
  let open Core_kernel in
  In_channel.read_all file

let _ =
  let input = read_file Sys.argv.(1) in
  let parsed = parse input in
  Eval.eval_top parsed |> print_value;
  Cam.compile parsed |> Cam.eval |> print_value
