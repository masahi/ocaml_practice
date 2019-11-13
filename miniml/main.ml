open Syntax

let parse str =
  Parser.main Lexer.token
    (Lexing.from_string str)

let read_file file =
  let open Core_kernel in
  In_channel.read_all file

let _ =
  let input = read_file Sys.argv.(1) in
  let parsed = parse input |> Desugar.match_to_if in
  let interp_res = Eval.eval_top parsed in
  let cam_res = Cam.compile parsed |> Cam.eval in
  let zam_res = Zam.compile parsed |> Zam.eval in
  Printf.printf "Interpreter result: %s\n" (interp_res |> string_of_value);
  Printf.printf "CAM result: %s\n" (cam_res |> string_of_value);
  Printf.printf "ZAM result: %s\n" (zam_res |> string_of_value)
