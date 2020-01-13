(* originally from https://keleshev.com/composable-error-handling-in-ocaml *)
(* modified to get it compile, work with dune, and let syntax in introduced in 4.08 *)
(* render returns tree type instead of string, to work around limitation of let* *)

type tree = int

module Parser = struct
  type error = [
    | `ParserSyntaxError of int
    | `ParserGrammarError of int * string
  ]

  let parse : string -> (tree, [> error]) result = fun _ ->
    match Random.int 3 with
    | 0 -> Ok 0
    | 1 -> Error (`ParserSyntaxError 0)
    | 2 -> Error (`ParserGrammarError (0, ""))
    | _ -> failwith "Random busted"
end

module Validation = struct
  type error = [
    | `ValidationLengthError of int
    | `ValidationHeightError of int
  ]

  let perform : tree -> (tree, [> error]) result = fun _ ->
    match Random.int 3 with
    | 0 -> Ok 0
    | 1 -> Error (`ValidationLengthError 0)
    | 2 -> Error (`ValidationHeightError 0)
    | _ -> failwith "Random busted"
end

module Display = struct
  type error = [
    | `DisplayError of string
  ]

  let render : tree -> (tree, [> error]) result = fun _ ->
    match Random.int 2 with
    | 0 -> Ok 0
    | 1 -> Error (`DisplayError "")
    | _ -> failwith "Random busted"
end

module Let_syntax = struct
  let bind res f =
    match res with
    | Ok r -> f r
    | _ -> res
  let (let*) = bind
end

let main source =
  let open Let_syntax in
  let* tree = Parser.parse source in
  let* tree = Validation.perform tree in
  Display.render tree

let handle_errors source =
  Random.self_init ();
  match main source with
  | Ok output ->
      Printf.printf "%d\n" output
  | Error (`ParserSyntaxError line) ->
      Printf.eprintf "Syntax error at line %d\n" line
  | Error (`ParserGrammarError (line, message)) ->
      Printf.eprintf "Grammar error at line %d: %s\n" line message
  | Error (`ValidationLengthError length) ->
      Printf.eprintf "Validation error: length %d is out of bounds\n" length
  | Error (`ValidationHeightError height) ->
      Printf.eprintf "Validation error: height %d is out of bounds\n" height
  | Error (`DisplayError message) ->
      Printf.eprintf "Display error: %s\n" message

let _ =
  handle_errors ""
