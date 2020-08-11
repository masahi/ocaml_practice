open Codelib
open Automata_type

let makeau:
  (token, 'a) automation ->
  ('a -> (token list -> bool) code) ->
  'a -> (token list -> bool) code =
  fun {finals; trans} self state ->
  let accept = List.mem state finals in
  let next token = List.assoc token (List.assoc state trans) in
  .<fun stream ->
  match stream with
  | A :: r -> .~(self (next A)) r
  | B :: r -> .~(self (next B)) r
  | [] -> accept>.

let au1 =
  {finals = [S];
   trans = [(S, [(A, S); (B, T)]);
            (T, [(A, S); (B, U)]);
            (U, [(A, T); (B, U)]);]}

let _ =
  let automata = Letrec.letrec (makeau au1) (fun automata -> automata S) in
  print_code Format.std_formatter automata; print_newline ()
