let rec map f = fun lst ->
  match lst with
    [] -> []
  | hd::tl ->
    let first = f hd in
    let rest = map f tl in
    first :: rest
in
let square = fun x -> x * x in
let lst = 1 :: 2 :: 3 :: [] in
map square lst
