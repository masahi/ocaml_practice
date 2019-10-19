let rec len lst =
  if lst = [] then 0
  else 1 + len (List.tl lst) in
let rec map f = fun lst ->
  match lst with
    [] -> []
  | hd::tl ->
    let first = f hd in
    let rest = map f tl in
    first :: rest
in
let square = fun x -> x * x in
let fn = map square in
let lst = 1 :: 2 :: 3 :: [] in
fn lst
