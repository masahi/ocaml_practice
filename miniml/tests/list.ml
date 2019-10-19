let rec len lst =
  if lst = [] then 0
  else 1 + len (List.tl lst) in
let rec map f = fun lst ->
  if lst = [] then []
  else
    let first = f (List.hd lst) in
    let rest = map f (List.tl lst) in
    first :: rest
in
let square = fun x -> x * x in
let fn = map square in
let lst = 1 :: 2 :: 3 :: [] in
fn lst

