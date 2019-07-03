open OffshoringIR

let square x = x *. x

let rec power n x =
  if n = 0 then 1.
  else if n mod 2 = 0 then square (power (n/2) x)
  else x *. (power (n-1) x)

let rec spower n x =
  if n = 0 then .<1.>.
  else if n mod 2 = 0 then
    .<let x' = .~x *. .~x in
    .~(spower (n/2) .<x'>.)>.
  else .<.~x *. .~(spower (n-1) x)>.

let spowern n = .<fun x -> .~(spower n .<x>.)>.

let pprint_offshore_ir = function
 | Fun(_) -> Printf.printf "%s\n" "Fun(args, typ, exp)"
 | Proc(_) -> Printf.printf "%s\n" "Prof(args, cmd)"

let _ =
  let go n _ =
    let power_staged = spowern n in
    let proc = offshore (module DefaultConv) power_staged in
    pprint_offshore_ir proc
  in
  go 100000 1.00001
