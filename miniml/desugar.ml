open Syntax

let rec desugar_list_patmatch exp = function
  | (Empty, case1)::[(Cons(Var(hd), Var(tl)), case2)] ->
    let cond = Binop(Eq, exp, Empty) in
    let then_clause = match_to_if case1 in
    let else_clause =
      Let(hd, Head(exp), Let(tl, Tail(exp), match_to_if case2))
    in
    If(cond, then_clause, else_clause)
  | _ -> assert false
and match_to_if = function
  | Match(exp, patlist) -> desugar_list_patmatch exp patlist
  | Binop(op, e1, e2) -> Binop(op, match_to_if e1, match_to_if e2)
  | If(e1,e2,e3) ->
    If(match_to_if e1, match_to_if e2, match_to_if e3)
  | Let(x,e1,e2) -> Let(x, match_to_if e1, match_to_if e2)
  | Fun(x,e1) -> Fun(x, match_to_if e1)
  | App(e1,e2) -> App(match_to_if e1, match_to_if e2)
  | LetRec(f,x,e1,e2) -> LetRec(f, x, match_to_if e1, match_to_if e2)
  | Cons(e1,e2) -> Cons(match_to_if e1, match_to_if e2)
  | Head(lst) -> Head(match_to_if lst)
  | Tail(lst) -> Tail(match_to_if lst)
  | e -> e
