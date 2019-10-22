open Syntax

let desugar_list_patmatch exp = function
  | (Empty, case1)::[(Cons(Var(hd), Var(tl)), case2)] ->
    let cond = Binop(Eq, exp, Empty) in
    let then_clause = case1 in
    let else_clause =
      Let(hd, Head(exp), Let(tl, Tail(exp), case2))
    in
    If(cond, then_clause, else_clause)
  | _ -> assert false

let match_to_if = function
  | Match(exp, patlist) -> desugar_list_patmatch exp patlist
  | e -> e
