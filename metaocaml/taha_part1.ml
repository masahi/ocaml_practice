type exp = Int of int | Var of string | App of string * exp
         | Add of exp * exp | Sub of exp * exp
         | Mul of exp * exp | Div of exp * exp | Ifz of exp * exp * exp

type def = Declaration of string * string * exp
type prog = Program of def list * exp

let fact = Program([Declaration
                      ("fact", "x", Ifz(Var "x",
                                        Int 1,
                                        Mul(Var "x",
                                            (App ("fact", Sub(Var "x", (Int 1)))))))
                   ],
                   App("fact",  Div(Int 20, Int 2)))

exception Yikes

let env0 = fun _ -> raise Yikes

let fenv0 = env0

let ext env x v = fun y -> if x=y then v else env y

let rec eval1 e env fenv =
  match e with
  | Int i -> i
  | Var s -> env s
  | App (s,e2) -> (fenv s) (eval1 e2 env fenv)
  | Add (e1,e2) -> (eval1 e1 env fenv) + (eval1 e2 env fenv)
  | Sub (e1,e2) -> (eval1 e1 env fenv) - (eval1 e2 env fenv)
  | Mul (e1,e2) -> (eval1 e1 env fenv) * (eval1 e2 env fenv)
  | Div (e1,e2) -> (eval1 e1 env fenv) / (eval1 e2 env fenv)
  | Ifz (e1,e2,e3) ->
    if (eval1 e1 env fenv) = 0 then eval1 e2 env fenv
    else eval1 e3 env fenv

let rec peval1 p env fenv =
  match p with
  | Program ([], e) -> eval1 e env fenv
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    let rec f x = eval1 e1 (ext env s2 x) (ext fenv s1 f)
    in peval1 (Program(tl, e)) env (ext fenv s1 f)

let rec eval2 e env fenv =
  match e with
  | Int i -> .<i>.
  | Var s -> env s
  | App (s,e2) -> .<.~(fenv s) .~(eval2 e2 env fenv)>.
  | Add (e1,e2) -> .<.~(eval2 e1 env fenv) + .~(eval2 e2 env fenv)>.
  | Sub (e1,e2) -> .<.~(eval2 e1 env fenv) - .~(eval2 e2 env fenv)>.
  | Mul (e1,e2) -> .<.~(eval2 e1 env fenv) * .~(eval2 e2 env fenv)>.
  | Div (e1,e2) -> .<.~(eval2 e1 env fenv) / .~(eval2 e2 env fenv)>.
  | Ifz (e1,e2,e3) ->
    .<if .~(eval2 e1 env fenv) = 0 then .~(eval2 e2 env fenv)
    else .~(eval2 e3 env fenv)>.

let rec peval2 p env fenv =
  match p with
  | Program ([], e) -> eval2 e env fenv
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    .<let rec f x = .~(eval2 e1 (ext env s2 .<x>.) (ext fenv s1 .<f>.))
    in .~(peval2 (Program(tl, e)) env (ext fenv s1 .<f>.))>.

let rec eval3 e env fenv =
  match e with
  | Int i -> Some i
  | Var s -> Some (env s)
  | App (s,e2) ->
    begin match (eval3 e2 env fenv) with
    | Some x -> (fenv s) x
    | None -> None
    end
  | Add (e1,e2) ->
    begin match (eval3 e1 env fenv), (eval3 e2 env fenv) with
      | Some x, Some y -> Some (x + y)
      | _ -> None
    end
  | Sub (e1,e2) ->
    begin match (eval3 e1 env fenv), (eval3 e2 env fenv) with
      | Some x, Some y -> Some (x - y)
      | _ -> None
    end
  | Mul (e1,e2) ->
    begin match (eval3 e1 env fenv), (eval3 e2 env fenv) with
      | Some x, Some y -> Some (x * y)
      | _ -> None
    end
  | Div (e1,e2) ->
    begin match (eval3 e1 env fenv), (eval3 e2 env fenv) with
      | Some x, Some y when y > 0 -> Some (x / y)
      | _ -> None
    end
  | Ifz (e1,e2,e3) ->
    begin match (eval3 e1 env fenv) with
      | Some 0 -> eval3 e2 env fenv
      | Some _ -> eval3 e3 env fenv
      | None -> None
    end

let peval3 p env fenv =
  let rec inner p env fenv=
  match p with
  | Program ([], e) -> eval3 e env fenv
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    let rec f x = eval3 e1 (ext env s2 x) (ext fenv s1 f)
    in inner (Program(tl, e)) env (ext fenv s1 f)
  in
  match inner p env fenv with
  | Some x -> x
  | None -> assert false

let rec eval4 e env fenv =
  match e with
  | Int i -> .<Some i>.
  | Var s -> .<Some .~(env s)>.
  | App (s,e2) ->
    .<begin match .~(eval4 e2 env fenv) with
    | Some x -> .~(fenv s) x
    | None -> None
    end>.
  | Add (e1,e2) ->
    .<begin match .~(eval4 e1 env fenv), .~(eval4 e2 env fenv) with
      | Some x, Some y -> Some (x + y)
      | _ -> None
    end>.
  | Sub (e1,e2) ->
    .<begin match .~(eval4 e1 env fenv), .~(eval4 e2 env fenv) with
      | Some x, Some y -> Some (x - y)
      | _ -> None
    end>.
  | Mul (e1,e2) ->
    .<begin match .~(eval4 e1 env fenv), .~(eval4 e2 env fenv) with
      | Some x, Some y -> Some (x * y)
      | _ -> None
    end>.
  | Div (e1,e2) ->
    .<begin match .~(eval4 e1 env fenv), .~(eval4 e2 env fenv) with
      | Some x, Some y when y > 0 -> Some (x / y)
      | _ -> None
    end>.
  | Ifz (e1,e2,e3) ->
    .<begin match .~(eval4 e1 env fenv) with
      | Some 0 -> .~(eval4 e2 env fenv)
      | Some _ -> .~(eval4 e3 env fenv)
      | None -> None
    end>.

let peval4 p env fenv =
  let rec inner p env fenv=
  match p with
  | Program ([], e) -> eval4 e env fenv
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    .<let rec f x = .~(eval4 e1 (ext env s2 .<x>.) (ext fenv s1 .<f>.))
    in .~(inner (Program(tl, e)) env (ext fenv s1 .<f>.))>.
  in
  .<match .~(inner p env fenv) with
  | Some x -> x
  | None -> assert false
    >.

let rec eval5 e env fenv k =
  match e with
  | Int i -> k (Some i)
  | Var s -> k (Some (env s))
  | App (s,e2) ->
    eval5 e2 env fenv (function
    | Some x -> k (Some ((fenv s) x))
    | None -> k None
      )
  | Add (e1,e2) ->
    eval5 e1 env fenv (fun r ->
        eval5 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> k (Some (x + y))
              | _ -> k None
            end
          )
      )
  | Sub (e1,e2) ->
    eval5 e1 env fenv (fun r ->
        eval5 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> k (Some (x - y))
              | _ -> k None
            end
          )
      )
  | Mul (e1,e2) ->
    eval5 e1 env fenv (fun r ->
        eval5 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> k (Some (x * y))
              | _ -> k None
            end
          )
      )
  | Div (e1,e2) ->
    eval5 e1 env fenv (fun r ->
        eval5 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y when y > 0 -> k (Some (x / y))
              | _ -> k None
            end
          )
      )
  | Ifz (e1,e2,e3) ->
    eval5 e1 env fenv (function
      | Some 0 -> eval5 e2 env fenv k
      | Some _ -> eval5 e3 env fenv k
      | None -> k None
      )

let peval5 p env fenv =
  let rec inner p env fenv k =
  match p with
  | Program ([], e) -> eval5 e env fenv k
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    let rec f x = eval5 e1 (ext env s2 x) (ext fenv s1 f) k
    in inner (Program(tl, e)) env (ext fenv s1 f) k
  in
  let init_k = function
    | Some x -> x
    | _ -> assert false
  in
  inner p env fenv init_k

let rec eval6 e env fenv k =
  match e with
  | Int i -> k (Some .<i>.)
  | Var s -> k (Some (env s))
  | App (s,e2) ->
    eval6 e2 env fenv (function
    | Some x -> k (Some .<.~(fenv s) .~x>.)
    | None -> k None
      )
  | Add (e1,e2) ->
    eval6 e1 env fenv (fun r ->
        eval6 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> k (Some .<.~x + .~y>.)
              | _ -> k None
            end
          )
      )
  | Sub (e1,e2) ->
    eval6 e1 env fenv (fun r ->
        eval6 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> k (Some .<.~x - .~y>.)
              | _ -> k None
            end
          )
      )
  | Mul (e1,e2) ->
    eval6 e1 env fenv (fun r ->
        eval6 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> k (Some .<.~x * .~y>.)
              | _ -> k None
            end
          )
      )
  | Div (e1,e2) ->
    eval6 e1 env fenv (fun r ->
        eval6 e2 env fenv (fun s ->
            begin match r, s with
              | Some x, Some y -> .<if .~y = 0 then .~(k None)
                                  else .~(k (Some .<.~x / .~y>.))>.
              | _ -> k None
            end
          )
      )
  | Ifz (e1,e2,e3) ->
    eval6 e1 env fenv (function
        | Some x ->
          .<if .~x = 0 then .~(eval6 e2 env fenv k)
              else .~(eval6 e3 env fenv k)>.
        | None -> k None
      )

let peval6 p env fenv =
  let rec inner p env fenv k =
  match p with
  | Program ([], e) -> eval6 e env fenv k
  | Program (Declaration (s1, s2, e1) :: tl, e) ->
    .<let rec f x = .~(eval6 e1 (ext env s2 .<x>.) (ext fenv s1 .<f>.) k)
    in .~(inner (Program(tl, e)) env (ext fenv s1 .<f>.) k)>.
  in
  let init_k = function
    | Some x -> x
    | _ -> .<failwith "unexpected result">.
  in
  inner p env fenv init_k

let _ =
  let ret = peval1 fact env0 fenv0 in
  Printf.printf "%d\n" ret;
  let cde = peval2 fact env0 fenv0 in
  Printf.printf "%d\n" (Runnative.run cde);
  let ret = peval3 fact env0 fenv0 in
  Printf.printf "%d\n" ret;
  let cde = peval4 fact env0 fenv0 in
  Printf.printf "%d\n" (Runnative.run cde);
  let ret = peval5 fact env0 fenv0 in
  Printf.printf "%d\n" ret;
  let cde = peval6 fact env0 fenv0 in
  Printf.printf "%d\n" (Runnative.run cde);

  let open Codelib in
  let staged1_cde = .<fun () -> .~(peval2 fact env0 fenv0)>. in
  let staged2_cde = .<fun () -> .~(peval4 fact env0 fenv0)>. in
  let staged3_cde = .<fun () -> .~(peval6 fact env0 fenv0)>. in
  print_code Format.std_formatter staged1_cde; print_newline();
  print_code Format.std_formatter staged2_cde; print_newline();
  print_code Format.std_formatter staged3_cde; print_newline();
  let staged1_f = Runnative.run staged1_cde in
  let staged2_f = Runnative.run staged2_cde in
  let staged3_f = Runnative.run staged3_cde in

  let open Core_bench in
  [Bench.Test.create ~name:"unstaged1" (fun () -> ignore(peval1 fact env0 fenv0));
   Bench.Test.create ~name:"staged1" (fun () -> ignore(staged1_f ()));
   Bench.Test.create ~name:"unstaged2" (fun () -> ignore(peval3 fact env0 fenv0));
   Bench.Test.create ~name:"staged2" (fun () -> ignore(staged2_f ()));
   Bench.Test.create ~name:"unstaged3" (fun () -> ignore(peval5 fact env0 fenv0));
   Bench.Test.create ~name:"staged3" (fun () -> ignore(staged3_f ()));
  ]
  |> Bench.bench
