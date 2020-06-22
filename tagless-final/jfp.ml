open Codelib
(* let varZ env = fst env
 * let varS vp env = vp (snd env)
 *
 * let b (bv:bool) env = bv
 *
 * let lam e env = fun x -> e (x, env)
 *
 * let app e1 e2 env = (e1 env) (e2 env)
 *
 * let testf1 = app (lam varZ) (b true)
 *
 * let testf3 = app (lam (varS varZ)) (b true) *)


module type Symantics = sig
  type 'a repr
  val int  : int  -> int repr
  val bool : bool -> bool repr

  val lam : ('a repr -> 'b repr) -> ('a -> 'b) repr
  val app : ('a -> 'b) repr -> 'a repr -> 'b repr
  val fix : ('x -> 'x) -> (('a -> 'b) repr as 'x)

  val add  : int repr -> int repr -> int repr
  val mul  : int repr -> int repr -> int repr
  val leq  : int repr -> int repr -> bool repr
  val if_ : bool repr -> (unit -> 'x) -> (unit -> 'x) -> ('a repr as 'x)

end

module EX(S: Symantics) = struct
  open S

  let test1 () = app (lam (fun x -> x)) (bool true)

  let testpowerfix () =
    lam (fun x -> fix (fun self -> lam (fun n ->
        if_ (leq n (int 1)) (fun () -> (int 1))
          (fun () -> mul x (app self (add n (int (-1))))))))

  let testpowerfix7 = lam (fun x -> app (app (testpowerfix ()) x) (int 7))
end

module R = struct
  type 'a repr = 'a
  let int (x:int) = x
  let bool b = b
  let lam f = f
  let app e1 e2 = e1 e2
  let fix f = let rec self n = f self n in self
  let add e1 e2 = e1 + e2
  let mul e1 e2 = e1 * e2
  let leq e1 e2 = e1 < e2
  let if_ eb et ee = if eb then (et ()) else (ee ())
end

module L = struct
  type 'a repr = int
  let int (_:int) = 1
  let bool _ = 1
  let lam f = f 0 + 1
  let app e1 e2 = e1 + e2 + 1
  let fix f = f 0 + 1
  let add e1 e2 = e1 + e2 + 1
  let mul e1 e2 = e1 + e2 + 1
  let leq e1 e2 = e1 + e2 + 1
  let if_ eb et ee = eb + (et ()) + (ee ()) + 1
end

module C = struct
  type 'a repr = 'a code
  let int (x:int) = .<x>.
  let bool b = .<b>.
  let lam f = .<fun x -> .~(f .<x>.)>.
  let app e1 e2 = .<.~e1 .~e2>.
  let fix f = .<let rec self n = .~(f .<self>.) n in self>.
  let add e1 e2 = .<.~e1 + .~e2>.
  let mul e1 e2 = .<.~e1 * .~e2>.
  let leq e1 e2 = .<.~e1 < .~e2>.
  let if_ eb et ee = .<if .~eb then .~(et ()) else .~(ee ())>.
end

module EXR = EX(R)
module EXL = EX(L)
module EXC = EX(C)

let _ =
  Printf.printf "%d\n" (EXR.testpowerfix7 2);
  print_code Format.std_formatter (EXC.testpowerfix7);
  let fact = Runnative.run (EXC.testpowerfix7) in
  Printf.printf "\n%d\n" (fact 2)
