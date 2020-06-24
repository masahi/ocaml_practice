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

module type Symantics_PE = sig
  type ('sv, 'dv) repr
  val int  : int  -> (int, int) repr
  val bool : bool -> (bool, bool) repr

  val lam : (('sa, 'da) repr -> ('sb, 'db) repr as 'x) -> ('x, 'da -> 'db) repr
  val app : ('x, 'da -> 'db) repr -> (('sa, 'da) repr -> ('sb, 'db) repr as 'x)
  val fix : ('x -> 'x) -> ((('sa, 'da) repr -> ('sb, 'db) repr, 'da -> 'db) repr as 'x)

  val add  : (int, int) repr -> (int, int) repr -> (int, int) repr
  val mul  : (int, int) repr -> (int, int) repr -> (int, int) repr
  val leq  : (int, int) repr -> (int, int) repr -> (bool, bool) repr
  val if_ : (bool, bool) repr -> (unit -> 'x) -> (unit -> 'x) -> (('sa, 'da) repr as 'x)

end

module P =
struct
  type ('sv,'dv) repr = {st: 'sv option; dy: 'dv code}
  let abstr {dy = x; _} = x
  let pdyn x = {st = None; dy = x}

  let int  (x:int)  = {st = Some (R.int x);
                       dy = C.int x}
  let bool (x:bool) = {st = Some (R.bool x);
                       dy = C.bool x}

  (* generic build - takes a repr constructor, an interpreter function
     and a compiler function (all binary) and builds a PE version *)
  let build cast f1 f2 = function
    | {st = Some m; _}, {st = Some n; _} -> cast (f1 m n)
    | e1, e2 -> pdyn (f2 (abstr e1) (abstr e2))
  (* same as 'build' but takes care of the neutral element (e) simplification
     allowed via a monoid structure which is implicitly present *)
  let monoid cast one f1 f2 = function
    | {st = Some e'; _}, e when e' = one -> e
    | e, {st = Some e'; _} when e' = one -> e
    | ee -> build cast f1 f2 ee
  (* same as above but for a ring structure instead of monoid *)
  let ring cast zero one f1 f2 = function
    | ({st = Some e'; _} as e), _ when e' = zero -> e
    | _, ({st = Some e'; _} as e) when e' = zero -> e
    | ee -> monoid cast one f1 f2 ee

  let add e1 e2 = monoid int 0 R.add C.add (e1,e2)
  let mul e1 e2 = ring int 0 1 R.mul C.mul (e1,e2)
  let leq e1 e2 = build bool R.leq C.leq (e1,e2)
  let if_ eb et ee = match eb with
    | {st = Some b; _} -> if b then et () else ee ()
    | _ -> pdyn (C.if_ (abstr eb)
                   (fun () -> abstr (et ()))
                   (fun () -> abstr (ee ())))

  let lam f =
    {st = Some f;
     dy = C.lam (fun x -> abstr (f (pdyn x)))}

  let app ef ea = match ef with
    | {st = Some f; _} -> f ea
    | _ -> pdyn (C.app (abstr ef) (abstr ea))

   (*
     For now, to avoid divergence at the PE stage, we residualize.
     Actually, we unroll the fixpoint exactly once, and then
     residualize
   *)
  (* let fix f = f (pdyn (C.fix (fun x -> abstr (f (pdyn x)))))
  *)
  let fix f = let fdyn = C.fix (fun x -> abstr (f (pdyn x)))
    in let rec self = function
        | {st = Some _; _} as e -> app (f (lam self)) e
        | e -> pdyn (C.app fdyn (abstr e))
    in {st = Some self; dy = fdyn}

end

module P_GADT = struct
  type _ repr =
    | VI: int -> int repr
    | VB: bool -> bool repr
    | VF: ('a repr -> 'b repr) -> ('a -> 'b) repr
    | Dyn: 'a C.repr -> 'a repr

  let pdyn x = Dyn x

  let rec abstr: type a. a repr -> a C.repr = function
    | VI(v) -> C.int v
    | VB(b) -> C.bool b
    | VF(f) -> C.lam (fun x -> abstr (f (pdyn x)))
    | Dyn(d) -> d

  let int i = VI i

  let bool b = VB b

  let lam f = VF f

  let app: type a b. (a -> b) repr -> a repr -> b repr = fun func arg ->
    match func with
    | VF(f) -> f arg
    | Dyn(f) -> Dyn (C.app f (abstr arg))

  (* val fix : ('x -> 'x) -> (('a -> 'b) repr as 'x)   *)
  (* let fix f = let fdyn = C.fix (fun x -> abstr (f (pdyn x)))
   *   in let rec self = function
   *       | {st = Some _; _} as e -> app (f (lam self)) e
   *       | e -> pdyn (C.app fdyn (abstr e))
   *   in {st = Some self; dy = fdyn} *)

  let fix: type a b. ((a -> b) repr -> (a -> b) repr) -> (a -> b) repr = fun f ->
    let fdyn = C.fix (fun x -> abstr (f (pdyn x))) in
    let rec self: a repr -> b repr = fun arg ->
      match arg with
     | VI(_) -> app (f (lam self)) arg
     | VB(_) -> app (f (lam self)) arg
     | VF(_) -> app (f (lam self)) arg
     | Dyn(c) -> Dyn (C.app fdyn c)
    in
    lam self

  let add (e1: int repr) (e2: int repr) =
    match e1, e2 with
    | VI(0), _ -> e2
    | _, VI(0) -> e1
    | VI(i1), VI(i2) -> VI(i1 + i2)
    | _, _ -> Dyn(C.add (abstr(e1)) (abstr(e2)))

  let mul (e1: int repr) (e2: int repr) =
    match e1, e2 with
    | VI(0), _ -> int 0
    | _, VI(0) -> int 0
    | VI(1), _ -> e2
    | _, VI(1) -> e1
    | VI(i1), VI(i2) -> VI(i1 * i2)
    | _, _ -> Dyn(C.mul (abstr(e1)) (abstr(e2)))

  let leq (e1: int repr) (e2: int repr) =
    match e1, e2 with
    | VI(i1), VI(i2) -> VB(i1 <= i2)
    | _, _ -> Dyn(C.leq (abstr(e1)) (abstr(e2)))

  let if_: type a. bool repr -> (unit -> a repr) -> (unit -> a repr) -> a repr = fun be et ee ->
    match be with
    | VB(b) -> if b then et () else ee ()
    | _ -> Dyn (C.if_ (abstr be) (fun () -> (abstr (et ()))) (fun () -> (abstr (ee ()))))
  (*
   * val fix : ('x -> 'x) -> (('a -> 'b) repr as 'x)
 *)
end

module EX_PE(S: Symantics_PE) = struct
  open S

  let test1 () = app (lam (fun x -> x)) (bool true)

  let testpowerfix () =
    lam (fun x -> fix (fun self -> lam (fun n ->
        if_ (leq n (int 1)) (fun () -> (int 1))
          (fun () -> mul x (app self (add n (int (-1))))))))

  let testpowerfix7 = lam (fun x -> app (app (testpowerfix ()) x) (int 7))
end

module EXR = EX(R)
module EXL = EX(L)
module EXC = EX(C)
module EXP = EX_PE(P)

let _ =
  Printf.printf "%d\n" (EXR.testpowerfix7 2);
  print_code Format.std_formatter (EXC.testpowerfix7);
  let fact = Runnative.run (EXC.testpowerfix7) in
  Printf.printf "\n%d\n" (fact 2)

let _ =
  (* Printf.printf "%d\n" (EXR.testpowerfix7 2); *)
  print_code Format.std_formatter (EXP.testpowerfix7.dy);
  let fact = Runnative.run (EXP.testpowerfix7.dy) in
  Printf.printf "\n%d\n" (fact 2)
