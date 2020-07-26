open Codelib

module type MONOID = sig
  type t
  val unit : t
  val (<*>) : t -> t -> t
end

type sta = Sta and dyn = Dyn and eseq = E: _ seq -> eseq
and _ seq =
    Empty : _ seq
  | ConsS : string      * dyn seq -> sta seq
  | ConsD : string code *  _  seq -> dyn seq

let consS : string -> eseq -> eseq =
  fun x xs -> match xs with
    | E Empty -> E (ConsS (x, Empty))
    | E (ConsS (y, ys)) -> E (ConsS (x ^ y, ys))
    | E (ConsD (_,_) as r) -> E (ConsS (x, r))

let consD : string code -> eseq -> eseq =
  fun x (E xs) -> E (ConsD (x, xs))

let rec (<*>) : eseq -> eseq -> eseq =
  fun (E l) r -> match l with
    | Empty -> r
    | ConsS (h, t) -> consS h (E t <*> r)
    | ConsD (h, t) -> consD h (E t <*> r)

module PS_string =
struct
  type ps = eseq
  module N = struct
    type t = ps
    let unit = Empty
    let (<*>) = (<*>)
  end
   let dyn d = consD d (E Empty)
   let sta s = consS s (E Empty)
   module Eva(O:MONOID) = struct
     let rec eva : (string code -> O.t) -> (string -> O.t) -> ps -> O.t
       = fun f g e  -> match e with
           E Empty -> O.unit
         | E (ConsS (h, Empty)) -> g h
         | E (ConsD (h, Empty)) -> f h
         | E (ConsS (h, t)) -> O.(g h <*> eva f g (E t))
         | E (ConsD (h, t)) -> O.(f h <*> eva f g (E t))
   end
end
module Code_string_monoid = struct
  type t = string code
  let unit = .<"">.
  let (<*>) l r = .< .~l ^ .~r >.
end

let cd = let module E = PS_string.Eva(Code_string_monoid) in
  E.eva (fun d -> d) (fun s -> .<s>.)

(* Preliminaries: format strings *)
type (_,_) fmt =
  | Int : (int -> 'a, 'a) fmt
  | Lit : string -> ('a, 'a) fmt
  | Bool : (bool -> 'a, 'a) fmt
  | Cat : ('a,'b) fmt * ('b,'c) fmt -> ('a,'c) fmt
let (%) x y = Cat (x, y)

let rec printk : type a r. (a, r) fmt -> (string -> r) -> a =
  fun fmt k -> match fmt with
    | Int -> fun i -> k (string_of_int i)
    | Bool -> fun b -> k (string_of_bool b)
    | Lit s -> k s
    | Cat (l, r) -> printk l (fun x ->
        printk r (fun y ->
            k (x ^ y)))

let sprintf : type a. (a, string) fmt -> a =
  fun fmt -> printk fmt (fun x -> x)

let _ =
  let s = sprintf (Bool % Lit "," % Lit " " % Bool % Lit " " % Int) true false 1 in
  Printf.printf "%s\n" s

 let rec printk2 : type a r. (a,r) fmt -> (string code -> r code) -> a code
 = fun fmt k -> match fmt with
   | Int -> .< fun i -> .~(k .<string_of_int i>.) >.
   | Bool -> .< fun b -> .~(k .<string_of_bool b>.) >.
   | Lit s -> k .< s >. (* CSP *)
   | Cat (l, r) -> printk2 l (fun x ->
                  printk2 r (fun y ->
                  k .< .~x ^ .~y >.))

 let sprintf2 : type a. (a, string) fmt -> a code =
   fun fmt -> printk2 fmt (fun x -> x)

let _ =
  let cde = sprintf2 (Bool % Lit "," % Lit " " % Bool) in
  print_code Format.std_formatter cde; print_newline ();
  let s = Runnative.run cde true false in
  Printf.printf "%s\n" s

let rec printk3
  : type a r. (a, r) fmt -> (PS_string.ps -> r code) -> a code
  = let open PS_string in fun fmt k -> match fmt with
    | Int -> .< fun i -> .~(k (dyn .<string_of_int i>.)) >.
    | Bool -> .< fun b -> .~(k (dyn .<string_of_bool b>.)) >.
    | Lit s -> k (sta s)   (* better! *)
    | Cat (l, r) -> printk3 l (fun x ->
        printk3 r (fun y ->
            k (x <*> y)))

let sprintf3 : type a. (a, string) fmt -> a code =
  fun fmt -> printk3 fmt cd

let _ =
  let cde = sprintf3 (Bool % Lit "," % Lit " " % Bool) in
  print_code Format.std_formatter cde; print_newline ();
  let s = Runnative.run cde true false in
  Printf.printf "%s\n" s

let dot'
  : int -> float array code -> float array code -> float code
  = fun n l r ->
    let rec loop i =
      if i = n then .< 0. >.
      else .< ((.~ l).(i) *. (.~ r).(i))
           +. .~(loop (i + 1)) >.
    in loop 0

let dot''
  : int -> float array -> float array code -> float code
  = fun n l r ->
    let rec loop i =
      if i = n then .< 0. >.
      else let li = l.(i) in
        .< (li *. (.~ r).(i)) +. .~(loop (i + 1)) >.
    in loop 0

let _ =
  let cde = .< fun a -> .~(dot'' 3 [| 0.0; 1.0; 2.0|] .<a>.)>. in
  print_code Format.std_formatter cde; print_newline ()
