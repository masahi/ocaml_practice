open Codelib

type ('p, 'v) monad = 's -> ('s -> 'v -> 'w) -> 'w
  constraint 'p = <state: 's; answer: 'w; ..>

let ret (a: 'v) : ('p, 'v) monad = fun s k -> k s a

let bind (m: ('p, 'v) monad) (f: 'v -> ('p, 'u) monad) : ('p, 'u) monad
    = fun s k -> m s (fun s' b -> f b s' k)

let fetch s k = k s s and store v _ k = k v ()

let k0 _ v = v
let runM m = fun s0 -> m s0 k0

let retN (a: 'v code) : (<answer: 'w code; ..>, 'v code) monad =
  fun s k -> .<let t = .~a in .~(k s .<t>.)>.

module Let_syntax = struct
  let (let*) d f = bind d f
end

let _ =
  let one = .<1>. in
  let plus x y = .<.~x + .~y>. in
  let simplest_code = let gen x y = plus x (plus y one) in
    .<fun x y -> .~(gen .<x>. .<y>.)>. in
  print_code Format.std_formatter simplest_code; print_newline ();
  let f = Runnative.run simplest_code in
  Printf.printf "%d\n" (f 2 3);
  let simplest_param_code plus one = let gen x y = plus x (plus y one) in
    .<fun x y -> .~(gen .<x>. .<y>.)>. in
  let plus x y = .<.~x +. .~y>. and one = .<1.0>. in
  let float_f = Runnative.run (simplest_param_code plus one) in
  Printf.printf "%f\n" (float_f 2. 3.);
  let letgen exp k = .<let t = .~exp in .~(k .<t>.)>. in
  let param_code2 plus one =
    let gen x y k = letgen (plus y one)
        (fun ce -> k (plus ce (plus x ce)))
    and
      k0 x = x
    in
    .<fun x y -> .~(gen .<x>. .<y>. k0)>. in
  print_code Format.std_formatter (param_code2 plus one); print_newline ();
  let f_cps = Runnative.run (param_code2 plus one) in
  Printf.printf "%f\n" (f_cps 2. 3.)

let _ =
  let plus x y = .<.~x +. .~y>. and one = .<1.0>. in
  let open Let_syntax in
  let param_code3 plus one =
    let gen x y =
      let* ce = retN (plus y one) in
      ret (plus ce (plus x ce))
    in
    .<fun x y -> .~(runM (gen .<x>. .<y>.) ())>. in

  print_code Format.std_formatter (param_code3 plus one); print_newline ();
  let f_monad = Runnative.run (param_code3 plus one) in
  Printf.printf "%f\n" (f_monad 2. 3.)
