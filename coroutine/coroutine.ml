(* originally from https://gist.github.com/keigoi/5860564 *)
(* modified to work with dune *)

(* Coroutine implementation in OCaml, with Oleg's delimited continuation *)
(* see http://okmij.org/ftp/continuations/implementations.html#caml-shift *)
module D = Delimcc

(* Coroutine yielded a value of type 'a, and will resume with some value of type 'b *)
type ('a, 'b) suspend =
  | Cont of 'a * ('b, ('a,'b) suspend) D.subcont
  | Finish

let start_coroutine f =
  let p = D.new_prompt () in
  (* a function to switch back to main thread *)
  let switch_to_main x = D.take_subcont p (fun k () -> Cont(x,k))
  in
  (* start the coroutine until it suspends *)
  match D.push_prompt p (fun () -> f switch_to_main; Finish) with
  | Cont(x,k) ->
    let next = ref k in
    (* function *)
    let continue_coroutine y =
      match D.push_delim_subcont !next (fun () -> y) with
      | Cont(x,k) -> next := k; x
      | Finish -> assert false
    in
    (x, continue_coroutine)
  | _ -> assert false


(* example *)
let _ =

  let rec hello_server f =
    print_endline ("Hello, " ^ f ());
    hello_server f
  in

  (* start a hello-server coroutine *)
  let _, f = start_coroutine hello_server in

  f "OCaml";
  f "Nagoya";
  f "keigoi";

  let inc_server x f =
    let rec loop x =
      ignore (f x);
      loop (x+1)
    in loop x
  in

  (* start another coroutine *)
  let _, g = start_coroutine (inc_server 0) in

  let print = Printf.printf "got %d\n"
  in
  print (g ()); (* 1 *)
  print (g ()); (* 2 *)
  print (g ()); (* 3 *)
