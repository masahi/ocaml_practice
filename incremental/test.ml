module Inc = Incremental.Make ()

open Inc

let () =
  let x = Var.create 13 in
  let y = Var.create 17 in
  let z = map2 (Var.watch x) (Var.watch y) ~f:(fun x y -> x + y) in
  let z_o = observe z in
  stabilize ();
  assert (Observer.value_exn z_o = 30);
  Var.set x 19;
  stabilize ();
  assert (Observer.value_exn z_o = 36);
  let w = map2 (Var.watch y) z ~f:(fun y z -> y - z) in
  let w_o = observe w in
  stabilize ();
  Printf.printf "w: %d\n" (Observer.value_exn w_o);
