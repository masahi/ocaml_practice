open Codelib

let filter : float array -> float array -> float array = fun b x ->
  let m = Array.length b in
  let y i =
    if i < m-1 then x.(i) else
    let sum = ref 0.0 in
    for k = 0 to m-1 do
      sum := !sum +. b.(k) *. x.(i-k)
    done;
    !sum
  in
  Array.init (Array.length x) y         (* essentially, for-loop *)

let impulses : int -> int -> int -> float array = fun i j n ->
  let x = Array.make n 0.0 in
  x.(i) <- 1.0;
  x.(j) <- 1.0;
  x

let _ =
  filter [|0.5; 0.3; 0.2|] (impulses 2 2 7) |> Array.iter (fun x -> Printf.printf "%f\n" x)


type spine_arr = float code array
type dyn_arr = float array code

let filter_spine: spine_arr -> (float array -> float array) code =
  fun b -> .<fun x ->
    .~(let m = Array.length b in
       .<let y i =
           if i < m - 1 then x.(i) else
             .~(let sum = ref .<0.0>. in
                for k = 0 to m - 1 do
                  sum := .<.~(!sum) +. .~(b.(k)) *. x.(i-k)>.
                done;
               !sum)
       in
       Array.init (Array.length x) y
       >.)
    >.

let _ =
  let cde = .<fun b1 b2 b3 -> .~(filter_spine [| .<b1>.; .<b2>.; .<b3>. |])>. in
  print_code Format.std_formatter cde; print_newline();
