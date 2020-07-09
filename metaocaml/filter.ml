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
  print_code Format.std_formatter cde; print_newline()

let filter_staged: float array -> (float array -> float array) code =
  fun b -> .<fun x ->
    .~(let m = Array.length b in
       .<let y i =
           if i < m - 1 then x.(i) else
             .~(let sum = ref .<0.0>. in
                for k = 0 to m - 1 do
                  let bk = b.(k) in
                  sum := .<.~(!sum) +. bk *. x.(i-k)>.
                done;
               !sum)
       in
       Array.init (Array.length x) y
       >.)
    >.

let _ =
  let cde = filter_staged [|0.5; 0.3; 0.2|] in
  print_code Format.std_formatter cde; print_newline()

let rec convert_list: float code list -> float list code = function
  | hd :: tl ->
    let tl_code = convert_list tl in
    .<.~hd :: .~tl_code>.
  | [] -> .<[]>.

let convert_array: float code array -> float array code =
  fun arr ->
    let lst = List.init (Array.length arr) (fun i -> arr.(i)) in
    let lst_code = convert_list lst in
     .<Array.of_list .~lst_code >.

let _ =
  let cde = convert_array [|.<1.>.; .<2.>.; .<3.>. |] in
  print_code Format.std_formatter cde; print_newline()

let filter_spine_adaptive ?(threshold=3) :
  spine_arr -> (float array -> float array) code = fun b ->
  let m = Array.length b in
  if m < threshold then
    filter_spine b
  else
    .<let arr_code = .~(convert_array b) in
    fun x ->
      let y i =
        if i < m - 1 then x.(i)
        else
          let sum = ref 0.0 in
          for k = 0 to m - 1 do
            sum := !sum +. arr_code.(k) *. x.(i - k)
          done;
          !sum
      in
      Array.init (Array.length x) y>.

let _ =
  let cde = .<fun b1 b2 b3 b4 -> .~(filter_spine_adaptive [|.<b1>.; .<b2>.; .<b3>. ; .<b4>. |])>. in
  print_code Format.std_formatter cde; print_newline()
