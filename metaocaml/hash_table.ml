open Codelib

let create_table = .< fun keys values ->
  let tbl = Hashtbl.create 123456 in
  let rec zip lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> []
    | h1::t1, h2::t2 -> (h1, h2) :: (zip t1 t2)
    | _, _ -> failwith "list len different" in
  let tbl = zip keys values |> List.fold_left (fun acc (k, v) ->  Hashtbl.add acc k v; acc) tbl in
  tbl
  >.

let sum_values = .< fun key tbl ->
  let values = Hashtbl.find_all tbl key in
  List.fold_left (fun acc v -> acc + v) 0 values >.

let test = .< fun keys values ->
  let tbl = .~(create_table) keys values in
  let keys_uniq = Hashtbl.to_seq_keys tbl |> List.of_seq |> List.sort_uniq String.compare in
  List.map (fun key -> (key, .~(sum_values) key tbl)) keys_uniq >.

let _ =
  print_code Format.std_formatter test;
  Printf.printf "\n";
  let keys = ["h"; "h"; "h"; "h"; "w"; "w"; "w"; "w"]; in
  let values = List.map (fun _ -> Random.int 100) keys in
  let pairs = (Runnative.run test) keys values in
  List.iter (fun (k, v) -> Printf.printf "key: %s, values: %d\n" k v) pairs
