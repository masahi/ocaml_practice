type 'e rel = 'e -> 'e list
type 'e prop = 'e -> bool

let near = fun n ->
  [n - 2; n - 1; n; n + 1; n + 2]

let rec loop p f x =
  if p x = true then x
  else loop p f (f x)

let rec exists p = function
  | [] -> false
  | x::xs ->
    if p x then true
    else exists p xs

let rec find p = function
  | [] -> None
  | x::xs ->
    if p x then Some(x)
    else find p xs

let rec flat_map r = function
  | [] -> []
  | x::xs ->
    r x @ (flat_map r xs)

let rec iter_rel r n = fun x ->
  if n <= 1 then r x
  else
    let new_rel = iter_rel r (n - 1) in
    flat_map r (new_rel x)

let solve r p x =
  let rec iter configs =
    match find p configs with
    | Some(x) -> x
    | None -> iter (flat_map r configs)
  in
  iter [x]

let solve_path r p x =
  let path_rel = fun path ->
    match path with
    | [] -> []
    | hd::_ ->
      List.map (fun next -> next :: path) (r hd)
  in
  let path_prop = fun path ->
    match path with
    | [] -> false
    | hd::_ -> p hd
  in
  solve path_rel path_prop [x] |> List.rev

type ('a, 'set) set_operations =
  { empty : 'set ;
    mem : 'a -> 'set -> bool ;
    add : 'a -> 'set -> 'set }

module IntSet = Set.Make(Int)
module IntListSet = Set.Make(struct
  type t = int list
  let compare l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | l, [] -> 1
    | [], l -> -1
    | hd1::_, hd2::_ -> Int.compare hd1 hd2
end)

let int_set_operations =
  { empty = IntSet.empty;
    mem = IntSet.mem;
    add = IntSet.add
  }

let int_list_set_operations =
  { empty = IntListSet.empty;
    mem = IntListSet.mem;
    add = IntListSet.add
  }

let _ =
  let path = solve_path near (fun x -> x = 12) 0 in
  List.iter (fun x -> Printf.printf "%d\n" x) path
