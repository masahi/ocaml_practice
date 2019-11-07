type 'e rel = 'e -> 'e list
type 'e prop = 'e -> bool

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

let archive_map opset rel (s, l) =
  let new_elts = flat_map rel l in
  let l' =
    List.filter (fun elt -> opset.mem elt s |> not) new_elts
  in
  let s' =
    List.fold_left (fun acc elt -> opset.add elt acc) s l'
  in
  (s', l')

let solve' opset r p x =
  let rec iter s l =
    match find p l with
    | Some(x) -> x
    | None ->
      let (s', l') = archive_map opset r (s, l) in
      iter s' l'
  in
  let init_set = opset.add x opset.empty in
  iter init_set [x]

let solve_path' opset r p x =
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
  solve' opset path_rel path_prop [x] |> List.rev

let _ =
  let near = fun n -> [n - 2; n - 1; n; n + 1; n + 2] in
  let path = solve_path' int_list_set_operations near (fun x -> x = 12) 0 in
  List.iter (fun x -> Printf.printf "%d\n" x) path

type ('configuration, 'move) puzzle =
  { move : 'configuration -> 'move -> 'configuration;
    possible_moves : 'configuration -> 'move list;
    final : 'configuration -> bool }

let solve_puzzle p opset c =
  let rel = fun conf ->
    let moves = p.possible_moves conf in
    List.map (fun move -> p.move c move) moves
  in
  solve_path' opset rel p.final c

type piece_kind = S | H | V | C | X
type piece = piece_kind * int
let x = (X, 0) and s = (S, 0) and h = (H, 0)
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3))
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3))
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ]

type board = piece array array

let final board =
  board.(3).(1) = s &&
  board.(3).(2) = s &&
  board.(4).(1) = s &&
  board.(4).(2) = s

type move = Move of piece * direction * board
and direction = { dcol : int; drow : int }

let move _ (Move (_, _, b)) = b

let move_piece board p {drow; dcol} =
  let list_diff l1 l2 =
    List.filter (fun elt -> List.mem elt l2 |> not ) l1
  in
  let occupied_pos (i, j) =
    match p with
    | (S, _) -> [(i, j); (i+1, j); (i, j+1); (i+1, j+1)]
    | (H, _) -> [(i, j); (i, j+1)]
    | (V, _) -> [(i, j); (i+1, j)]
    | (C, _) -> [(i, j)]
    | _ -> []
  in
  let diff_occupied_pos (i, j) =
    let current = occupied_pos (i, j) in
    let next = (occupied_pos (i+drow, j+dcol)) in
    let next_occupied_pos = list_diff next current in
    let next_vacant_pos = list_diff current next in
    (next_occupied_pos, next_vacant_pos)
  in
  let can_move next_occupied =
    let in_bound i j =
      i >= 0 && i < 5 && j >= 0 && j < 4
    in
    List.for_all (fun (i, j) -> in_bound i j && board.(i).(j) = x) next_occupied
  in
  let pos = ref None in
  Array.iteri (fun i row ->
      Array.iteri (fun j elt ->
          if elt = p then
            match !pos with
            | None ->
              pos := Some(i, j)
            | Some(i, j) -> ()
          else
            ()
        ) row
    ) board;
  match !pos with
  | None -> None
  | Some(i, j) ->
    let (next_occupied, next_vacant) = diff_occupied_pos (i, j) in
    if can_move next_occupied then begin
      let board_copy = Array.copy board in
      List.iter (fun (i, j) -> board_copy.(i).(j) <- p) next_occupied;
      List.iter (fun (i, j) -> board_copy.(i).(j) <- x) next_vacant;
      Some(board_copy)
    end else
      None

let concat_map l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (List.rev_append (f hd) acc) tl
  in
  aux [] l

let possible_moves board =
  let get_moves p =
    let directions = [{drow=0; dcol=1}; {drow=0; dcol=(~-1)}; {drow=1; dcol=0}; {drow=(~-1); dcol=0}] in
    List.filter_map (fun dir ->
        match move_piece board p dir with
        | None -> None
        | Some(b) -> Some(Move(p, dir, b))
      ) directions
  in
  concat_map ~f:(fun p -> get_moves p) all_pieces

let klotski = { move; possible_moves; final }
