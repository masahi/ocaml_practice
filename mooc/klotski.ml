module Solver = struct
  type 'e rel = 'e -> 'e list
  type 'e prop = 'e -> bool
  type ('a, 'set) set_operations =
    { empty : 'set ;
      mem : 'a -> 'set -> bool ;
      add : 'a -> 'set -> 'set }
  type ('configuration, 'move) puzzle =
    { move : 'configuration -> 'move -> 'configuration;
      possible_moves : 'configuration -> 'move list;
      final : 'configuration -> bool }

  let archive_map opset rel (s, l) =
    let open Base in
    let rec iter seen new_elts = function
      | [] -> (seen, new_elts)
      | x::xs ->
        let (seen, new_elts) =
        List.fold_left (rel x) ~init:(seen, new_elts) ~f:(fun (set, frontiers) elt ->
            if opset.mem elt set then (set, frontiers)
            else (opset.add elt set, elt :: frontiers)
          )
        in
        iter seen new_elts xs
    in
    iter s [] l

  let solve' opset r p x =
    let rec iter s l round =
      match List.find_opt p l with
      | Some(x) -> x
      | None ->
        let (s', l') = archive_map opset r (s, l) in
        Printf.printf "Round %d, frontier size: %d\n" round (List.length l');
        iter s' l' (round + 1)
    in
    let init_set = opset.add x opset.empty in
    iter init_set [x] 0

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

  let solve_puzzle p opset c =
    let rel = fun conf ->
      let moves = p.possible_moves conf in
      List.map (fun move -> p.move c move) moves
    in
    solve_path' opset rel p.final c
end

type piece_kind = S | H | V | C | X
type piece = piece_kind * int
type board = piece array array
type move = Move of piece * direction * board
and direction = { dcol : int; drow : int }

let x = (X, 0) and s = (S, 0) and h = (H, 0)
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3))
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3))
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ]
let string_of_piece (kind, ind) =
  let char =
    match kind with
    | S -> "S"
    | H -> "H"
    | V -> "V"
    | C -> "C"
    | X -> "X"
  in
  Printf.sprintf "(%s, %d)" char ind

let final board =
  board.(3).(1) = s &&
  board.(3).(2) = s &&
  board.(4).(1) = s &&
  board.(4).(2) = s

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
  let copy_board board =
    Array.init 5 (fun i -> Array.copy board.(i))
  in
  let pos = ref None in
  let open Base in
  Array.iteri board ~f:(fun i row ->
      Array.iteri row ~f:(fun j elt ->
          if Caml.compare elt p = 0 then
            match !pos with
            | None ->
              pos := Some(i, j)
            | Some(_) -> ()
          else
            ()
        )
  );
  match !pos with
  | None -> None
  | Some(i, j) ->
    let (next_occupied, next_vacant) = diff_occupied_pos (i, j) in
    if can_move next_occupied then begin
      let board_copy = copy_board board in
      List.iter ~f:(fun (i, j) -> board_copy.(i).(j) <- p) next_occupied;
      List.iter ~f:(fun (i, j) -> board_copy.(i).(j) <- x) next_vacant;
      Some(board_copy)
    end else
      None

let possible_moves board =
  let open Base in
  let get_moves p =
    let directions = [{drow=0; dcol=1}; {drow=0; dcol=(~-1)}; {drow=1; dcol=0}; {drow=(~-1); dcol=0}] in
    List.filter_map directions ~f:(fun dir ->
        match move_piece board p dir with
        | None -> None
        | Some(b) -> Some(Move(p, dir, b))
      )
  in
  List.concat_map ~f:(fun p -> get_moves p) all_pieces

module BoardSetCompare = struct
    type t = board
    let compare b1 b2 =
      let compare_piece (k1, ind1) (k2, ind2) =
        let kind_to_int = function
          | S -> 5
          | H -> 4
          | C -> 3
          | V -> 2
          | X -> 1
        in
        if k1 = k2 then Int.compare ind1 ind2
        else Int.compare (kind_to_int k1) (kind_to_int k2)
      in
      let result = ref None in
      let open Base in
      Array.iter (Array.zip_exn b1 b2) ~f:(fun (row1, row2) ->
          Array.iter (Array.zip_exn row1 row2) ~f:(fun (elt1, elt2) ->
                  match !result with
                  | Some(_) -> ()
                  | None ->
                    let c = compare_piece elt1 elt2 in
                    if not (phys_equal c 0) then result := Some(c)
                    else ()
            )
        );
      match !result with
      | None -> 0
      | Some(c) -> c
end

module BoardListCompare = struct
  type t = board list
  let compare l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | b1::_, b2::_ -> BoardSetCompare.compare b1 b2
end

module BoardSet = Set.Make(BoardSetCompare)
module BoardListSet = Set.Make(BoardListCompare)

let solve_klotski initial_board =
  let open Solver in
  let klotski = { move; possible_moves; final } in
  let board_list_set_operations =
    { empty = BoardListSet.empty;
      mem = BoardListSet.mem;
      add = BoardListSet.add
    }
  in
  solve_puzzle klotski board_list_set_operations initial_board

let initial_board_simpler =
  [| [| c2 ; s  ; s  ; c1 |] ;
     [| c0 ; s  ; s  ; c3 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| x  ; x  ; x  ; x  |] |]

let initial_board =
  [| [| v0 ; s  ; s  ; v1 |];
     [| v0 ; s  ; s  ; v1 |];
     [| v2 ; h  ; h  ; v3 |];
     [| v2 ; c0 ; c1 ; v3 |];
     [| c2 ; x  ; x  ; c3 |] |]

let print_board board =
  Array.iter (fun row ->
      Array.iter (fun p -> Printf.printf "%s " (string_of_piece p)) row;
      Printf.printf "\n"
    )
    board;
  Printf.printf "\n"
    
let _ =
  solve_klotski initial_board_simpler |> List.iter print_board
