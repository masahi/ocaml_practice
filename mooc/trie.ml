type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let rec children_from_char key = function
  | [] -> None
  | (c, tri)::_ when c = key -> Some(tri)
  | _::rest -> children_from_char key rest

let rec update_children key new_trie = function
  | [] -> []
  | (c, _)::rest when c = key -> (key, new_trie)::rest
  | hd::rest -> hd :: (update_children key new_trie rest)

let explode s = List.init (String.length s) (String.get s)

let lookup tri key =
  let rec lookup_inner tri char_list =
    match tri, char_list with
    | Trie(None, _), [] -> None
    | Trie(Some(i), _), [] -> Some(i)
    | Trie(_, children), c::chars ->
      match children_from_char c children with
      | None -> None
      | Some(tri_inner) -> lookup_inner tri_inner chars
  in
  lookup_inner tri (explode key)

let insert tri key value =
  let rec insert_inner tri char_list value =
    match tri, char_list with
    | Trie(_, children), [] -> Trie(Some(value), children)
    | Trie(v, children), c::chars ->
      match children_from_char c children with
      | None ->
        let new_node = Trie(None, []) in
        let new_pair = (c, insert_inner new_node chars value) in
        Trie(v, new_pair::children)
      | Some(tri_inner) ->
        let new_tri_inner = insert_inner tri_inner chars value in
        Trie(v, update_children c new_tri_inner children)
  in
  insert_inner tri (explode key) value

let _ =
  assert(lookup example "A" = Some(15));
  assert(lookup (insert example "B" 1) "B" = Some(1));
  assert(lookup (insert example "t" 1) "ten" = Some(12));
  assert(lookup example "tea" = Some(3));
  assert(lookup (insert example "tea" 5) "tea" = Some(5));
  assert(lookup (insert example "tea" 5) "ten" = Some(12));
  assert(lookup (insert example "teaaa" 10) "tea" = Some(3));
  assert(lookup (insert example "teaaa" 10) "teaaa" = Some(10));
  assert(lookup (insert example "teaaa" 10) "ted" = Some(4));
  assert(lookup example "tec" = None);
  assert(lookup (insert example "tec" 3) "tec" = Some(3));
  assert(lookup example "i" = Some(11));
  assert(lookup (insert example "i" 8) "i" = Some(8));
  assert(lookup (insert example "in" 6) "inn" = Some(9));
