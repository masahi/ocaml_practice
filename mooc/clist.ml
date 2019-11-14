type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))

let rec to_list = function
  | CSingle(x) -> [x]
  | CApp(left, right) -> to_list left @ to_list right
  | CEmpty -> []

let rec of_list = function
  | [] -> CEmpty
  | [x] -> CSingle x
  | x::xs -> CApp(CSingle x, of_list xs)

let append l1 l2 =
  match l1, l2 with
  | CEmpty, l -> l
  | l, CEmpty -> l
  | _ -> CApp(l1, l2)

let rec hd = function
  | CEmpty -> None
  | CSingle(x) -> Some(x)
  | CApp(left, _) -> hd left

let rec tl = function
  | CEmpty -> None
  | CSingle(_) -> Some(CEmpty)
  | CApp(left, right) ->
    match tl left with
    | None -> tl right
    | Some(l) -> Some(CApp(l, right))
