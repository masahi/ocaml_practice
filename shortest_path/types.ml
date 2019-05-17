type node = string
type edge = {
  dst : node;
  weight : float
}

module Graph = struct
  module M = Treemap.Make(String)

  let empty = M.empty

  let get_edges g node =
    match M.lookup g node with
    | None -> []
    | Some edge_list -> edge_list

  let add_edge g src dst weight =
    let edges = get_edges g src in
    M.insert g src ((dst, weight) :: edges)
end
