module type S = sig
  type node
  type edge
  type t

  val empty : t
  val get_nodes : t -> node list
  val get_edges : t -> node -> edge list
  val get_neighbors : t -> node -> node list
  val add_edge : t -> node -> node -> float -> t
  val get_edge_dst : edge -> node
  val get_edge_weight : edge -> float
end

module Make(V: Types.Ordered) : S with type node := V.t = struct
  module M = Treemap.Make(V)

  type edge = {
    dst : V.t;
    weight : float
  }
  type t = edge list M.t

  let empty = M.empty

  let get_nodes g = M.keys g

  let get_edges g node =
    match M.lookup g node with
    | None -> []
    | Some edge_list -> edge_list

  let get_neighbors g node =
    get_edges g node |> List.map (fun {dst; _}  -> dst)

  let add_edge g src dst weight =
    let edges = get_edges g src in
    M.insert g src ({dst; weight} :: edges)

  let get_edge_dst {dst; _} = dst
  let get_edge_weight {weight; _} = weight

end
