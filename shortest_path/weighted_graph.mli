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

module Make(V: Types.Ordered) : S with type node := V.t
