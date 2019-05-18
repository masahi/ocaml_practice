module OcamlGraph = Graph
open Base

module G = struct
  type t = Decl.Graph.t

  module V = struct
    type t = string
    let compare = String.compare
    let hash = String.hash
    let equal = String.equal
  end

  module E = struct
    type t = V.t * V.t * float
    type label = float
    let label (_, _, l) = l
    let src (src, _, _) = src
    let dst (_, dst, _) = dst
    let create src weight dst = (src, dst, weight)
  end

  let iter_vertex f graph =
    Decl.Graph.get_nodes graph |> List.iter ~f

  let fold_vertex f graph init =
     Decl.Graph.get_nodes graph |> List.fold_right ~f ~init

  let iter_succ f graph node =
    Decl.Graph.get_neighbors graph node |> List.iter ~f

  let iter_succ_e f graph node =
    Decl.Graph.get_edges graph node |>
    List.iter ~f:(fun edge ->
        let nei = Decl.Graph.get_edge_dst edge in
        let w = Decl.Graph.get_edge_weight edge in
        f (node, nei, w))

  let fold_edges_e f graph init =
    Decl.Graph.get_nodes graph |>
    List.fold_right ~init ~f:(fun n acc ->
        Decl.Graph.get_edges graph n |>
        List.fold_right ~init:acc
          ~f:(fun edge acc ->
              let nei = Decl.Graph.get_edge_dst edge in
              let w = Decl.Graph.get_edge_weight edge in
              f (n, nei, w) acc))

  let nb_vertex graph = Decl.Graph.get_nodes graph |> List.length
end

module W = struct
  type edge = G.E.t
  type t = float
  let weight (_, _, w) = w
  let compare = Float.compare
  let add = Float.add
  let zero = Float.zero
  let sub = Float.sub
end

let run_dijkstra graph src dst =
  let module Dijkstra = OcamlGraph.Path.Dijkstra(G)(W) in
  let (edge_list, dist) = Dijkstra.shortest_path graph src dst in
  let path = List.map ~f:(fun (src, _, _) -> src) edge_list in
  (dist, path)

let run_johnson graph =
  let module Johnson = OcamlGraph.Path.Johnson(G)(W) in
  let dist_table = Johnson.all_pairs_shortest_paths graph in
  Johnson.HVV.fold (fun (n1, n2) w acc -> (n1, n2, w) :: acc) dist_table []
