open Metro

module DistMap = Map.Make(String)
module Heap = Core_kernel.Heap

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


let lookup_name k name_list =
  let {romaji; _} = List.find (fun {kanji; _} -> kanji = k) name_list in
  romaji

let get_edge_list name_list ekikan_list =
  List.map (fun {kiten; shuten; kyori; _} ->
      let src_name = lookup_name kiten name_list in
      let dst_name = lookup_name shuten name_list in
      (src_name, {dst = dst_name; weight = kyori})) ekikan_list

let get_node_list name_list =
  List.map (fun {romaji; _} -> romaji) name_list

let build_graph edge_list =
  let reverse_edge (src, {dst; weight}) = (dst, {dst = src; weight}) in
  let update_edge_list graph (src, {dst; weight}) = Graph.add_edge graph src dst weight in
  let g = List.fold_left (fun acc edge -> update_edge_list acc edge) Graph.empty edge_list in
  List.fold_left (fun acc edge -> update_edge_list acc (reverse_edge edge)) g edge_list 

let shortest_path graph src dst =
  let update_edge heap dist_map (neighbor, weight) current_dist parents =
    let new_dist = current_dist +. weight in
    match DistMap.find_opt neighbor dist_map with
    | Some (d, _) when d < new_dist -> (heap, dist_map)
    | _ ->
      let new_dist_map = DistMap.add neighbor (new_dist, parents) dist_map in
      Heap.add heap (neighbor, new_dist);
      (heap, new_dist_map) in
  let rec loop heap dist_map =
    match Heap.pop heap with
    | None -> (heap, dist_map)
    | Some (n, dist) ->
      let edge_list = Graph.get_edges graph n in
      let (_, parents) = DistMap.find n dist_map in
      let new_heap, new_dist_map =
        List.fold_left (fun (new_heap, new_dist_map) edge ->
            update_edge new_heap new_dist_map edge dist (n :: parents))
          (heap, dist_map) edge_list in
      loop new_heap new_dist_map
  in
  let cmp (_, dist1) (_, dist2) = Float.compare dist1 dist2 in
  let heap = Heap.create ~cmp () in
  Heap.add heap (src, 0.0);
  let init_dist_map = DistMap.singleton src (0.0, []) in
  let _, final_dist_map = loop heap init_dist_map in
  match DistMap.find_opt dst final_dist_map with
  | None -> assert false
  | Some (d, path) -> (d, List.rev path)

(* let print_edge {src; dst; weight} =
 *   Printf.printf "src=%s, dst=%s, weight = %f\n" src dst weight *)

(* let print_graph graph =
 *   Graph.iter (fun node edges ->
 *       Printf.printf "src %s ->" node;
 *       List.iter (fun (dst, weight) -> Printf.printf " (%s, %f), " dst weight) edges;
 *       print_string "\n";) graph *)

let () =
  let edge_list = get_edge_list global_ekimei_list global_ekikan_list in
  let graph = build_graph edge_list in
  let (src, dst) = ("myogadani", "meguro") in
  let (dist, path) = shortest_path graph src dst in
  Printf.printf "Distance: %f\n" dist;
  Printf.printf "Path: ";
  List.iter (fun n -> Printf.printf " %s -> " n) path;
  Printf.printf "%s\n" dst
