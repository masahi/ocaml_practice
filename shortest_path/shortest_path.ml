open Core_kernel
open Types

module DistMap = Treemap.Make(String)
module Heap = Core_kernel.Heap

let build_graph edge_list =
  let update_edge_list graph (src, {dst; weight}) = Graph.add_edge graph src dst weight in
  List.fold_left ~f:(fun acc edge -> update_edge_list acc edge) ~init:Graph.empty edge_list

let shortest_path graph src dst =
  let update_edge heap dist_map (neighbor, weight) current_dist parents =
    let new_dist = current_dist +. weight in
    match DistMap.lookup dist_map neighbor with
    | Some (d, _) when d <= new_dist -> (heap, dist_map)
    | _ ->
      let new_dist_map = DistMap.insert dist_map neighbor (new_dist, parents) in
      Heap.add heap (neighbor, new_dist);
      (heap, new_dist_map) in
  let rec loop heap dist_map =
    match Heap.pop heap with
    | None -> (heap, dist_map)
    | Some (n, dist) ->
      let edge_list = Graph.get_edges graph n in
      let parents =
        match DistMap.lookup dist_map n with
        | None -> []
        | Some (_, parents) -> parents in
      let new_heap, new_dist_map =
        List.fold_left ~f:(fun (new_heap, new_dist_map) edge ->
            update_edge new_heap new_dist_map edge dist (n :: parents))
          ~init:(heap, dist_map) edge_list in
      loop new_heap new_dist_map
  in
  let cmp (_, dist1) (_, dist2) = Float.compare dist1 dist2 in
  let heap = Heap.create ~cmp () in
  Heap.add heap (src, 0.0);
  let init_dist_map = DistMap.insert DistMap.empty src (0.0, []) in
  let _, final_dist_map = loop heap init_dist_map in
  match DistMap.lookup final_dist_map dst with
  | None -> (Float.infinity, [])
  | Some (d, path) -> (d, List.rev path)

let run src dst edge_list =
  let graph = build_graph edge_list in
  print_string "Graph loaded.\n";
  Out_channel.flush stdout;
  let (dist, path) = shortest_path graph src dst in
  Printf.printf "Distance: %f\n" dist;
  Printf.printf "Path: ";
  List.iter ~f:(fun n -> Printf.printf " %s -> " n) path;
  Printf.printf "%s\n" dst

let run_dimacs () =
  let (src, dst) = ("500", "9998") in
  Dimacs.load_edge_list "USA-road-d.NY.gr" |> run src dst

let () =
  run_dimacs ()
