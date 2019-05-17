open Core_kernel
open Types

let load_edge_list filename =
  In_channel.with_file filename ~f:(fun file ->
      In_channel.fold_lines file ~init:[] ~f:(fun lst line ->
          match String.split line ~on:' ' with
          | ["a" ; src ; dst ; w] ->
            let weight = Float.of_string w in
            (src, {dst; weight}) :: lst
          | _ -> lst))
