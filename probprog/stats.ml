module RandomSampling = struct
  let normal mu sigma = Owl_base_stats.gaussian_rvs ~mu ~sigma

  let bernoulli p =
    let b = Owl_base_stats.bernoulli_rvs ~p in
    if b > 0.0 then true
    else false

  let uniform_discrete choices =
    let length = List.length choices in
    let index = Owl_stats.uniform_int_rvs ~a:0 ~b:length-1 in
    let open Base in
    List.nth_exn choices index

  let categorical weighted_choices =
    let open Base in
    let (choices, weights) = List.unzip weighted_choices in
    let index =
       Array.of_list weights |> Owl_stats.categorical_rvs
    in
    List.nth_exn choices index
end

module Pdf = struct
  let normal v mu sigma =
    Owl_stats.gaussian_pdf v ~mu ~sigma
end
