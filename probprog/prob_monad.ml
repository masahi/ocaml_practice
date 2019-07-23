module Dist = struct
  type prob = float

  type _ primitive =
    | Gaussian : float * float -> float primitive
    | Bernoulli : float -> bool primitive
    | UniformD : 'a list -> 'a primitive
    | Categorical: ('a * prob) list -> 'a primitive

  type _ t =
    | Return : 'a -> 'a t
    | Bind: 'b t * ('b -> 'a t) -> 'a t
    | Primitive : 'a primitive -> 'a t
    | Conditional : ('a -> prob) * 'a t -> 'a t
end

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

open Dist
module R = RandomSampling

let bind d f = Bind(d, f)
let (>>=) = bind
let (let*) = (>>=)
let return x = Return(x)

let sample_prim: type a'. a' Dist.primitive -> a' = function
  | Gaussian(mu, sigma) -> R.normal mu sigma
  | Bernoulli(p) -> R.bernoulli p
  | UniformD(choices) -> R.uniform_discrete choices
  | Categorical(weighted_choices) -> R.categorical weighted_choices

let rec sample_dist: type a'. a' Dist.t -> a' = function
  | Return(x) -> x
  | Bind(d, f) ->
     let x = sample_dist d in
     sample_dist (f x)
  | Primitive(p) -> sample_prim p
  | Conditional(_) -> assert false

let () =
  let rv =
    let* x = Primitive(Gaussian(0.0, 1.0)) in
    let* y = Primitive(Gaussian(0.0, 1.0)) in
    let* b = Primitive(Bernoulli(0.5)) in
    if b then return (x +. y)
    else return (x -. y)
  in
  let samples = Array.init 1000 (fun _ -> sample_dist rv) in
  Array.iter (fun x -> Printf.printf "%f\n" x) samples

let () =
  let prob = [(1, 0.05); (2, 0.1); (3, 0.15); (4, 0.2); (5, 0.2); (6, 0.3)] in
  let loaded_die = Primitive(Categorical(prob)) in
  let sum_dist =
    let* x1 = loaded_die in
    let* x2 = loaded_die in
    return (x1 + x2)
  in
  let samples = Array.init 1000 (fun _ -> sample_dist sum_dist) in
  Array.iter (fun x -> Printf.printf "%d\n" x) samples
