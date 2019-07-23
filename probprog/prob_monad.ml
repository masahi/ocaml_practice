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
let (let*) d f = bind d f
let return x = Return(x)
let map d f =
  let* x = d in
  return (f x)
let (let+) d f = map d f

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

let rec prior: type a'. a' Dist.t -> (a' * prob) Dist.t = function
  | Conditional(likelihood, d) ->
     let* (x, s) = prior d in
     return (x, s *. (likelihood x))
  | Bind(d, f) ->
     let* (x, s) = prior d in
     let* y = f x in
     return (y, s)
  | _ as d ->
     let* x = d in
     return (x, 1.0)

let mh: type a'. int -> a' Dist.t -> a' list Dist.t = fun num_iter d ->
  let proposal = prior d in
  let rec iterate: int -> (a' * prob) list Dist.t -> (a' * prob) list Dist.t = fun i dist ->
    if i = 0 then dist
    else
      let next_dist =
        let* (prop, prob_prop) = proposal in
        let* current_samples = dist in
        let (current, current_prob) = Base.List.hd_exn current_samples in
        let accept_prob = Float.min 1.0 (prob_prop /. current_prob) in
        let* accept = Primitive(Bernoulli(accept_prob)) in
        let next = if accept then (prop, prob_prop) else (current, current_prob) in
        return (next :: current_samples)
      in
      iterate (i - 1) next_dist
  in
  let init_dist =
    let+ prop = proposal in
    [prop]
  in
  let+ samples = iterate num_iter init_dist in
  List.map fst samples

let plot_hist ?(bin=10) fname x =
  let open Owl_plplot in
  let h = Plot.create fname in
  Plot.histogram ~h ~bin x;
  Plot.output h

let () =
  let rv =
    let* x = Primitive(Gaussian(0.0, 1.0)) in
    let* y = Primitive(Gaussian(0.0, 1.0)) in
    let* b = Primitive(Bernoulli(0.5)) in
    if b then return (x +. y)
    else return (x -. y)
  in
  let open Owl in
  let samples = Mat.init 1000 1 (fun _ -> sample_dist rv) in
  plot_hist "gauss_sum2.png" samples


let () =
  let prob = [(1, 0.05); (2, 0.1); (3, 0.15); (4, 0.2); (5, 0.2); (6, 0.3)] in
  let loaded_die = Primitive(Categorical(prob)) in
  let sum_dist =
    let* x1 = loaded_die in
    let* x2 = loaded_die in
    return (x1 + x2)
  in
  let open Owl in
  let samples = Mat.init 1000 1 (fun _ -> sample_dist sum_dist |> float_of_int) in
  plot_hist "loaded_die_sum2.png" samples


let sample_sum dist n init binop unary_func =
  let rec helper n acc =
    if n = 0 then return acc
    else
      let* x = dist in
      helper (n - 1) (binop acc (unary_func x))
  in
  helper n init


let () =
  let binomial p n =
    let ber = Primitive(Bernoulli(p)) in
    sample_sum ber n 0 (+) Bool.to_int
  in
  let dist = binomial 0.3 100 in
  let open Owl in
  let samples = Mat.init 1000 1 (fun _ -> sample_dist dist |> float_of_int) in
  plot_hist ~bin:30 "binomial_1000_0.3.png" samples


let () =
  let normal = Primitive(Gaussian(0.0, 1.0)) in
  let chi2 k =
    sample_sum normal k 0.0 (+.) (fun x -> x *. x)
  in
  let student_t df =
    let* z = normal in
    let* v = chi2 df in
    let rv = z *. sqrt ((float_of_int df) /. v) in
    return rv
  in
  let open Owl in
  let dist = student_t 10 in
  let samples = Mat.init 1000 1 (fun _ -> sample_dist dist) in
  plot_hist ~bin:50 "student_t_5.png" samples

let () =
  let data =
    let num = 100 in
    let slope = 2.0 in
    let intercept = 5.0 in
    let xs = List.init num (fun i -> float_of_int i) in
    let ys_true = List.map (fun x -> x *. slope +. intercept) xs in
    let ys_obs = List.map (fun y -> y +. (R.normal 0.0 5.0) *. 5.0) ys_true in
    (xs, ys_obs)
  in
  let list_to_mat lst =
    let len = List.length lst in
    let arr = Array.of_list lst in
    Owl.Mat.init len 1 (fun i -> arr.(i)) in
  let (xs, ys_obs) = data in
  let open Owl_plplot in
  let h = Plot.create "linreg.png" in
  Plot.plot ~h (list_to_mat xs) (list_to_mat ys_obs);
  Plot.output h;
  let normal = Primitive(Gaussian(0.0, 1.0)) in
  let linear =
    let* a = normal in
    let* b = normal in
    return (a, b)
  in
  let observe (x, y) prior =
    let likelihood (a, b) =
      let l = Owl_stats.gaussian_pdf y ~mu:(a *. x +. b) ~sigma:1.0 in
      Printf.printf "(x, y): (%f, %f), (a, b): (%f,%f), mu: %f, l:%.20f \n" x y a b (a *. x +. b) l;
      l
    in
    Conditional(likelihood, prior) in
  let points =
    Base.List.zip_exn xs ys_obs in
  let conditional = List.fold_left (fun dist point -> observe point dist) linear points in
  let proposal = prior conditional in
  let samples = List.init 1000 (fun _ -> sample_dist proposal) in
  let sorted = Base.List.sort samples ~compare:(fun (_, p1) (_, p2) -> Bool.to_int (p1 > p2)) in
  List.iter (fun ((slope, intercept), p) -> Printf.printf "slope %f, intercept %f, prob %.20f\n" slope intercept p) sorted
