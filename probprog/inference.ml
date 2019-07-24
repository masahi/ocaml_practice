open Dist
module R = Stats.RandomSampling

let sample_prim: type a. a Dist.primitive -> a = function
  | Normal(mu, sigma) -> R.normal mu sigma
  | Bernoulli(p) -> R.bernoulli p
  | UniformD(choices) -> R.uniform_discrete choices
  | Categorical(weighted_choices) -> R.categorical weighted_choices

let rec sample_dist: type a. a Dist.t -> a = function
  | Return(x) -> x
  | Bind(d, f) ->
     let x = sample_dist d in
     sample_dist (f x)
  | Primitive(p) -> sample_prim p
  | Conditional(_) -> assert false

let sample_dist_tailcall: type a. a Dist.t -> a = fun d ->
  let rec helper: type b. b Dist.t -> (b -> a) -> a
    = fun d k -> match d with
    | Return(x) -> k x
    | Bind(d, f) ->
       helper d (fun x ->
          helper (f x) k) (* stack overflow at (f x)*)
    | Primitive(p) -> k (sample_prim p)
    | Conditional(_) -> assert false
  in
  helper d (fun x -> x)

let rec prior: type a. a Dist.t -> (a * prob) Dist.t = fun d ->
  let open Dist.Let_Syntax in
  match d with
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

let prior_tailcall: type a. a Dist.t -> (a * prob) Dist.t = fun d ->
  let open Dist.Let_Syntax in
  let rec helper: type b. b Dist.t -> ((b * prob) Dist.t -> (a * prob) Dist.t) -> (a * prob) Dist.t
    = fun d k -> match d with
    | Conditional(likelihood, d) ->
       helper d (fun dist ->
           let* (x, s) = dist in
           k (return (x, (s *. (likelihood x)))))
    | Bind(d, f) ->
       helper d (fun dist ->
           let* (x, s) = dist in
           let* y = f x in
           k (return (y, s)))
    | _ as d ->
       let* x = d in
       k (return (x, 1.0))
  in
  helper d (fun dist -> dist)

let mh: type a. int -> a Dist.t -> a list Dist.t = fun num_iter d ->
  let proposal = prior_tailcall d in
  let open Dist.Let_Syntax in
  let rec get_valid_proposal prop_dist =
    let* (prop, prob) = prop_dist in
    if prob > 0.000001 then return (prop, prob)
    else get_valid_proposal prop_dist
  in
  let rec iterate: int -> (a * prob) list Dist.t -> (a * prob) list Dist.t = fun i dist ->
    if i = 0 then dist
    else
      let next_dist =
        let* (prop, prob_prop) = get_valid_proposal proposal in
        let* current_samples = dist in
        let (current, current_prob) = Base.List.hd_exn current_samples in
        let accept_prob = Float.min 1.0 (prob_prop /. current_prob) in
        (* Printf.printf "prob_prop %.20f, current_prob %.20f, accept prob: %.20f\n" prob_prop current_prob accept_prob; *)
        let* accept = bernoulli accept_prob in
        let next = if accept then (prop, prob_prop) else (current, current_prob) in
        return (next :: current_samples)
      in
      iterate (i - 1) next_dist
  in
  let init_dist =
    let+ (prop, prob) = get_valid_proposal proposal in
    (* Printf.printf "initial prob %.20f\n" prob; *)
    [(prop, prob)]
  in
  let+ samples = iterate num_iter init_dist in
  List.map fst samples
