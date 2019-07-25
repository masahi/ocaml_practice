module D = Dist
module R = Stats.RandomSampling

let gen_data n slope intercept =
  let xs = List.init n (fun i -> float_of_int i) in
  let ys_true = List.map (fun x -> x *. slope +. intercept) xs in
  let ys_obs = List.map (fun y -> y +. (R.normal 0.0 1.0) *. 0.5) ys_true in
  (xs, ys_obs)

let linear =
  let open Dist.Let_Syntax in
  let* a = D.std_normal () in
  let* b = D.std_normal () in
  D.return (a, b)

let observe (x, y) prior =
  let likelihood (a, b) = Stats.Pdf.normal y (a *. x +. b) 1.0 in
  D.Conditional(likelihood, prior)

let () =
  let (xs, ys_obs) = gen_data 10 0.8 2.3 in
  Plot.plot "linreg.png" xs ys_obs;
  let points = Base.List.zip_exn xs ys_obs in
  let conditional = List.fold_left (fun dist point -> observe point dist) linear points in
  let open Inference in
  let dist = mh 100000 conditional in
  let samples = sample_dist_tailcall dist in
  let open Base in
  List.iter (List.take samples 10) ~f:(fun (slope, intercept) -> Stdlib.Printf.printf "slope %f, intercept %f\n" slope intercept);
  let (slopes, intercepts) = List.unzip samples in
  Plot.plot_hist ~bin:50 "slope.png" slopes;
  Plot.plot_hist ~bin:50 "intercepts.png" intercepts;
  let open Core_bench in
  [Bench.Test.create ~name:"mh 10000" (fun () -> ignore(sample_dist_tailcall dist))]
  |> Bench.bench
