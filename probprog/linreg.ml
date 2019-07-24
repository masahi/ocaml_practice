open Dist
module R = Stats.RandomSampling

let gen_data n slope intercept =
  let xs = List.init n (fun i -> float_of_int i) in
  let ys_true = List.map (fun x -> x *. slope +. intercept) xs in
  let ys_obs = List.map (fun y -> y +. (R.normal 0.0 1.0) *. 1.0) ys_true in
  (xs, ys_obs)

let list_to_mat lst =
  let len = List.length lst in
  let arr = Array.of_list lst in
  Owl.Mat.init len 1 (fun i -> arr.(i))

let linear =
  let open Dist.Let_Syntax in
  let* a = std_normal () in
  let* b = std_normal () in
  return (a, b)

let observe (x, y) prior =
  let likelihood (a, b) = Stats.Pdf.normal y (a *. x +. b) 1.0 in
  Conditional(likelihood, prior)

let () =
  let (xs, ys_obs) = gen_data 10 1.3 0.2 in
  Plot.plot "linreg.png" (list_to_mat xs) (list_to_mat ys_obs);
  let points = Base.List.zip_exn xs ys_obs in
  let conditional = List.fold_left (fun dist point -> observe point dist) linear points in
  let open Inference in
  let dist = mh 100000 conditional in
  let samples = sample_dist_tailcall dist in
  let open Base in
  List.iter (List.take samples 10) ~f:(fun (slope, intercept) -> Stdlib.Printf.printf "slope %f, intercept %f\n" slope intercept);
  let (slope, intercept) = List.hd_exn samples in
  List.iter points ~f:(fun (x, y) ->
      let pdf = Stats.Pdf.normal y (slope *. x +. intercept) 1.0 in
      Stdlib.Printf.printf "(x, y): (%f, %f), pdf:%.20f\n" x y pdf)
