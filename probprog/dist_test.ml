open Dist
open Inference
open Dist.Let_Syntax
open Plot

let () =
  let rv =
    let* x = std_normal () in
    let* y = std_normal () in
    let* b = bernoulli 0.5 in
    if b then return (x +. y)
    else return (x -. y)
  in
  let open Owl in
  let samples = Mat.init 1000 1 (fun _ -> sample_dist rv) in
  plot_hist "gauss_sum2.png" samples


let () =
  let prob = [(1, 0.05); (2, 0.1); (3, 0.15); (4, 0.2); (5, 0.2); (6, 0.3)] in
  let loaded_die = categorical prob in
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
    let ber = bernoulli p in
    sample_sum ber n 0 (+) Bool.to_int
  in
  let dist = binomial 0.3 100 in
  let open Owl in
  let samples = Mat.init 1000 1 (fun _ -> sample_dist dist |> float_of_int) in
  plot_hist ~bin:30 "binomial_1000_0.3.png" samples


let () =
  let normal = std_normal () in
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
