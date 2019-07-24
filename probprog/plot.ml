let list_to_mat lst =
  let len = List.length lst in
  let arr = Array.of_list lst in
  Owl.Mat.init len 1 (fun i -> arr.(i))

let plot_hist ?(bin=10) fname x =
  let open Owl_plplot in
  let h = Plot.create fname in
  Plot.histogram ~h ~bin (list_to_mat x);
  Plot.output h

let plot fname x y =
  let open Owl_plplot in
  let h = Plot.create fname in
  Plot.plot ~h (list_to_mat x) (list_to_mat y);
  Plot.output h;
