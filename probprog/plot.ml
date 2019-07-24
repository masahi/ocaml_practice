let plot_hist ?(bin=10) fname x =
  let open Owl_plplot in
  let h = Plot.create fname in
  Plot.histogram ~h ~bin x;
  Plot.output h

let plot fname x y =
  let open Owl_plplot in
  let h = Plot.create fname in
  Plot.plot ~h x y;
  Plot.output h;
