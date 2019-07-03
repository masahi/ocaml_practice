let square x = x *. x

let rec power n x =
  if n = 0 then 1.
  else if n mod 2 = 0 then square (power (n/2) x)
  else x *. (power (n-1) x)

let rec spower n x =
  if n = 0 then .<1.>.
  else if n mod 2 = 0 then
    .<let x' = .~x *. .~x in
    .~(spower (n/2) .<x'>.)>.
  else .<.~x *. .~(spower (n-1) x)>.

let spowern n = .<fun x -> .~(spower n .<x>.)>.

let _ =
  let open Core_bench in
  let go n x =
    let power_staged = spowern n in
    Codelib.print_code Format.std_formatter power_staged; print_newline ();
    let spower_fn = Runnative.run power_staged in
    Printf.printf "%.10f\n" (spower_fn x);
    Printf.printf "%.10f\n" (power n x);
    [Bench.Test.create ~name:"unstaged" (fun () -> ignore(power n x));
     Bench.Test.create ~name:"staged" (fun () -> ignore(spower_fn x))]
    |> Bench.bench
  in
  go 100000 1.00001

(* .<
 * fun x_1 ->
 *   let x'_2 = x_1 *. x_1 in
 *   let x'_3 = x'_2 *. x'_2 in
 *   let x'_4 = x'_3 *. x'_3 in
 *   let x'_5 = x'_4 *. x'_4 in
 *   let x'_6 = x'_5 *. x'_5 in
 *   x'_6 *.
 *     (let x'_7 = x'_6 *. x'_6 in
 *      let x'_8 = x'_7 *. x'_7 in
 *      x'_8 *.
 *        (let x'_9 = x'_8 *. x'_8 in
 *         let x'_10 = x'_9 *. x'_9 in
 *         x'_10 *.
 *           (let x'_11 = x'_10 *. x'_10 in
 *            x'_11 *.
 *              (let x'_12 = x'_11 *. x'_11 in
 *               let x'_13 = x'_12 *. x'_12 in
 *               let x'_14 = x'_13 *. x'_13 in
 *               let x'_15 = x'_14 *. x'_14 in
 *               let x'_16 = x'_15 *. x'_15 in
 *               x'_16 *. (let x'_17 = x'_16 *. x'_16 in x'_17 *. 1.)))))>.
 * 2.7182682372
 * 2.7182682372
 * Estimated testing time 20s (2 benchmarks x 10s). Change using '-quota'.
 * ┌──────────┬──────────┬─────────┬─────────┐
 * │ Name     │ Time/Run │ mWd/Run │ mGC/Run │
 * ├──────────┼──────────┼─────────┼─────────┤
 * │ unstaged │  98.06ns │  44.00w │ 0.17e-3 │
 * │ staged   │   8.54ns │   2.00w │         │
 * └──────────┴──────────┴─────────┴─────────┘ *)
