open Fft_types

let split arr =
  let n = Array.length arr in
  let evens = Array.init (n/2) (fun i -> arr.(2 * i)) in
  let odds = Array.init (n/2) (fun i -> arr.(2 * i + 1)) in
  evens, odds

module MakeFFT(D: Domain)(Staged_D: StagedDomain with type t_sta = D.t) = struct
  module FFT_staged = Fft_staged.MakeFFT(Staged_D)

  let merge evens odds =
    let half = Array.length evens in
    let n = half * 2 in
    let coeffs = Array.init half (fun i -> (D.primitive_root_power n i)) in
    let multiplied = Array.map2 (fun y coeff -> D.mul y coeff) odds coeffs in
    Array.init n (fun i -> if i < half then D.add evens.(i)  multiplied.(i)
                   else D.sub evens.(i - half) multiplied.(i - half))

  let fft_unrolled_16 =
    let exponent = S(S(S(S(Z)))) in
    let cde = FFT_staged.mk exponent in
    Runnative.run cde

  let rec fft input =
    let n = Array.length input in
    if n = 16 then fft_unrolled_16 input
    else
      let evens, odds = split input in
      merge (fft evens) (fft odds)

  let dft input =
    let n = Array.length input in
    let dot xs ys = Array.fold_left (fun acc (x, y) -> D.add acc (D.mul x y))
        D.zero (Array.map2 (fun x y -> (x, y)) xs ys) in
    Array.init n (fun i ->
        let coeffs = Array.init n (fun j -> (D.primitive_root_power n (i * j))) in
        dot input coeffs)
end

let _ =
  let size = 1024 in
  let open MakeFFT(ComplexDomain)(ComplexStagedDomain) in
  let open Complex in
  let input = Array.init size (fun _ -> {re=Random.float 1.0; im=Random.float 1.0}) in
  let res = fft input in
  let ref_res = dft input in
  Array.iter2 (fun {re=re1;im=im1} {re=re2;im=im2} -> Printf.printf "Staged: (%f, %f), Ref: (%f, %f)\n" re1 im1 re2 im2) res ref_res
