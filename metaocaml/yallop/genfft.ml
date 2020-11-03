open Complex

let split arr =
  let n = Array.length arr in
  let evens = Array.init (n/2) (fun i -> arr.(2 * i)) in
  let odds = Array.init (n/2) (fun i -> arr.(2 * i + 1)) in
  evens, odds

let merge evens odds =
  let half = Array.length evens in
  let n = half * 2 in
  let coeffs = Array.init half (fun i -> (Fft_unstaged.w n i)) in
  let multiplied = Array.map2 (fun y coeff -> mul y coeff) odds coeffs in
  Array.init n (fun i -> if i < half then add evens.(i)  multiplied.(i)
                 else sub evens.(i - half) multiplied.(i - half))

let rec fft input =
  let n = Array.length input in
  if n = 16 then Fft_staged.fft_16 input
  else
    let evens, odds = split input in
    merge (fft evens) (fft odds)

let dft input =
  let w n i j = Fft_unstaged.w n (i * j) in
  let n = Array.length input in
  let zero = { re = 0.0; im = 0.0 } in
  let dot xs ys = Array.fold_left (fun acc (x, y) -> add acc (mul x y))
      zero (Array.map2 (fun x y -> (x, y)) xs ys) in
  Array.init n (fun i ->
      let coeffs = Array.init n (fun j -> (w n i j)) in
      dot input coeffs)

let _ =
  let size = 1024 in
  let input = Array.init size (fun _ -> {re=Random.float 1.0; im=Random.float 1.0}) in
  let res = fft input in
  let ref_res = dft input in
  Array.iter2 (fun {re=re1;im=im1} {re=re2;im=im2} -> Printf.printf "Staged: (%f, %f), Ref: (%f, %f)\n" re1 im1 re2 im2) res ref_res
