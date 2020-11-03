open Complex

let split n arr =
  let evens = Array.init (n/2) (fun i -> arr.(2 * i)) in
  let odds = Array.init (n/2) (fun i -> arr.(2 * i + 1)) in
  evens, odds

let merge n evens odds =
  let half = n / 2 in
  let coeffs = Array.init half (fun i -> (Fft_unstaged.w n i)) in
  let multiplied = Array.map2 (fun y coeff -> mul y coeff) odds coeffs in
  Array.init n (fun i -> if i < half then add evens.(i)  multiplied.(i)
                 else sub evens.(i - half) multiplied.(i - half))

let fft n input =
  if n = 16 then Fft_staged.fft_16 input
  else
    let evens, odds = split n input in
    merge n evens odds

let _ =
  let size = 1024 in
  let input = Array.init size (fun _ -> {re=Random.float 1.0; im=Random.float 1.0}) in
  let res = fft size input in
  Array.iter (fun {re;im} -> Printf.printf "Staged: (%f, %f)\n" re im) res
