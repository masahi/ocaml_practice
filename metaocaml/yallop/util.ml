open Fft_types

let rec nat_to_int: type n. n nat -> int = function
  | Z -> 0
  | S(n) -> 1 + (nat_to_int n)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
