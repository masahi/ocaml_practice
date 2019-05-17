let rec iota_aux m n acc =
  if m = n then acc
  else
    iota_aux m (n - 1) ((n-1) :: acc)

let iota m n = iota_aux m n []
