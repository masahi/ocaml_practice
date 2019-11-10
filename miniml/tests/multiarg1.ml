 let rec sum a =
    fun b -> fun c -> fun d -> fun e -> a + b + c + d + e
  in
  sum 1 2 3 4 5
