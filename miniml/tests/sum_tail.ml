let rec sum x =
    fun a -> if x = 0 then a else sum (x + (-1)) (x + a)
  in
  sum 3 0
