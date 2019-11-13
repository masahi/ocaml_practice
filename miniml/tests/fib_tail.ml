let rec fib n =
  fun x -> fun y ->
     if n = 0 then x
     else fib (n-1) y (x+y)
in
fib 10 0 1
