let x = 1 in
  let f = fun y -> x + y in
    let x = 2 in
      f (x + 3)
