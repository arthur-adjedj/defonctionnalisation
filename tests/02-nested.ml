let f x =
  let g y = y + 1 in
  g (g x)

let _ = print_int (f 40)
