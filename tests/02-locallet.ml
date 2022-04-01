let f x b =
  let y = x + 1 in
  let z = if b then y else - y in
  z

let _ = print_int (f (-41) false)
