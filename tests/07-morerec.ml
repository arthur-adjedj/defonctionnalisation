let rec print_tree j k =
  let rec print_line n =
    if n = 0 then print_newline ()
    else
      let _ = print_string "*" in
      print_line (n-1)
  in
  if k = 0 then ()
  else if j = k then print_line k
  else (
    let _ = print_line j in
    let _ = print_tree (j+1) k in
    print_line j
  )

let _ = print_tree 1 5
