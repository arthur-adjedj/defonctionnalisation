let tt f g x = f x
let ff f g x = g x
let ifte b f g = b f g

let _ =
  let ok = function _ -> print_string "ok" in
  let ko = function _ -> print_string "ko" in
  let _ = ifte tt ok ko () in
  let _ = ifte ff ko ok () in
  print_newline ()
