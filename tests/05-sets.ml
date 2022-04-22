let empty = function x -> false

let insert x s = (function y -> (x = y) || (s y))

let _ =
  let s = insert 2 (insert 1 empty) in
  let _ = print_string (if s 1 then "ok" else "ko") in
  let _ = print_string (if s 2 then "ok" else "ko") in
  let _ = print_string (if not (s 3) then "ok" else "ko") in
  print_newline ()

let _ =
  let s = insert "a" (insert "b" empty) in
  let _ = print_string (if s "a" then "ok" else "ko") in
  let _ = print_string (if s "b" then "ok" else "ko") in
  let _ = print_string (if not (s "c") then "ok" else "ko") in
  print_newline ()
