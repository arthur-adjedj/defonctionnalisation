let fold_right f l acc =
  let rec fold_right_cps l k =
    match l with
    | [] -> k acc
    | x::xs -> fold_right_cps xs (function acc -> k (f x acc)) in
  fold_right_cps l (function x -> x)

let _ =
  if fold_right (function x -> function acc -> x :: acc) [1;2;3;4] [] = [1;2;3;4] then
    print_string "ok"
  else
    print_string "ko"
