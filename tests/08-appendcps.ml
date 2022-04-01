let rec append_cps l l' k =
  match l with
  | [] -> k l'
  | x :: xs -> append_cps xs l' (function res -> k (x :: res))

let _ =
  if (append_cps [1;2;3] [4;5;6] (function x -> x) = [1;2;3;4;5;6]) then
    print_string "ok"
  else
    print_string "ko"
