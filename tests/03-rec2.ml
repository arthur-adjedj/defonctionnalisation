let rec print_strings l =
  match l with
  | [] -> ()
  | s :: l' ->
    let _ = print_string s in
    print_strings l'

let _ = print_strings ["hello"; " "; "world"]
