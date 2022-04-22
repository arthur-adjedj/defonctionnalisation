let succ x = x + 1
let hello s = "hello " ^ s
let twice f = function x -> f (f x)
let _ = print_int (twice succ 40)
let _ = print_string (twice hello "world")