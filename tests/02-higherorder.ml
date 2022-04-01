let succ x = x + 1
let twice f = function x -> f (f x)

let _ = print_int (twice succ 40)
