open Parse
open Type


let (<<) f g x = f (g x)
let (>>) f g x = g (f x)

let () =
  let op = expr_of_string >> infer_type >> string_of_type >> print_endline in
  op "if 1 > 2 + 3 then fun x -> x + 2 else if true then 2 else 3"



