open Parse

let () = print_endline (string_of_expr (expr_of_string "(fun x -> x + 2) 3"))

