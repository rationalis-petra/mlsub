open Parse


let () = print_endline (string_of_expr (eval "2+3*4>=2+3"))
