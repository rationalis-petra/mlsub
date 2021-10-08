open Parse

let () = print_endline (string_of_expr (parse_string "(fun x -> x + 2) 3"))

