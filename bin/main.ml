open Parse

let () = print_endline
           (string_of_expr
              (expr_of_string
                 "if true then 10 else 3"))

