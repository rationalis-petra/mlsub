open Parse

let () = print_endline
           (string_of_expr
              (expr_of_string
                 "{ x = 2, y = 3 } "))

