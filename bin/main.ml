open Parse

let () = print_endline
           (string_of_expr
              (expr_of_string
                 "if 1 > 2 + 3 then fun x -> x + 2 else if true then 2 else 3"))

