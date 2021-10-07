open Parse.Expr

let test = function
    Plus -> "hello"
  | _ -> "goodbye"
 

let () = print_endline (test Minus)
