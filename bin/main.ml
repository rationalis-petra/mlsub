module AST = Codegen.AST

let (>>) f g x = g (f x)

module StrMap = Map.Make(String)

let rec main_loop () = 
  let op = Parse.expr_of_string >>
           AST.expr_of_pexpr >>
           (fun x -> Codegen.codegen_expr x StrMap.empty )>>
           Llvm.dump_value >>
           (fun () -> print_endline "") in
  let line = read_line () in
  if line == "quit" then ()
  else (op line; main_loop ())


let () = main_loop()
