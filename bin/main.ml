let (>>) f g x = g (f x)

module StrMap = Map.Make(String)

(* Notes to self *)
(* Llvm.dump_value to convert llvalue -> string *)

let rec main_fun () =
  let op = file_to_str >>
           Parse.expr_of_string >>
           Codegen.expr_of_pexpr >>
           Codegen.codegen_program >>
           Llvm.dump_module >>
           (fun () -> print_endline "") in
  if (Array.length Sys.argv) != 2 then 
    (print_endline "Error: MLSub expects exactly one argument.")
  else 
    op (Array.get Sys.argv 1)
    (* let compiled = Codegen.get_module () in *)
    (* str_to_file "a.out" compiled *)


and file_to_str filename = 
  let file = open_in filename in
  let str = really_input_string file (in_channel_length file) in
  close_in file;
  str

and str_to_file filename contents =
  let file = open_out filename in
  Stdlib.output_string file contents;
  close_out file


(* let () = main_loop() *)
(* For temporary/testing purposes *)

(* let () = Codegen.init () *)
(* let () = Codegen.test () *)
let () = main_fun ()
