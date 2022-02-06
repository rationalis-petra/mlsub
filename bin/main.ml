let (>>) f g x = g (f x)

module StrMap = Map.Make(String)

exception Err of string

(* Notes to self *)
(* Llvm.dump_value to convert llvalue -> string *)
(* Llvm.dump_module to convert llvalue -> string *)

(* Use the Command Module for CLI parsing *)

let rec compile in_filename out_filename format =
  let compile =
    file_to_str >>
    Parse.expr_of_string >>
    (fun x -> print_endline (Parse.string_of_expr x); x) >>
    Codegen.expr_of_pexpr >>
    Codegen.codegen_program >>
           (fun m -> match format with
                     | "bitcode" ->
                        ignore (Llvm_bitwriter.write_bitcode_file m out_filename)
                     | "ir" ->
                        str_to_file (Llvm.string_of_llmodule m) out_filename
                     | _ -> raise (Err ("unrecognized output format: " ^ format))) in
  compile in_filename

and file_to_str filename = 
  let file = open_in filename in
  let str = really_input_string file (in_channel_length file) in
  close_in file;
  str

and str_to_file contents filename  =
  let file = open_out filename in
  Stdlib.output_string file contents;
  close_out file

open Cmdliner

let in_filename = 
  let doc = "The name of the MLSub file to compile" in
  let env = Arg.env_var "SOURCE_FILENAME" ~doc in
  Arg.(value & pos 0 string "test.mls" & info [] ~env ~docv:"SOURCE" ~doc)

let out_filename = 
  let doc = "The name of the output file to compile to" in
  Arg.(value & opt string "test.bc" & info ["o"; "output"] ~docv:"OUTFILE" ~doc)

let out_format = 
  let doc = "The name of format to compile to: can be IR," ^
            "Bitcode, Assembly or Binary" in
  Arg.(value & opt string "bitcode" & info ["f"; "format"] ~docv:"FORMAT" ~doc)

let compile_t = Term.(const compile $ in_filename $ out_filename $ out_format)


let info =
  let doc = "Compile a program in the MLSub language" in
  Term.info "mlsub" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (compile_t, info)
