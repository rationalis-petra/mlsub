let (>>) f g x = g (f x)

module StrMap = Map.Make(String)

exception Err of string

(* The arguments are parsed in directly  *)
let rec compile mode in_filename out_filename format rcd_impl =
  Codegen.set_record_impl rcd_impl;
  let parse = 
    file_to_str >>
    Parse.expr_of_string >>
    (fun x -> print_endline (Parse.string_of_expr x))
  in
  let type_progn = 
    file_to_str >>
    Parse.expr_of_string >>
    Type.infer_type >>
    (fun t -> print_endline (Type.string_of_type t)) in
  (* let typecheck =  *)
  let compile =
    file_to_str >>
    Parse.expr_of_string >>
      (fun f -> (Type.infer_type >>
                   Type.string_of_type >>
                   (fun str -> "Program has type: " ^ str) >>
                   print_endline) f; f) >>
    Codegen.expr_of_pexpr >>
    Codegen.codegen_program >>
           (fun m -> match format with
                     | "bitcode" ->
                        ignore (Llvm_bitwriter.write_bitcode_file m out_filename)
                     | "ir" ->
                        str_to_file (Llvm.string_of_llmodule m) out_filename
                     | _ -> raise (Err ("unrecognized output format: " ^ format))) in
  match mode with
  | "parse" -> parse in_filename
  | "type" -> type_progn in_filename
  | "compile" -> compile in_filename
  | _ -> print_endline ("err: invalid argument to mode: " ^ mode)

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

let rec_impl = 
  let doc =
    "The name of the record implementation mechanism to use: can be hashmap or" ^
            "global. Default value is global" in
  Arg.(value & opt string "global" & info ["r"; "record"] ~docv:"RECORD" ~doc)

let mode = 
  let doc = "The desired action to perform: parse, typecheck or compile " ^
            "Default is compile" in
  Arg.(value & opt string "compile" & info ["m"; "mode"] ~docv:"MODE" ~doc)

let compile_t = Term.(const compile $ mode  $ in_filename $ out_filename $ out_format$ rec_impl)


let info =
  let doc = "Compile a program in the MLSub language" in
  Term.info "mlsub" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (compile_t, info)
