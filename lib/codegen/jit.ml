(* open Llvm *)

(* module ExecutionEngine = Llvm_executionengine *)

let init_the_module () =
  (* JIT SETUP *)
  
  (* let the_execution_engine = ExecutionEngine.create Compile.top_module in *)
  (* let the_fpm = PassManager.create_function Compile.top_module in *)
  
  (* DataLayout.add (ExecutionEngine.data_layout.add *)
  (*                 the_execution_engine) the_fpm; *)


  let _ = Compile.codegen_proto ("puti", [|("integer", Compile.int_type)|]) in
  let _ = Compile.codegen_proto ("get_field", [|("table", Compile.int_type);
                                                (* TODO: change to string type! *)
                                                ("field_name", Compile.int_type)|]) in
  ()
