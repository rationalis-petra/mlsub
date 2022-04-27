open Llvm
open Data


(* compiler switches *)
type flag_type = {
    assert_valid : bool ref; (* whether to use LLVM static analysis *)
    record_impl : string ref (* which implementation of record polymorphism to *)
  }
let flags : flag_type =
  { assert_valid = ref true;
    record_impl = ref "global";
  }



(* global variables, some store the results of analysis; others contain *)
(* potentially architecture-specific information *)
let ptr_size = 8
let universal_record : (int StrMap.t) ref = ref StrMap.empty
let universal_accesses : (llvalue StrMap.t) ref = ref StrMap.empty

(* Initialisation *)
(* context *)
let context = global_context ()
(* module: create the module and set the target triple (x86) *)
let the_module = create_module context "MLSub"
let () = set_target_triple "x86_64-pc-linux-gnu" the_module
let builder = builder context

(* CODEGEN TYPES *)
(* Prime type: cast to get other types *)
let bool_type = i1_type context
let int_type = i64_type context
let int_ptr_type = pointer_type int_type

(* Closures (lambdas) *)
let closure_fn_type = function_type int_type [|int_type; int_ptr_type|]
let closure_fn_ptr_type = pointer_type closure_fn_type

exception CodeGenError of string
let err msg = raise (CodeGenError msg)



(* The String Map is the module used for a symbol-table *)

(* The set of used function names *)
let func_names = ref StrSet.empty
let lambda_counter = ref 0

(* Lambdas are converted to global functions! hence, we need to make sure that *)
(* we generate unique names for them *)
let gen_unique_name () = 
  let name = ref ("lambda" ^ string_of_int !lambda_counter) in
  while StrSet.mem !name !func_names do
    lambda_counter := !lambda_counter + 1;
    name := "lambda" ^ string_of_int !lambda_counter
  done;
   func_names := StrSet.add (!name) (!func_names);
   !name







(* Much like in C/C++, there are functions declarations (in LLVM: prototypes)
 * that describe a function, e.g. i64 add (i64: a, i64: b). The function 
 * codegen_proto function is used to generate these prototypes given a name and 
 * a list of (argname, type) pairs. All functions have return type int_type *)

let codegen_proto (name, args) = 
  let type_arr = Array.map (fun (_, y) -> y) args in
  let ft = function_type int_type type_arr in
  let f =
    match lookup_function name the_module with
    | None -> declare_function name ft the_module
    (* If 'f' conflicted, there was already something named 'name'. If it
     * has a body, don't allow redefinition or reextern. *)
    | Some f ->
       (* If 'f' already has a body, reject this. *)
       if Array.length (basic_blocks f) == 0 then () else
         raise (CodeGenError ("redefinition of function: " ^ name));

       (* If 'f' took a different number of arguments, reject. *)
       if Array.length (params f) == Array.length args then () else
         raise (CodeGenError "redefinition of function with different # args");
       f
  in
  f
