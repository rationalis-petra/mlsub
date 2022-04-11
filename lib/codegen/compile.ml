open Llvm
open Data
open Data.AST

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
   !name


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

(* Declare external functions: *)
let global_decls = [|("printi", function_type int_type [|int_type|]);
                     ("mk_record", function_type int_type [|int_type|]);
                     ("set_field", function_type (void_type context)
                                     [|int_type; int_type; int_type|]);
                     ("mk_field_access", function_type int_type [|int_type|])|]
let globals = 
  Array.map (fun (name, ft) ->
      declare_function name ft the_module)
    global_decls

let printi = Array.get globals 0
let mk_record = Array.get globals 1
let set_field = Array.get globals 2
let mk_field_access = Array.get globals 3

(* Associate string field-names with integers *)

let get_field = 
  let _field_map : (llvalue StrMap.t) ref = ref StrMap.empty in
  let _counter = ref 0 in
  (fun field ->
    match StrMap.find_opt field !_field_map with
    | Some v -> v
    | None ->
       let i = !_counter in
       _counter := i + 1;
       let lli = const_int int_type i in
       _field_map := StrMap.add field lli !_field_map;
       lli) 



(* CODEGEN FUNCTIONS *)

(* given an expression and a symtable (map of variables to values), generate *)
(* an LLVM IR code segment for an expression *)
let rec codegen_expr expr symtable =
  match expr with
  (* Integer and boolean constants are simple to generate code for, and we also *)
  (* have their name. *)
  | Int i -> const_int int_type i
  | Bool b ->
     let int_of_bool b = if b then 1 else 0 in
     const_int int_type (int_of_bool b)
  | Var name ->
     (match StrMap.find_opt name symtable with
      | Some v -> v
      | None -> err ("unknown variable name: " ^ name))
  (* return a function which takes a hasmap and returns required field *)
  | Access field ->
     (* the mk_field_access function returns a closure which accesses a record *)
     (* codegen_access fields symtable *)
     let field_ll = get_field field in
     build_call mk_field_access [|field_ll|] "accesstmp" builder

  | Record fields ->
     (* Record generation is relatively easy; create the record then call the
     requisite number of record insertions*) 
     (* codegen_record fields symtable *)
     let num_fields = const_int int_type (List.length fields) in
     let record = build_call mk_record [|num_fields|] "recordtmp" builder in
     List.iter (fun (field, value) ->
         let val_ll = codegen_expr value symtable in 
         let key = get_field field in
         ignore (build_call set_field [|record; key; val_ll|] "" builder)
       ) fields;
     record

  | Op (op, e1, e2) -> 
     let lhs = codegen_expr e1 symtable in
     let rhs = codegen_expr e2 symtable in
     (* For boolean operations, we must first cast int -> bool, then bool ->
      * int *)
     let build_bop build_fn lhs rhs name builder = 
       let lhs_as_bool = build_bitcast lhs bool_type "blhstmp" builder in
       let rhs_as_bool = build_bitcast rhs bool_type "brhstmp" builder in
       let out = build_fn lhs_as_bool rhs_as_bool name builder in
       build_bitcast out int_type "outtmp" builder in
     (* For comparison operations, we must cast the result (bool) to int  *)
     let build_intcmp cmptype lhs rhs name builder = 
       let out = build_icmp cmptype lhs rhs name builder in
       build_bitcast out int_type "outtmp" builder in
     (match op with
      | Add -> build_add  lhs rhs "addtmp" builder
      | Sub -> build_sub  lhs rhs "subtmp" builder
      | Mul -> build_mul  lhs rhs "multmp" builder
      | Div -> build_sdiv lhs rhs "divtmp" builder
      | Gre -> build_intcmp Icmp.Sge lhs rhs "gretmp" builder
      | Eql -> build_intcmp Icmp.Eq lhs rhs "eqltmp" builder
      | And -> build_bop build_and lhs rhs "andtmp" builder
      | Or  -> build_bop build_or lhs rhs "ortmp"  builder)

  | Let (var, body, var_expr) ->
     let new_symtable = StrMap.add var (codegen_expr var_expr symtable)
                          symtable in
     let body_val = codegen_expr body new_symtable in
     body_val


  | Fun (var, expr) ->
     (* Most of the code here lives in the codegen_closure helper function. This
      * simply generates a unique name for the global function which is used in
      * the lambda *)
     let num = (!lambda_counter) in
     (lambda_counter := num + 1;
      let name = gen_unique_name () in
      let func = codegen_closure name var expr symtable in
      func) 

  | Apply (e1, e2) ->
     (* In order to call a function, we need to: 
      * 1. Cast the value to an int*, call it λ
      * 2. Dereference to get x, then cast x to int fnc(int int)
      * 3. Add 1 to get the environment (second argument to fnc) 
      * 4. Apply to the function to the arguments *)

     let callee = codegen_expr e1 symtable in

     (* 1 *)
     let lambda = build_inttoptr callee int_ptr_type "closure" builder in

     (* 2 *)
     let func_int = build_load lambda "func_int" builder in
     let func = build_inttoptr func_int closure_fn_ptr_type "func" builder in

     (* 3 *)
     let env_int = build_add callee (const_int int_type ptr_size) "env_int" builder in
     let env_ptr = build_inttoptr env_int int_ptr_type "env_ptr" builder in

     (* 4 *)
     let arg = codegen_expr e2 symtable in

     let args = [|arg; env_ptr|] in
     build_call func args "calltmp" builder



  | If (cond, e1, e2) ->
     let cond_code = codegen_expr cond symtable in
     (* The output is in the form of an int64; we need to cast to bool *)
     let cond_bool = build_bitcast cond_code bool_type "condtmp" builder in 
     let start_bb = insertion_block builder in
     let the_function = block_parent start_bb in
     let then_bb = append_block context "then" the_function in
     position_at_end then_bb builder;
     let then_code = codegen_expr e1 symtable in
     let new_then_bb = insertion_block builder in
     (* emit 'else' value *)
     let else_bb = append_block context "else" the_function in
     position_at_end else_bb builder;
     let else_code = codegen_expr e2 symtable in
     let new_else_bb = insertion_block builder in
     (* Emit merge block?? *)
     let merge_bb = append_block context "ifcont" the_function in
     position_at_end merge_bb builder;
     let incoming = [(then_code, new_then_bb); (else_code, new_else_bb)] in
     let phi = build_phi incoming "iftmp" builder in
     (* return to the start block to add conditional branch *)
     position_at_end start_bb builder;
     ignore (build_cond_br cond_bool then_bb else_bb builder);
     (* Set a unconditional branch at the end of the 'then' block and the
      * 'else' block to the 'merge' block. *)
     position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
     position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

     (* Finally, set the builder to the end of the merge block. *)
     position_at_end merge_bb builder;

     phi



(* Much like in C/C++, we have functions declarations (in LLVM: prototypes)
 * that describe a function, e.g. i64 add (i64: a, i64: b). The function 
 * codegen_proto function is used to generate these prototypes given a name and 
 * a list of (argname, type) pairs. All functions have return type int_type *)

and codegen_proto (name, args) = 
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
         raise (CodeGenError "redefinition of function");

       (* If 'f' took a different number of arguments, reject. *)
       if Array.length (params f) == Array.length args then () else
         raise (CodeGenError "redefinition of function with different # args");
       f
  in
  f

(* Generate code for a TOPLEVEL function. This is not for lambda functions! *)

(* codegen_func generates a new toplevel function with a given argument-list *)
(* and body*)
and codegen_func ((name, args), body) = 
  (* Generate a function handle (prototype) *)
  let func_handle = codegen_proto (name, args) in
  (* Create a new basic block to start insertion into. *)
  let bb = append_block context "entry" func_handle in
  position_at_end bb builder;
  try
    let symtable =
      StrMap.of_seq (Array.to_seq 
                       (Util.arr_zip
                          (Array.map (fun (x, _) -> x) args)
                          (params func_handle))) in 
    let ret_val = codegen_expr body symtable in

    (* Finish off the function. *)
    let _ = build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    (if !(flags.assert_valid) then
       Llvm_analysis.assert_valid_function func_handle
     else
       ());

    func_handle
  with e ->
    delete_function func_handle;
    raise e

(* Codegen closure will:
 * 1. Generate an array representing the environment
 * 2. Generate a function which takes an argument and a context, corresponding
 *    to the provided body
 * 3. Return a pointer to a (f, r) pair, where f is the generated function and r
 * is the generated record *)

and codegen_closure name argname body symtable = 
  (* Remember where in the code we were (relevant later) *)
  let current_bb = insertion_block builder in

  (* Discover which variables the sub-expression uses:  *)
  let rec get_free_vars body = 
    match body with
    | Var v -> StrSet.singleton v
    | Record lst ->
       List.fold_right
         (fun (_, e1) vs ->
           StrSet.union (get_free_vars e1) vs) lst StrSet.empty
    | Op (_, e1, e2) ->
       StrSet.union (get_free_vars e1) (get_free_vars e2)
    | Fun (v, bdy) ->
       StrSet.remove v (get_free_vars bdy)
    | Let (v, e1, e2) -> 
       StrSet.remove v
         (StrSet.union (get_free_vars e1) (get_free_vars e2))
    | Apply (e1, e2) ->
       StrSet.union (get_free_vars e1) (get_free_vars e2)
    | _ -> StrSet.empty in

  (* The variables we want to place in a context *)
  (* Note that the argument of the function may be in the set of free
   * variables, so we remove it *)
  let ctx_vars = Array.of_seq
                   (StrSet.to_seq (StrSet.remove argname (get_free_vars body))) in

  (* Build the array representing the environment: first allocate, then insert
   * variables *)
  let closure_ptr = build_array_malloc int_type
                 (const_int int_type (Array.length ctx_vars))
                 "closure_env" builder in
  let closure_as_int = build_ptrtoint closure_ptr int_type "closure_as_int"
                         builder in

  Array.iteri (fun _idx _var ->
      let idx = codegen_expr (Int (_idx + ptr_size)) symtable in
      let var = codegen_expr (Var _var) symtable in
      let closure_idx = build_add closure_as_int idx
                          "closure_idx" builder in
      let closure_idx_ptr = build_inttoptr closure_idx int_ptr_type
                              "tmpaddr" builder in
      ignore (build_store var closure_idx_ptr builder))
    ctx_vars;

  (* First generate the function handle *)
  let func_handle = codegen_proto
                      (name, [|(argname, int_type); ("env", int_ptr_type)|]) in

  (* Create a new basic block to start insertion into. *)
  let bb = append_block context "entry" func_handle in
  position_at_end bb builder;

  (* Variable to hold the second argument *)
  let closure_env = Array.get (params func_handle) 1 in

  (* Now, generate the new mapping of variables to values as looking up elements
   * in the array. The /second/ argument must first be cast into the array type *)
  let new_symtable1 =
    let ptrint = build_ptrtoint closure_env int_type "envint" builder in
    Array.fold_right 
      (fun (varname, llvalue) map -> StrMap.add varname llvalue map)
      (Array.mapi (fun idx varname ->
           (varname,
            (* cast to int -> add appropriate idx -> cast back to ptr -> store *)
            let ptridx = build_add ptrint (const_int int_type (idx * ptr_size))
                           "envintx" builder in
            let ptr = build_inttoptr ptridx int_ptr_type "ptr" builder in
            build_load ptr "envtmp" builder))
         ctx_vars)
      StrMap.empty in

  (* Map the function argument name to the corresponding parameter in the
   * symbol table. *)
  let new_symtable2 =
    StrMap.add argname (Array.get (params func_handle) 0) new_symtable1
  in

  (* Now, generate the function body + return *)
  let compiled_body = codegen_expr body new_symtable2 in
  ignore (build_ret compiled_body builder);

  (if !(flags.assert_valid) then
     Llvm_analysis.assert_valid_function func_handle
   else
     ());

  (* Switch back to generating code in the original basic block *)
  position_at_end current_bb builder;

  (* Store the function in the closure's memory *)
  let func_as_int = build_ptrtoint func_handle int_type "func_as_int" builder in
  ignore (build_store func_as_int closure_ptr builder);
  build_ptrtoint closure_ptr int_type "closure_as_int" builder


and codegen_record _ _ =
  match !(flags.record_impl) with
  | "hashmap" -> ()
  | "universal" -> ()
  | _ -> err ("unrecognised record implementation: " ^ !(flags.record_impl))

and codegen_access _ =
  match !(flags.record_impl) with
  | "hashmap" -> ()
  | "universal" -> ()
  | _ -> err ("unrecognised record implementation: " ^ !(flags.record_impl))

let codegen_program expr =
  (* Possibly perform some analysis dependent on record_imple *)
  if !(flags.record_impl) == "universal" then
    begin
    (* get a set of all record labels, then enumerate them *)
    let labels = Analysis.calc_uni_record expr in 
    let (_, rcd_map) =
      StrSet.fold
        (fun lbl (idx, map) -> (idx + 1, StrMap.add lbl idx map))
        labels
        (0, StrMap.empty) in
    universal_record := rcd_map
    end;
    
  (* Generate a function handle for main *)
  let main_type =
    let i32_t = i32_type context in
    let chr_t = i8_type context in
    function_type i32_t [|i32_t; pointer_type (pointer_type chr_t)|] in

  let main_handle = declare_function "main" main_type the_module in

  (* Create a new basic block to start insertion into. *)
  let bb = append_block context "entry" main_handle in
  position_at_end bb builder;

  (* Generate Main Body & insert into function block *)
  let main_body = codegen_expr expr StrMap.empty in


  (* Call printi on the result *)
  ignore (build_call printi [|main_body|] "prntout" builder);

  (* "Cap" main function with return 0 *)
  let ret_val = const_int (i32_type context) 0 in
  ignore (build_ret ret_val builder);

  (* Validate the generated code, checking for consistency. *)
  (if !(flags.assert_valid) then
     Llvm_analysis.assert_valid_function main_handle
   else
     ());

  (* Return the module *)
  the_module
