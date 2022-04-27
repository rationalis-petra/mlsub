open Llvm
open Data
open Data.AST

open Shared


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

  | Access field -> codegen_access field
  | Record fields -> codegen_record fields symtable
  | Op (op, e1, e2) -> codegen_op op e1 e2 symtable
  | If (cond, e1, e2) -> codegen_if cond e1 e2 symtable
  | Let (var, var_expr, body) ->
     let new_symtable = StrMap.add var (codegen_expr var_expr symtable)
                          symtable in
     let body_val = codegen_expr body new_symtable in
     body_val

  | LetRec (var, Fun (inner_var, expr), body) ->
     let num = (!lambda_counter) in
     lambda_counter := num + 1;
     let name = gen_unique_name () in
     let func = codegen_closure (Some var) name inner_var expr symtable in

     let new_symtable = StrMap.add var func symtable in
     let body_val = codegen_expr body new_symtable in
     body_val

  | LetRec (v, e1, e2) ->
     print_endline (string_of_expr (LetRec (v, e1, e2)));
     err "Currently can only compile recursive functions"


  | Fun (var, expr) ->
     (* Most of the code here lives in the codegen_closure helper function. This
      * simply generates a unique name for the global function which is used in
      * the lambda *)
     let num = (!lambda_counter) in
     (lambda_counter := num + 1;
      let name = gen_unique_name () in
      let func = codegen_closure None name var expr symtable in
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


and codegen_op op e1 e2 symtable = 
     let lhs = codegen_expr e1 symtable in
     let rhs = codegen_expr e2 symtable in
     (* For boolean operations, we must first cast int -> bool, then bool ->
      * int *)
     let build_bop build_fn lhs rhs name builder = 
       let lhs_as_bool = build_intcast lhs bool_type "blhstmp" builder in
       let rhs_as_bool = build_intcast rhs bool_type "brhstmp" builder in
       let out = build_fn lhs_as_bool rhs_as_bool name builder in
       build_intcast out int_type "outtmp" builder in
     (* For comparison operations, we must cast the result (bool) to int  *)
     let build_intcmp cmptype lhs rhs name builder = 
       let out = build_icmp cmptype lhs rhs name builder in
       build_intcast out int_type "outtmp" builder in
     (match op with
      | Add -> build_add  lhs rhs "addtmp" builder
      | Sub -> build_sub  lhs rhs "subtmp" builder
      | Mul -> build_mul  lhs rhs "multmp" builder
      | Div -> build_sdiv lhs rhs "divtmp" builder
      | Les -> build_intcmp Icmp.Slt lhs rhs "lestmp" builder
      | Gre -> build_intcmp Icmp.Sge lhs rhs "gretmp" builder
      | Eql -> build_intcmp Icmp.Eq lhs rhs "eqltmp" builder
      | And -> build_bop build_and lhs rhs "andtmp" builder
      | Or  -> build_bop build_or lhs rhs "ortmp"  builder)

and codegen_if cond e1 e2 symtable = 
     let cond_code = codegen_expr cond symtable in
     (* The output is in the form of an int64; we need to cast to bool *)
     let cond_bool = build_intcast cond_code bool_type "condtmp" builder in
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

(* Codegen closure will:
 * 1. Generate an array representing the environment
 * 2. Generate a function which takes an argument and a context, corresponding
 *    to the provided body
 * 3. Return a pointer to a (f, r) pair, where f is the generated function and r
 * is the generated record *)

and codegen_closure recursive name argname body symtable = 
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
    | Let (v, e1, e2) | LetRec (v, e1, e2) -> 
       StrSet.remove v
         (StrSet.union (get_free_vars e1) (get_free_vars e2))
    | If (e1, e2, e3) -> 
       StrSet.union (get_free_vars e1)
         (StrSet.union (get_free_vars e2) (get_free_vars e3))
    | Apply (e1, e2) ->
       StrSet.union (get_free_vars e1) (get_free_vars e2)
    | _ -> StrSet.empty in

  (* The variables we want to place in a context *)
  (* Note that the argument of the function may be in the set of free
   * variables, so we remove it *)
  let ctx_vars =
    let ctx_varset = StrSet.remove argname (get_free_vars body) in
    Array.of_seq (StrSet.to_seq ctx_varset) in

  (* Build the array representing the environment: first allocate, then insert
   * variables *)
  let closure_ptr = build_array_malloc int_type
                 (const_int int_type (Array.length ctx_vars))
                 "closure_env" builder in
  let closure_as_int = build_ptrtoint closure_ptr int_type "closure_as_int"
                         builder in

  (* Recursive *)
  let new_symtable =
    match recursive with
    | Some var -> StrMap.add var closure_as_int symtable
    | None -> symtable in

  Array.iteri (fun _idx _var ->
      let idx = codegen_expr (Int (_idx + ptr_size)) new_symtable in
      let var = codegen_expr (Var _var) new_symtable in
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
     Llvm_analysis.assert_valid_function func_handle);

  (* Switch back to generating code in the original basic block *)
  position_at_end current_bb builder;

  (* Store the function in the closure's memory *)
  let func_as_int = build_ptrtoint func_handle int_type "func_as_int" builder in
  ignore (build_store func_as_int closure_ptr builder);
  build_ptrtoint closure_ptr int_type "closure_as_int" builder


and codegen_record fields symtable =
  match !(flags.record_impl) with
  | "hashmap" ->
     (* Record generation is relatively easy; create the record then call the
        requisite number of record insertions*) 
     let num_fields = const_int int_type (List.length fields) in
     let record = build_call mk_record [|num_fields|] "recordtmp" builder in
     List.iter (fun (field, value) ->
         let val_ll = codegen_expr value symtable in 
         let key = get_field field in
         ignore (build_call set_field [|record; key; val_ll|] "" builder)
       ) fields;
     record
  | "global" ->
     let record_size = StrMap.fold (fun _ _ n -> n + 1) (!universal_record) 0 in
     let record = build_array_malloc int_type (const_int int_type record_size)
                    "record" builder in
     let record_int  = build_ptrtoint record int_type "record_as_int" builder in
     List.iter (fun (field, value) ->
         let val_ll = codegen_expr value symtable in
         let idx = StrMap.find field !universal_record in
         let idx_lli = build_add record_int (const_int int_type idx) "fieldtmp_int" builder in
         let idx_llp = build_inttoptr idx_lli int_ptr_type "fieldtmp_ptr" builder in
         ignore (build_store val_ll idx_llp builder))
       fields;
     record_int
  | _ -> err ("unrecognised record implementation: " ^ !(flags.record_impl))

and codegen_access field =
  match !(flags.record_impl) with
  | "hashmap" ->
     let field_ll = get_field field in
     build_call mk_field_access [|field_ll|] "accesstmp" builder
  | "global" -> 
     (* Start by checking the cache to see if we've already generated a field access function *)
     (match StrMap.find_opt field !universal_accesses with
      | Some x -> x
      | None ->
         begin
           (* We need to actually generate the function... *)
         let current_bb = insertion_block builder in
         let access_handle =
           codegen_proto (gen_unique_name ()
                        , [|("rcd", int_type); ("env", int_ptr_type)|]) in

         let bb = append_block context "entry" access_handle in
         position_at_end bb builder;

         let idx_ll = const_int int_type (StrMap.find field !universal_record) in 

         let arg = Array.get (params access_handle) 0 in
         let idx_int = build_add idx_ll arg "idx_int" builder in
         let idx_ptr = build_inttoptr idx_int int_ptr_type "idx_ptr" builder in
         let ret_val = build_load idx_ptr "ret_field" builder in
         ignore (build_ret ret_val builder);

         (if !(flags.assert_valid) then
            Llvm_analysis.assert_valid_function access_handle);

         position_at_end current_bb builder; 

         let closure_ptr = build_array_malloc int_type
                 (const_int int_type 1)
                 "closure_env" builder in

         let access_as_int = build_ptrtoint access_handle int_type "func_as_int" builder in
         ignore (build_store access_as_int closure_ptr builder);
         let access_fnl = build_ptrtoint closure_ptr int_type "closure_as_int"
                        builder in
         universal_accesses := StrMap.add field access_fnl !universal_accesses;
         access_fnl
         end)
  | _ -> err ("unrecognised record implementation: " ^ !(flags.record_impl))

let codegen_program expr =
  (* Possibly perform some analysis dependent on record_imple *)
  if (!(flags.record_impl)) = "global" then
    begin
    (* get a set of all record labels, then enumerate them *)
    let labels = Analysis.calc_uni_record expr in 
    let (_, rcd_map) =
      StrSet.fold
        (fun lbl (idx, map) -> (idx + 1, StrMap.add lbl idx map))
        labels
        (0, StrMap.empty) in
    universal_record := rcd_map;
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
