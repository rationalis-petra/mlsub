open Llvm
open Data
open Data.AST_Evidence

open Shared

let access_cache : llvalue StrMap.t ref = ref StrMap.empty


let rec codegen_expr expr symtable =
  (match expr with
   | Int i -> 
      (const_int int_type i, NullEv, None)
   | Bool b ->
      let int_of_bool b = if b then 1 else 0 in
      (const_int int_type (int_of_bool b), NullEv, None)
   | Var name ->
      (match StrMap.find_opt name symtable with
       | Some v -> v
       | None -> err ("unknown variable name: " ^ name))
 
   | Access field -> codegen_access field
   | Apply (e1, e2) -> 
      let (callee, required, _) = codegen_expr e1 symtable in

      let closure = build_inttoptr callee int_ptr_type "closure" builder in

      let func_int = build_load closure "func_int" builder in
      let func = build_inttoptr func_int closure_fn_ptr_type "func" builder in

      let env_int = build_add callee (const_int int_type ptr_size) "env_int" builder in
      let env_ptr = build_inttoptr env_int int_ptr_type "env_ptr" builder in

      let (arg, provided, ev) = codegen_expr e2 symtable in
      let new_evidence =
        (match ev with
         | Some ev -> coerce_evidence required provided ev
         | None-> err "expected evidence from a term, but none was provided") in

      let args = [|arg; env_ptr; new_evidence|] in
      
      let retval = build_call func args "calltmp" builder in
      (retval, NullEv, None)
      
   | _ -> err "unimplemented for evidence")


and codegen_access field =
  match StrMap.find_opt field !access_cache with
  | Some x -> 
      (x, RecordEv [(field, NullEv)], None)
  | None -> begin
      let current_bb = insertion_block builder in
      let access_handle =
        codegen_proto (gen_unique_name ()
                     , [|("rcd", int_type); ("env", int_ptr_type); ("evid", int_ptr_type)|]) in
      let bb = append_block context "entry" access_handle in
      position_at_end bb builder;

      let rcd = Array.get (params access_handle) 0 in

      (* get the evidence *)
      let e_fnc = codegen_get_evidence_fnc () () () in
      let ret_val = build_call e_fnc [|rcd|] "retval" builder in
      ignore (build_ret ret_val builder);

      (* Epilogue *)
      position_at_end current_bb builder; 

      let closure_ptr = build_array_malloc int_type
                          (const_int int_type 1)
                          "closure_env" builder in

      let access_as_int = build_ptrtoint access_handle int_type "func_as_int" builder in
      ignore (build_store access_as_int closure_ptr builder);
      let access_fnl = build_ptrtoint closure_ptr int_type "closure_as_int"
                         builder in
      access_cache := StrMap.add field access_fnl !access_cache;
      (access_fnl, RecordEv [field, NullEv], None)
    end

  
(* c_g_e_f evidence_description evidence field *)
and codegen_get_evidence_fnc _ _ _ =
  const_int int_type 0

(* Given two pieces of evidence *)
and coerce_evidence _ _ _ =
  const_int int_type 0
