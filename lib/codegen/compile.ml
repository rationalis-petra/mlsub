(* The AST module contains the expr type, as well as information on how to *)
(* convert from the parser's expr type*)


module AST = struct
  type pexpr = Parse.expr
  type op = Parse.op

  type expr
    = Int of int
    | Bool of bool
    | Var of string
    | Access of string
    | Record of (string * expr) list
    | Op of op * expr * expr
    | If of expr * expr * expr
    | Fun of string * expr
    | Let of string * expr * expr
    | Apply of expr * expr;;

  let rec expr_of_pexpr = function
    | Parse.Int i -> Int i
    | Parse.Bool b -> Bool b
    | Parse.Var s -> Var s
    | Parse.Access s -> Access s
    | Parse.Record fields ->
       Record (List.map (fun (k, v) -> (k, expr_of_pexpr v)) fields)
    | Parse.Op (op, e1, e2) -> Op (op, expr_of_pexpr e1, expr_of_pexpr e2)
    | Parse.If (e1, e2, e3) ->
       If (expr_of_pexpr e1, expr_of_pexpr e2, expr_of_pexpr e3)
    | Parse.Let (var, e1, e2) ->
       Let (var, expr_of_pexpr e2, expr_of_pexpr e1)
    | Parse.LetRec (var, e1, e2) ->
       Let (var, expr_of_pexpr e2, expr_of_pexpr e1)
    | Parse.Fun (v, e) -> Fun (v, expr_of_pexpr e)
    | Parse.Apply (e1, e2) ->
       Apply (expr_of_pexpr e1, expr_of_pexpr e2)

end

open AST
open Llvm
open Data

exception CodeGenError of string

(* The String Map is the module used for a symbol-table *)

(* The set of used function names *)
let func_names = ref StrSet.empty
let lambda_counter = ref 0

let gen_unique_name () = 
  let name = ref ("lambda" ^ string_of_int !lambda_counter) in
  while StrSet.mem !name !func_names do
    lambda_counter := !lambda_counter + 1;
    name := "lambda" ^ string_of_int !lambda_counter
  done;
  (lambda_counter := !lambda_counter + 1;
   func_names := StrSet.add !name !func_names;
   !name)




let context = global_context ()
let the_module = create_module context "MLSub"
let builder = builder context

let int_type = i64_type context
(* let str_type = Llvm.str *)

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
      | None -> raise (CodeGenError ("unknown variable name: " ^ name)))
  (* return a function which takes a hasmap and returns required field *)
  | Access _ ->
     (* let proto = codegen_proto (fresh_name (), ) *)
     raise (CodeGenError "record field access not implemented")

  | Record _ -> raise (CodeGenError "record creation not implemented")
  | Op (op, e1, e2) -> 
     let lhs = codegen_expr e1 symtable in
     let rhs = codegen_expr e2 symtable in
     (match op with
      | Add -> build_add  lhs rhs "addtmp" builder
      | Sub -> build_sub  lhs rhs "subtmp" builder
      | Mul -> build_mul  lhs rhs "multmp" builder
      | Div -> build_sdiv lhs rhs "divtmp" builder
      | Gre -> build_icmp Icmp.Sge lhs rhs "gretmp" builder
      | Eql -> build_icmp Icmp.Eq lhs rhs "eqltmp" builder
      | And -> build_and  lhs rhs "andtmp" builder
      | Or  -> build_or   lhs rhs "ortmp"  builder)

  | Let (var, body, var_expr) ->
     let new_symtable = StrMap.add var (codegen_expr var_expr symtable)
                          symtable in
     let body_val = codegen_expr body new_symtable in
     body_val


  (* For the moment, a 'function' will be a function pointer. Code generation
     for a function will generate a new global function with *) 
  (* TODO: convert from function to closure (funptr, environment) values *)
  | Fun (var, expr) ->
     let current_bb = insertion_block builder in
     let num = (!lambda_counter) in
     (lambda_counter := num + 1;
      let name = gen_unique_name () in
      (* TODO: switch to *)
      (* let func = codegen_lambda ((name, Array.of_list [var]), expr) *)
      let func = codegen_func ((name, [|(var, int_type)|]), expr) in

      (* If we just return here, then any newly generated code will be inserted
       * into the function we just created, which is BAD. Hence, we must point
       * the builder at the basic block we were in at the beginning of the function *)
      position_at_end current_bb builder;
      
      func) 

  | Apply (e1, e2) ->
  (* Note: LLVM IR has no lambdas: need to generate named functions *)
     (* TODO: switch to lambdas *)
     let callee = codegen_expr e1 symtable in
     let args = Array.of_list [codegen_expr e2 symtable] in
     build_call callee args "calltmp" builder



  | If (cond, e1, e2) ->
     let cond_code = codegen_expr cond symtable in
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
     ignore (build_cond_br cond_code then_bb else_bb builder);
     (* Set a unconditional branch at the end of the 'then' block and the
      * 'else' block to the 'merge' block. *)
     position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
     position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

     (* Finally, set the builder to the end of the merge block. *)
     position_at_end merge_bb builder;

     phi

  (* | _ -> raise (CodeGenError "codegen_expr incomplete") *)


(* Handling of lambda (anonymous) functions: 
 * 1. Generate a (non-capturing) global function
 * 2. Return a pointer to said function
 * 3. Funcalls dereference & call the pointer
 *)

(* NOTE: CURRENTLY FUNCTIONS ARE NOT CLOSURES AND CAN ONLY HAVE INT_64
 * ARGUMENTS/RETURN VALUES! *)

(* Much like in C/C++, we have functions declarations (in LLVM: prototypes)
 * that describe a function, e.g. i64 add (i64: a, i64: b). The function *
 * codegen_proto function is used to generate these prototypes *)

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

(* Generate code for a TOPLEVEL function. When generating closures-functions, we *)
(* first create a new (unique) name and use it to generate a toplevel function *)
(* which is then bundled with a record into a closure object *)

(* TODO: carry context through codegen_func *)
and codegen_func ((name, args), body) = 
  (* Generate a function handle (prototype) *)
  let function_handle = codegen_proto (name, args) in
  (* Create a new basic block to start insertion into. *)
  let bb = append_block context "entry" function_handle in
  position_at_end bb builder;
  try
    let symtable =
      StrMap.of_seq (Array.to_seq 
                       (Util.arr_zip
                          (Array.map (fun (x, _) -> x) args)
                          (params function_handle))) in 
    let ret_val = codegen_expr body symtable in

    (* Finish off the function. *)
    let _ = build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function function_handle;

    function_handle
  with e ->
    delete_function function_handle;
    raise e

(* 
and codegen_lambda 
*)

(* This *)
let codegen_program expr =
  (* Generate a function handle (prototype) *)
  let args = [||] in
  let function_handle = codegen_proto ("main", args) in
  (* Create a new basic block to start insertion into. *)
  let bb = append_block context "entry" function_handle in
  position_at_end bb builder;
  try
    let symtable =
      StrMap.of_seq (Array.to_seq 
                       (Util.arr_zip args (params function_handle))) in
    (* Generate Main Body & insert into function block *)
    let func_body = codegen_expr expr symtable in

    (* "Cap" main function with return 0 *)

    (* Assign the body to a mutable variable, to insert it into the function's
     * basic_block *)
    let alloc = build_alloca int_type "x" builder  in
    ignore ( build_store func_body alloc builder);
    let ret_val = codegen_expr (AST.Int 0) StrMap.empty in
    let _ = build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function function_handle;

    (* Return the module *)
    the_module
  with e ->
    delete_function function_handle;
    raise e


(* This is codegen initialisation code, where I register a function to put *)
(* integers to stdio *)

  
