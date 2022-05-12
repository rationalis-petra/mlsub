
type iraw_expr = Parse.expr

type mlsub_type = Data.MLSubType.t
(* open Data.MLSubType *)

let default_context = 
  let open Type_check in
  (Context.singleton
     "not"
     (SimpleTypeScheme
        (Function (Primitive PrimBool, Primitive PrimBool))))

let infer_type expr =
  (* Reset counter!! *)
  Data.var_id_counter := 0;
  let simple_t = Type_check.typecheck expr default_context 0 in
  let compact_t = Type_simplify.CompactTypeScheme.canonicalize_type simple_t in
  let simplified_t = Type_simplify.CompactTypeScheme.simplify_type compact_t in
  Type_simplify.CompactTypeScheme.coalesce_compact_type simplified_t

let infer_type_opt expr = 
  try Some (infer_type expr) with
  | Data.TypecheckError _ -> None 


let infer_type_stepped expr =
  (* Reset counter!! *)
  Data.var_id_counter := 0;
  print_endline ("checking simple: ");
  let simple_t = Type_check.typecheck expr default_context 0 in
  print_endline ("simple_t: " ^ Data.string_of_simple_type simple_t);
  let compact_t = Type_simplify.CompactTypeScheme.canonicalize_type simple_t in
  print_endline ("compact_t: " ^ Type_simplify.CompactTypeScheme.to_str compact_t);
  let simplified_t = Type_simplify.CompactTypeScheme.simplify_type compact_t in
  print_endline ("simplified_t: " ^ Type_simplify.CompactTypeScheme.to_str simplified_t);
  Type_simplify.CompactTypeScheme.coalesce_compact_type simplified_t


let string_of_type = Data.string_of_type

module Internal = struct
  module Data = Data
  module Check = Type_check
  module Coalesce = Type_coalesce
  module Simplify = Type_simplify
end

