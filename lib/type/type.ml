
type raw_expr = Parse.expr

type mlsub_type
  = Top
  | Bottom
  | Union        of mlsub_type * mlsub_type
  | Intersection of mlsub_type * mlsub_type
  | Function     of mlsub_type * mlsub_type
  | Record       of (string * mlsub_type) list
  | Recursive    of string * mlsub_type
  | Variable     of string
  | Primitive    of primitive_type

and primitive_type = PrimInt | PrimBool

let default_context = 
  let open Type_check in
  (Context.singleton
     "not"
     (SimpleTypeScheme
        (Function (Primitive PrimBool, Primitive PrimBool))))

let infer_type expr =
  let simple_t = Type_check.typecheck expr default_context 0 in
  let compact_t = Type_simplify.CompactTypeScheme.canonicalize_type simple_t in
  let simplified_t = Type_simplify.CompactTypeScheme.simplify_type compact_t in
  Type_simplify.CompactTypeScheme.coalesce_compact_type simplified_t

let string_of_type = Data.string_of_type

module Internal = struct
  module Data = Data
  module Check = Type_check
  module Coalesce = Type_coalesce
  module Simplify = Type_simplify
end

