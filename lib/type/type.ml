
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

let infer_type = Type_check.infer_type

let string_of_type = Data.string_of_type

module Internal = struct
  module Data = Data
  module Check = Type_check
  module Target = Type_target
  module Coalesce = Type_coalesce
end

