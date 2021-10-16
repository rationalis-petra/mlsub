
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

(* val typecheck : raw_expr -> mlsub_type *)

module Internal = struct
  module Data = Data
  module Check = Type_check
  module Target = Type_target
  module Coalesce = Type_coalesce
end
