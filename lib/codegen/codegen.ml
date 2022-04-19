
type expr = Data.AST.expr

let expr_of_pexpr = Data.AST.expr_of_pexpr

let codegen_program = Compile.codegen_program

let init () = ()

let set_record_impl s =
  match s with
  | "global" | "hashmap" | "evidence" ->
     Compile.flags.record_impl := s
  | _ ->  Compile.err ("record implementation argument has value " ^ s
                       ^ " was expecting global, hashmap or evidence")

module Internal = struct
  module AST = Data.AST
end


