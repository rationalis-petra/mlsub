
type expr = Data.AST.expr

let expr_of_pexpr = Data.AST.expr_of_pexpr

let codegen_program = Compile.codegen_program

let init () = ()

module Internal = struct
  module AST = Data.AST
end


