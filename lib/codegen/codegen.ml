
type expr = Compile.AST.expr

let expr_of_pexpr = Compile.AST.expr_of_pexpr

let codegen_program = Compile.codegen_program

let init () = Jit.init_the_module ()


module Internal = struct
  module AST = Compile.AST
end


