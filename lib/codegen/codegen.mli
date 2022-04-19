
type expr

val expr_of_pexpr : Parse.expr -> expr

val codegen_program : expr -> Llvm.llmodule

val init : unit -> unit

val set_record_impl : string -> unit

(* Get the llmodule as a string *)

(* Get the llmodule as an intermediate representation *)
(* val get_code_ir : unit -> string *)

module Internal : sig
  module AST = Data.AST
end
