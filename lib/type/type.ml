module P = Parse
open Hashtbl

exception TypecheckError of string

type raw_expr = P.expr

type primitive = PrimInt | PrimBool

(* Store the constraints on a variable as a set of upper and lower bounds *)

type simple_type
  = Primitive of primitive 
  | Variable  of variable_state
  | Function  of simple_type * simple_type
  | Record    of (string * simple_type) list

and variable_state = {lower_bounds : simple_type list;
                      upper_bounds : simple_type list}


(* A context which stores the type of a variable *)
type ctx = (string, simple_type) Hashtbl.t

(* The core type-checking function *)
(* val typecheck : raw_expr -> ctx -> simple_type *)

(* constrain one type to be a subtype of another. This is done imperatively *)
(* val constrain: simple_type -> simple_type -> unit *)

let freshVar = Variable { lower_bounds = []; upper_bounds = [] }

let rec typecheck raw_expr ctx = 
  match raw_expr with
  (* Type checking primitives is relatively easy *)
  | P.Int _  -> Primitive PrimInt
  | P.Bool _ -> Primitive PrimBool
  (* Type-checking a name relatively easy - just lookup that name in the context *)
  | P.Var name -> find ctx name
  (* Type-checking a record is also easy - just typecheck all the subexpressions *)
  | P.Record xs -> Record (List.map (fun (n, e) -> (n, typecheck e ctx)) xs)
  (* To type the body of a lambda abstraction, we create a fresh variable to
     represent the parameter type *)
  (* | P.Fun *)
  | _ -> raise (TypecheckError "typecheck incomplete")
