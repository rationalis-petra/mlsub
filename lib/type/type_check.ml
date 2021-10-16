module P = Parse
open Data

module Context = Map.Make(String)

(* A context which stores the type of a variable *)
type ctx = simple_type Context.t

(* Utility functions *)
let fresh_var () = Variable ({lower_bounds = [];
                              upper_bounds = []})
let err msg = raise (TypecheckError msg)

let swap f a b = f b a

(* Constrain the lhs to be a subtype of the rhs. For performance reasons, the
   function caches results. In order to represent this, we have constrain be a
   class, with the cache being an instance variable and the actual function
   being a class method *) 

class constrain_func = object(self)
  val mutable cache: CSet.t = CSet.empty

  method constrain (lhs: simple_type) (rhs: simple_type) = 
    if CSet.mem (lhs, rhs) cache then
      ()
    else
      (cache <- CSet.add (lhs, rhs) cache;
       match (lhs, rhs) with
       | (Primitive n0, Primitive n1) when n0 = n1 -> ()

       (* Function types are constrained according to the usual
            rules of contravariance and covariance*)
       | (Function (l0, r0), Function (l1, r1)) ->
          self#constrain l1 l0;
          self#constrain r0 r1

       (* If we are constraining one record to be a subtype of
            another, then we require
            1. That the first record have all the fields of the second
            2. That the second's fields are constrained to be a
               subtype of the firsts' *)
       | (Record fs0, Record fs1) ->
          let _ = 
            List.map (fun (name1, type1) ->
                match List.find_opt (fun (name0, _) -> name0 = name1) fs0 with
                | Some (_, type0) -> self#constrain type0 type1
                | None -> err ("missing field: " ^ name1
                               ^ " when subtyping "
                               ^ string_of_type lhs)) fs1 in
          ()

       (* The tricky bits come when there's variables on the left
            or right, as this means that we have to carefully
            constrain them. 
            + First, we add the corresponding
              constraint to the upper/lower bounds, respectively 
            + Second, we iterate over the existing opposite bounds in
              order to make sure that they become consistent with the
              new bound (TODO: more carefully investigate why...) *)
       | (Variable lhs, rhs) ->
          lhs.upper_bounds <- rhs :: lhs.upper_bounds;
          let _ = 
            List.map (swap self#constrain rhs) lhs.lower_bounds in
          ()
       | (lhs, Variable rhs) ->
          rhs.lower_bounds <- lhs :: rhs.lower_bounds;
          let _ =
            List.map (self#constrain lhs) rhs.upper_bounds in
          ()
       | _ -> err ("cannot constrain " ^ (string_of_type lhs)
                   ^ " <: " ^ (string_of_type rhs)))

end

(* TODO: rather than have cf be a global variable, allow at the initial
   invocation of typecheck for a new constrain-function/cache to be used
   to prevent excess memory usage, particularly when testing... *)
let cf = new constrain_func



(* The raw type-checking/inference function *)
(* This will give us the raw type information which we can later condense into 
   the actual MLsub types*)

let rec typecheck raw_expr ctx = 
  match raw_expr with
  (* Type checking primitives is relatively easy *)
  | P.Int _  -> Primitive PrimInt
  | P.Bool _ -> Primitive PrimBool

  (* Type-checking a name relatively easy - just lookup that name in the context *)
  | P.Var name -> Context.find name ctx

  (* Type-checking a record is also easy - just typecheck all the subexpressions *)
  | P.Record xs -> Record (List.map (fun (n, e) -> (n, typecheck e ctx)) xs)

  (* To type the body of a lambda abstraction, we create a fresh variable to
     represent the parameter type, then typecheck the body with this
     new context *) 
  | P.Fun (name, body) -> 
     let param_type = fresh_var () in
     Function (param_type, typecheck body (Context.add name param_type
                                             ctx)) 

  (* A record access is just a function, and so type-checking is very similar:
     with the prime difference being that we know the input type must be a
     record matching name to the return type *) 
  | P.Access name ->
     let ret_type = fresh_var () in
     Function (Record [(name, ret_type)], ret_type)

  (* To typecheck a function application, we constrain the function to
     have an input type which is at least the argument, and a fresh
     result type. Then, return the return type *) 
  | P.Apply (func, arg) ->
     let ret_type = fresh_var () in
     cf#constrain (typecheck func ctx) (Function (typecheck arg ctx, ret_type));
     ret_type

  (* To typecheck an operator is much the same as a function application:
     we first constrain the inputs to the correct types (int/bool), then
     constrain the output to have the correct type (int/bool) *)
  | P.Op (op, e0, e1) ->
     begin
       match op with
       | Add | Sub | Mul | Div ->
          cf#constrain (typecheck e0 ctx) (Primitive PrimInt);
          cf#constrain (typecheck e1 ctx) (Primitive PrimInt);
          (Primitive PrimInt)
       | Gre | Eql ->
          cf#constrain (typecheck e0 ctx) (Primitive PrimInt);
          cf#constrain (typecheck e1 ctx) (Primitive PrimInt);
          (Primitive PrimBool)
       | And | Or -> 
          cf#constrain (typecheck e0 ctx) (Primitive PrimBool);
          cf#constrain (typecheck e1 ctx) (Primitive PrimBool);
          (Primitive PrimBool)
     end

  (* As with operators, the if statement can be typechecked in a manner similar
     to a function: constrain the first argument to be a bool, and the latter
     arguments should be the same type *)
  (* TODO: is this corrcet??? *)

  | P.If (e0, e1, e2) -> 
     let body_type = fresh_var () in
     cf#constrain (typecheck e0 ctx) (Primitive PrimBool);
     cf#constrain (typecheck e1 ctx) body_type;
     cf#constrain (typecheck e2 ctx) body_type;
     body_type

  (* TODO: Let and LetRec*)


  | _ -> err "typecheck function is incomplete"

