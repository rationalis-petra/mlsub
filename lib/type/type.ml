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

(* constrain one type to be a subtype of another. This is done 
   imperatively, and, further, the function must cache results. In
   order to represent this, we have constrain be a class, with the
   cache being an instance variable and the actual function being a
   class method  *)

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
let cf = new constrain_func



(* The core type-checking function *)
(* val typecheck : raw_expr -> ctx -> simple_type *)
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

  (* To typecheck a function application, we constrain the function to
     have an input type which is at least the argument, and a fresh
     result type. Then, return the result type *) 
  | P.Apply (func, arg) ->
     let res_type = fresh_var () in
     cf#constrain (typecheck func ctx) (Function (typecheck arg ctx, res_type));
     res_type

  | _ -> err "typecheck function is incomplete"

