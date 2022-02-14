module P = Parse
open Data

module Context = Map.Make(String)



(* A context which stores the type of a variable. Because this can be a simple *)
(* type or polymorphic type, we wrap in a first-class module *)

(* Instances of a type scheme *)

module type TScheme = sig
  module TypeScheme : TypeScheme
  val this : TypeScheme.t
  end

let mkTScheme (type a) (module M : TypeScheme with type t = a)
      (x : a) : (module TScheme) = 
  (module struct
     module TypeScheme = M
     let this = x
   end : TScheme) 

type ctx = (module TScheme) Context.t

let err msg = raise (TypecheckError msg)

let swap f a b = f b a


  

(* Constrain the lhs to be a subtype of the rhs. For performance reasons, the
   function caches results. In order to represent this, we have constrain be a
   class, with the cache being an instance variable and the actual function
   being a class method *) 

(* TODO: rather than have cf_cache be a global variable, allow at the initial
   invocation of typecheck for a new constrain-function/cache to be used
   to prevent excess memory usage, particularly when testing... *)
let cf_cache = ref CSet.empty

module PolCache = Map.Make(CompPolVar)

(* The constrain method is imperative: it will update the mutable bounds of
     type variable_state contained within a type to make the lhs a subtype of
     the rhs, or return an error if this is not possible *)
let rec constrain (lhs: simple_type) (rhs: simple_type) = 
  if CSet.mem (lhs, rhs) (!cf_cache) then
    ()
  else
    (* TODO: future optimization - only add store type variables to the cache*)
    (cf_cache := CSet.add (lhs, rhs) (!cf_cache);
     match (lhs, rhs) with
     | (Primitive n0, Primitive n1) when n0 = n1 -> ()

     (* Function types are constrained according to the usual
            rules of contravariance and covariance*)
     | (Function (l0, r0), Function (l1, r1)) ->
        constrain l1 l0;
        constrain r0 r1

     (* If we are constraining one record to be a subtype of
            another, then we require
            1. That the first record have all the fields of the second
            2. That the second's fields are constrained to be a
               subtype of the firsts' *)
     | (Record fs0, Record fs1) ->
        let _ = 
          List.map (fun (name1, type1) ->
              match List.find_opt (fun (name0, _) -> name0 = name1) fs0 with
              | Some (_, type0) -> constrain type0 type1
              | None -> err ("missing field: " ^ name1
                             ^ " when subtyping "
                             ^ string_of_simple_type lhs)) fs1 in
        ()

     (* The tricky bits come when there's variables on the left
            or right, as this means that we have to carefully
            constrain them. This involves checking the level. If the level
            comparison succeeds (TODO: what does this mean?) Then
            + First, we add the corresponding
              constraint to the upper/lower bounds, respectively 
            + Second, we iterate over the existing opposite bounds in
              order to make sure that they become consistent with the
              new bound (TODO: more carefully investigate why...) 
            Otherwise, we will make use of the extrude funcion... TODO *)
     | (Variable lhs, _) when lhs.level >= SimpleTypeScheme.level rhs ->
        lhs.upper_bounds <- rhs :: lhs.upper_bounds;
        let _ = 
          List.map (swap constrain rhs) lhs.lower_bounds in
        ()
     | (_, Variable rhs) when SimpleTypeScheme.level lhs <= rhs.level ->
        rhs.lower_bounds <- lhs :: rhs.lower_bounds;
        let _ =
          List.map (constrain lhs) rhs.upper_bounds in
        ()

     | (Variable lhv, rhs) ->
        (* extrude returns a copy of the problematic (level-violating) type
             up to it's type variables of the wrong level, except this copy has
             been modified so it is a the right level *)
        let rhs' = extrude rhs Negative (lhv.level) (ref PolCache.empty) in
        constrain lhs rhs'
     | (lhs, Variable rhv) ->
        let lhs' = extrude lhs Positive rhv.level (ref PolCache.empty) in
        constrain lhs' rhs
     | _ -> err ("cannot constrain " ^ (string_of_simple_type lhs)
                 ^ " <: " ^ (string_of_simple_type rhs)))




(* Extrude : make a copy up to type variables, with level correction *) 
and extrude (ty : simple_type) (pol : polarity) (lvl : int)
          (cache : simple_type PolCache.t ref) : simple_type =
  if (SimpleTypeScheme.level ty <= lvl) then
    ty
  else
    match ty with
    | Primitive _ -> ty
    | Function (l, r) -> Function (extrude l (inv pol) lvl cache,
                                   extrude r pol lvl cache)
    | Record fs ->
       Record (List.map
                 (fun (x, y) ->
                   (x, extrude y pol lvl cache))
                 fs)
    | Variable vs ->
       match PolCache.find_opt (vs, pol) (!cache) with 
       | Some v -> v
       | None ->
          let nvs =  fresh_var lvl in
          begin
            cache := PolCache.add (vs, pol) (Variable nvs) (!cache);
            if pol == Positive then
              begin
                vs.upper_bounds <- (Variable nvs) :: vs.upper_bounds;
                nvs.lower_bounds <- List.map (fun x -> extrude x pol lvl cache)
                                      vs.lower_bounds
              end
            else
              begin
                vs.lower_bounds <- (Variable nvs) :: vs.lower_bounds;
                nvs.upper_bounds <- List.map (fun x -> extrude x pol lvl cache)
                                      vs.upper_bounds
              end;
            Variable nvs
            end





(* The raw type-checking/inference function *)
(* This will give us the raw type information which we can later condense into 
   the actual MLsub types*)


let rec typecheck raw_expr (ctx : ctx) (lvl: int) : simple_type = 
  match raw_expr with
  (* Type checking primitives is relatively easy *)
  | P.Int _  -> Primitive PrimInt
  | P.Bool _ -> Primitive PrimBool

  (* Type-checking a name relatively easy - just lookup that name in the context *)
  | P.Var name ->
     let open (val (Context.find name ctx) : TScheme) in
     TypeScheme.instantiate this lvl

  (* Type-checking a record is also easy - just typecheck all the subexpressions *)
  | P.Record xs ->
     Record (List.map (fun (n, e) -> (n, typecheck e ctx lvl)) xs)

  (* To type the body of a lambda abstraction, we create a fresh variable to
     represent the parameter type, then typecheck the body with this
     new context *) 
  | P.Fun (name, body) -> 
     let param_type = Variable (fresh_var lvl) in
     Function (param_type,
               typecheck
                 body
                 (Context.add
                    name
                    (mkTScheme
                       (module SimpleTypeScheme)
                       param_type)
                    ctx)
                 lvl) 

  (* A record access is just a function, and so type-checking is very similar:
     with the prime difference being that we know the input type must be a
     record matching name to the return type *) 
  | P.Access name ->
     let ret_type = Variable (fresh_var lvl) in
     Function (Record [(name, ret_type)], ret_type)

  (* To typecheck a function application, we constrain the function to
     have an input type which is at least the argument, and a fresh
     result type. Then, return the return type *) 
  | P.Apply (func, arg) ->
     let ret_type = Variable (fresh_var lvl) in
     constrain (typecheck func ctx lvl) (Function (typecheck arg ctx lvl, ret_type));
     ret_type

  (* To typecheck an operator is much the same as a function application:
     we first constrain the inputs to the correct types (int/bool), then
     constrain the output to have the correct type (int/bool) *)
  | P.Op (op, e0, e1) ->
     begin
       match op with
       | Add | Sub | Mul | Div ->
          constrain (typecheck e0 ctx lvl) (Primitive PrimInt);
          constrain (typecheck e1 ctx lvl) (Primitive PrimInt);
          (Primitive PrimInt)
       | Gre | Eql ->
          constrain (typecheck e0 ctx lvl) (Primitive PrimInt);
          constrain (typecheck e1 ctx lvl) (Primitive PrimInt);
          (Primitive PrimBool)
       | And | Or -> 
          constrain (typecheck e0 ctx lvl) (Primitive PrimBool);
          constrain (typecheck e1 ctx lvl) (Primitive PrimBool);
          (Primitive PrimBool)
     end

  (* As with operators, the if statement can be typechecked in a manner similar
     to a function: constrain the first argument to be a bool, and the latter
     arguments should be the same type *)
  (* TODO: is this corrcet??? *)

  | P.If (e0, e1, e2) -> 
     let body_type = Variable (fresh_var lvl) in
     constrain (typecheck e0 ctx lvl) (Primitive PrimBool);
     constrain (typecheck e1 ctx lvl) body_type;
     constrain (typecheck e2 ctx lvl) body_type;
     body_type

  (* Let Binding*)
  | P.Let (name, e, bod) ->
     let val_t = typecheck e ctx (lvl + 1) in (* note the level increase *)
     typecheck
       bod
       (Context.add
          name
          (mkTScheme (module PolymorphicTypeScheme)
             (PolymorphicTypeScheme.mkpt lvl val_t)) ctx)
       lvl
  (* Recursive Let Binding*)
  | P.LetRec (name, e1, bod) ->
     let val_t = typecheck e1 ctx (lvl + 1) in
     typecheck
       bod
       (Context.add
          name
          (mkTScheme
             (module PolymorphicTypeScheme)
             (PolymorphicTypeScheme.mkpt lvl val_t))
          ctx)
       lvl

let infer_simple_type raw_expr = typecheck raw_expr Context.empty 0
