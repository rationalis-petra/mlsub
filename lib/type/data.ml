module P = Parse

exception TypecheckError of string

type raw_expr = P.expr


(* simple_type is the type inferred by the first stage of the type-checking
   algorithm *) 
type primitive = PrimInt | PrimBool

type simple_type
  = Primitive of primitive 
  | Variable  of variable_state
  | Function  of simple_type * simple_type
  | Record    of (string * simple_type) list

(* Store the constraints on a variable as a set of upper and lower bounds *)
and variable_state = {
    mutable lower_bounds : simple_type list;
    mutable upper_bounds : simple_type list;
    mutable uid : int }

let v_as_type (v : variable_state) : string = "ɑ" ^ string_of_int (v.uid)

type polarity = Positive | Negative

type polar_variable = PolarVariable of variable_state * polarity 

type mlsub_type
  = Top
  | Bottom
  | Union         of mlsub_type * mlsub_type
  | Intersection  of mlsub_type * mlsub_type 
  | FunctionType  of mlsub_type * mlsub_type
  | RecordType    of (string * mlsub_type) list
  | RecursiveType of string * mlsub_type
  | VariableType  of string
  | PrimitiveType of primitive





let rec string_of_type : simple_type -> string = 
  let string_of_primitive = function
    | PrimInt -> "PrimInt"
    | PrimBool -> "PrimBool"
  in function
  | Primitive prim -> "Primitive " ^ string_of_primitive prim
  | Variable _ -> "Variable <state not to string>"
  | Function (arg, res) -> "Function (" ^ string_of_type arg ^ ", " ^
                          string_of_type res ^ ")"
  | Record _ -> "<record to string incomplete>"


(* Use a functor to generate a set that stores the type simple_type *)
(* simple_type. For this to work, simple_type has to be comparable, hence the *)
(* large anonymous module. *)

(* Because we want to use a set to cache pairs of simple_type, we must introduce
   a module containing a comparison function for simple_type * simple_type *) 
module Comparisons = struct
  let rec compare_simpletype t1 t2 =
    match (t1, t2) with
    | (Primitive p1, Primitive p2) ->
       compare_primitive p1 p2  
    | (Variable v1, Variable v2) ->
       compare_varstate v1 v2
    | (Function (t11, t12), Function(t21, t22)) ->
       compare (t11, t12) (t21, t22)
    | (Record xs, Record ys) -> 
       begin
         match List.find_opt (fun (a, b) ->
                   compare_simpletype a b != 0) 
                 (List.combine (snd (List.split xs)) (snd (List.split ys))) with
         | Some (a, b) -> compare_simpletype a b
         | None -> 0
       end
    | (Primitive _, _) -> -1
    | (_, Primitive _) ->  1
    | (Variable _, _)  -> -1
    | (_, Variable _)  ->  1
    | (Function _, _)  -> -1
    | (_, Function _)  ->  1

  and compare_primitive p1 p2 = 
    match (p1, p2) with
    | (PrimBool, PrimBool) ->  0
    | (PrimInt,  PrimInt)  ->  0
    | (PrimBool, PrimInt)  -> -1
    | (PrimInt , PrimBool) ->  1

  and compare_varstate state1 state2 = 
    List.compare (compare_simpletype) (state1.lower_bounds @ state1.upper_bounds)
      (state2.lower_bounds @ state2.upper_bounds)
end
