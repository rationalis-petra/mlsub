module P = Parse

exception TypecheckError of string

type raw_expr = P.expr

(* simple_type is the type inferred by the first stage of the type-checking
   algorithm *) 

module PrimType = struct
  type primitive_type = PrimInt | PrimBool
end

module MLSubType = struct
  open PrimType

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

  type t = mlsub_type
end

open PrimType


type simple_type
  = Primitive of primitive_type
  | Variable  of variable_state
  | Function  of simple_type * simple_type
  | Record    of (string * simple_type) list

(* Store the constraints on a variable as a set of upper and lower bounds *)
and variable_state = {
    mutable lower_bounds : simple_type list;
    mutable upper_bounds : simple_type list;
    level : int;
    mutable uid : int }



type polarity = Positive | Negative
let inv = function
  | Positive -> Negative
  | Negative -> Positive

type polar_variable = variable_state * polarity 


let vst_to_str (v : variable_state) : string = "ɑ" ^ string_of_int (v.uid)
let vst_to_mlsub_type (v : variable_state) : MLSubType.t = Variable ("ɑ" ^ string_of_int (v.uid))

(* Utility functions *)
let var_id_counter = ref 0
let fresh_var l =
  begin
    let v = !var_id_counter in
    var_id_counter := v + 1;
    {lower_bounds = [];
     upper_bounds = [];
     level = l;
     uid = v}
  end


let string_of_primitive = function
  | PrimInt -> "int"
  | PrimBool -> "bool"

let rec string_of_simple_type : simple_type -> string = function
  | Primitive prim -> "Primitive " ^ string_of_primitive prim
  | Variable vst (* {lower_bounds; upper_bounds; level; uid} *) ->
     "Variable " ^ vst_to_str vst
  | Function (arg, res) -> "Function (" ^ string_of_simple_type arg ^ ", " ^
                          string_of_simple_type res ^ ")"
  | Record fields ->
     "{" ^ (List.fold_left
              (fun str (f,t) ->
                f ^ " : " ^ string_of_simple_type t ^ ";" ^ str
              )
              ""
              fields)
     ^ "}"

let rec string_of_type : MLSubType.t -> string = function
  | Top -> "⊤"
  | Bottom -> "⊥"
  | Union (Function (t1_, t2_), t2) ->
     "(" ^ (string_of_type (Function (t1_, t2_))) ^ ")⊔" ^ (string_of_type t2)
  | Union (t1, Function (t1_, t2_)) ->
     (string_of_type t1) ^ "⊔(" ^ (string_of_type (Function (t1_, t2_))) ^ ")"
  | Union (t1, t2) -> (string_of_type t1) ^ "⊔" ^ (string_of_type t2)

  | Intersection (Function (t1_, t2_), t2) ->
     "(" ^ (string_of_type (Function (t1_, t2_))) ^ ")⊓" ^ (string_of_type t2)
  | Intersection (t1, Function (t1_, t2_)) ->
     (string_of_type t1) ^ "⊓(" ^ (string_of_type (Function (t1_, t2_))) ^ ")"
  | Intersection (t1, t2) -> (string_of_type t1) ^ "⊓" ^ (string_of_type t2)


  | Function (Function (t1_, t2_), t2) ->
     "(" ^ (string_of_type (Function (t1_, t2_))) ^ ")" ^
       " → " ^ (string_of_type t2)
  | Function (t1, t2) -> (string_of_type t1) ^ " → " ^ (string_of_type t2)
  | Record lst -> 
     "{" ^ (List.fold_left
              (fun str (lab, t) ->
                lab ^ ":" ^ (string_of_type t) ^ "," ^ str ) "" lst) ^
       "}"
  | Recursive (var, t2) -> "μ" ^ var ^ "." ^ (string_of_type t2)
  | Variable s -> s
  | Primitive p -> string_of_primitive p


(* Use a functor to generate a set that stores the type simple_type *)
(* simple_type. For this to work, simple_type has to be comparable, hence the *)
(* large anonymous module. *)

module type Comp = sig
  type t
  val compare : t -> t -> int
end

module type CompT =
  functor (X : sig type t end) ->
  sig
    type t = X.t
    val compare : t -> t -> int
  end

module type CompProdT =
  functor (X : Comp) (Y : Comp) ->
  Comp with type t = X.t * Y.t

module type CompPrimitiveT = sig (* CompT (struct type t = simple_type end) *)
  type t = primitive_type
  val compare : t -> t -> int
end

module CompProd = 
  functor (X : Comp) (Y : Comp) -> struct
    type t = X.t * Y.t
    let compare (a, b) (a', b') =
      match X.compare a a' with
      | 0 -> Y.compare b b'
      | n -> n
  end
  
module type CompSimpleT = sig (* CompT (struct type t = simple_type end) *)
  type t = simple_type
  val compare : t -> t -> int
end

module type CompVarStT = sig
  type t = variable_state
  val compare : t -> t -> int
end

module type CompSimpleProdT = sig
  type t = (simple_type * simple_type)
  val compare : t -> t -> int
end

module type CompPolVarT = sig (* CompT (struct type t = simple_type end) *)
  type t = polar_variable
  val compare : t -> t -> int
end

module type CompPolT = sig (* CompT (struct type t = simple_type end) *)
  type t = polarity
  val compare : t -> t -> int
end

module rec CompPrimitive : CompPrimitiveT
  = struct
  type t = primitive_type
  let compare t1 t2 =
    match (t1, t2) with
    | (PrimBool, PrimBool) ->  0
    | (PrimInt , PrimInt)  ->  0
    | (PrimBool, PrimInt)  -> -1
    | (PrimInt , PrimBool) ->  1
  end

(* Because we want to use a set to cache pairs of simple_type, we must introduce
   a module containing a comparison function for simple_type * simple_type *) 
module rec CompSimple : CompSimpleT
  = struct
  type t = simple_type
  let rec compare t1 t2 =
    match (t1, t2) with
    | (Primitive p1, Primitive p2) ->
       CompPrimitive.compare p1 p2  
    | (Variable v1, Variable v2) ->
       CompVarSt.compare v1 v2
    | (Function (t11, t12), Function(t21, t22)) ->
       CompSimpleProd.compare (t11, t12) (t21, t22)
    | (Record xs, Record ys) -> 
       begin
         match List.find_opt (fun (a, b) ->
                   compare a b != 0) 
                 (List.combine (snd (List.split xs)) (snd (List.split ys))) with
         | Some (a, b) -> compare a b
         | None -> 0
       end
    | (Primitive _, _) -> -1
    | (_, Primitive _) ->  1
    | (Variable _, _)  -> -1
    | (_, Variable _)  ->  1
    | (Function _, _)  -> -1
    | (_, Function _)  ->  1



end

and CompVarSt : CompVarStT
  = struct
  type t = variable_state
  let compare state1 state2 = 
    Int.compare state1.uid state2.uid
  end

and CompSimpleProd : CompSimpleProdT = CompProd( CompSimple ) ( CompSimple )

module CompPol : CompPolT
  = struct
  type t = polarity
  let compare p1 p2 = 
    match (p1, p2) with  
    | (a, b) when a = b -> 0
    | (Positive, _) -> 1
    | (_, _) -> -1
  end

module CompPolVar : CompPolVarT
  = struct
  type t = polar_variable
  let compare pv1 pv2 = 
    let (v1, pol1) = pv1 in
    let (v2, pol2) = pv2 in
    match CompVarSt.compare v1 v2 with
    | 0 -> CompPol.compare pol1 pol2
    | n -> n
  end


module CSet = Set.Make(CompSimpleProd)

module SimpleSet = Set.Make(CompSimple)
module VarStateSet = Set.Make(CompVarSt)
module PrimSet = Set.Make(CompPrimitive)
module PolVarSet = Set.Make(CompPolVar)

module SimpleMap = Map.Make(CompSimple)
module PolVarMap = Map.Make(CompPolVar)
module VarStateMap = Map.Make(CompVarSt)




let rec vst_to_str_rec (v : variable_state) (s : VarStateSet.t) : string =
  let print_bounds bounds = 
    let news = VarStateSet.add v s in
    List.fold_left  (fun str ty ->  str ^ string_of_simple_type_rec ty news) "" bounds in
  if VarStateSet.mem v s then vst_to_str v
  else (vst_to_str v) ^ "{" ^ print_bounds v.lower_bounds ^ " | " ^ print_bounds v.upper_bounds
  ^ "}"

and string_of_simple_type_rec (ty : simple_type) (s : VarStateSet.t) : string = 
  match ty with
  | Primitive prim -> "Primitive " ^ string_of_primitive prim
  | Variable vst -> "Variable " ^ vst_to_str_rec vst s
  | Function (arg, res) -> "Function (" ^ string_of_simple_type_rec arg s ^ ", " ^
                             string_of_simple_type_rec res s ^ ")"
  | Record fields ->
     "{" ^ (List.fold_left
              (fun str (f,t) ->
                f ^ " : " ^ string_of_simple_type_rec t s ^ ";" ^ str
              )
              ""
              fields)
     ^ "}"
