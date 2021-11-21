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
    level : int;
    mutable uid : int }

let v_as_type (v : variable_state) : string = "ɑ" ^ string_of_int (v.uid)


type polarity = Positive | Negative
let inv = function
  | Positive -> Negative
  | Negative -> Positive

type polar_variable = variable_state * polarity 

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


(* TODO: the instantiate definition for typeschemes never use their level *)
(* argument... this seems fishy and deserves investigation *)
module type TypeScheme = sig
  type t
  val instantiate : t -> int -> simple_type 
  val level: t -> int
end

module SimpleTypeScheme
  = struct
  type t = simple_type
  let instantiate s _ = s
  let rec level s =
    match s with
    | Function (l, r) -> max (level l) (level r)
    | Record xs ->
       List.fold_right (fun x y -> max (level (snd x)) y) xs 0 
    | Variable vs -> vs.level
    | Primitive _ -> 0
end

module PolymorphicTypeScheme
  = struct
  type t = {level: int;
            body: simple_type}
  let rec instantiate s lvl = freshen_above s.level s.body lvl 
  and freshen_above lim ty lvl = 
    let freshened : (simple_type, simple_type) Hashtbl.t = Hashtbl.create 10 in
    let rec freshen (ty : simple_type) : simple_type =
      if SimpleTypeScheme.level ty <= lim then
        ty
      else
        match ty with
            | Variable tv ->
               begin
               match Hashtbl.find_opt freshened (freshen_above lim ty lvl) with
               | Some x -> x
               | None ->
                  let vr = Variable (fresh_var lvl) in
                  match vr with
                  | Variable v ->
                     begin
                       Hashtbl.add freshened ty vr;
                       v.lower_bounds <- List.rev
                                           (List.map freshen
                                              (List.rev (tv.lower_bounds)));
                       v.upper_bounds <- List.rev
                                           (List.map freshen
                                              (List.rev (tv.upper_bounds)));
                       vr
                     end
                  | _ -> raise (TypecheckError
                                  "err in freshen: fresh_var didn't return variable") 
               end
            | Function (l, r) -> Function (freshen l, freshen r)
            | Record fs -> Record (List.map (fun (v, t) -> (v, freshen t)) fs)
            | Primitive _ -> ty
    in
    freshen ty
  let level s = s.level
  let mkpt l b : t = {level = l; body = b}
end


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
  type t = primitive
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
  type t = primitive
  let compare t1 t2 =
    match (t1, t2) with
    | (PrimBool, PrimBool) ->  0
    | (PrimInt,  PrimInt)  ->  0
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
    List.compare (CompSimple.compare) (state1.lower_bounds @ state1.upper_bounds)
      (state2.lower_bounds @ state2.upper_bounds)
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

module SimpleSet = Set.Make(CompSimple)

module CSet = Set.Make(
                  struct
                    type t = simple_type * simple_type

                    let compare (t11, t12) (t21, t22) = 
                      if (CompSimple.compare t11 t21) = 0 then
                        0
                      else
                        CompSimple.compare t12 t22
                  end)

module VarStateSet = Set.Make(CompVarSt)
module PrimSet = Set.Make(CompPrimitive)



