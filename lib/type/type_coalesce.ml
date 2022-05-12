
open Data
(* Use a functor to generate a set that stores the type simple_type *)
(* simple_type. For this to work, simple_type has to be comparable, hence the *)
(* large anonymous module. *)

module PSet = Set.Make(
                  struct
                    type t = variable_state * polarity

                    let compare (v1, p1) (v2, p2) = 
                      match CompVarSt.compare v1 v2 with 
                            | 0 ->
                               begin
                                 match (p1, p2) with
                                 | (Positive, Negative) -> 1
                                 | (Negative, Positive) -> -1
                                 | _ -> 0
                               end
                            | n -> n 
                  end)


class fresh_var_factory = object
  val mutable cache : char list = ['a']

  method unique_name () =
    let rec inc carry lst = 
      if carry then
        match lst with
        | [] -> ['a']
        | x::xs -> match x with
                   | 'a' .. 'y' -> (Char.chr (1 + Char.code x)) :: lst
                   | _ -> 'a' :: inc true xs 
      else lst in
    let out = inc true cache in
    cache <- out;
    String.of_seq (List.to_seq out)
end

type polar_variable = variable_state * polarity

let fresh_var = new fresh_var_factory
          

let invert = function
  | Positive -> Negative
  | Negative -> Positive

exception RecLookup of string

let coalesce_type (t : simple_type) = 
  let recursive : (polar_variable, MLSubType.t) Hashtbl.t  = Hashtbl.create 10 in 

  let rec go t polar in_process =
    match t with
    (* Primitive is the base case *)
    | Primitive p -> MLSubType.Primitive p

    (* Function & Record are the simple recursive cases *)
    | Function (l, r) -> MLSubType.Function ((go l (invert polar) in_process),
                                       go r polar in_process)
    | Record xs ->
       MLSubType.Record (List.map (fun (n, t) -> (n, go t polar in_process))
                     xs)

    (* Variable is where the real work happens *)
    | Variable v ->
       let vpol = (v, polar) in
       (* first, check if the variable is already being coalesced*)
       match PSet.find_opt vpol in_process with
       (* If it is, we look up the 'recursive' map - if this map contains an
          entry, return it. Otherwise, create a fresh variable, insert it recursive
          and return *)
       | Some _ ->
          begin
            match Hashtbl.find_opt recursive vpol with
            | Some x -> x
            | None ->
               let x = MLSubType.Variable (fresh_var#unique_name ()) in
               Hashtbl.add recursive vpol x;
               x
          end
       (* Otherwise, look up the bounds of the variable (dependent on polarity) *)
       | None -> 
          let bounds = match polar with
                     | Positive -> v.lower_bounds
                     | Negative -> v.upper_bounds in
       (* Recurse over the bound types, with the in_process set now containing
          this type *)
          let bound_types = List.map (fun x ->
                                go x polar (PSet.add vpol in_process))
                              bounds in
       (* Now that that is done, combine all bound_types with either union or
          intersection, depending on polarity *)
          let mrg = match polar with
                  | Positive -> (fun x y -> MLSubType.Union (x, y))
                  | Negative -> (fun x y -> MLSubType.Intersection (x, y)) in
          let res = List.fold_left mrg (vst_to_mlsub_type v) bound_types in
       (* Finally, lookup this variable name in the recursive map *)
          match (Hashtbl.find_opt recursive vpol) with
          | Some (MLSubType.Variable n) -> MLSubType.Recursive(n, res)
          | Some _ ->
             raise (RecLookup "non-variable type in recursive in type_coalesce")
          | None -> res in

  let open Map.Make(String) in
  let renaming : (string t) ref = ref empty in 
  let counter = ref 0 in
  let rec rename ty =
    match ty with
    | MLSubType.Top -> ty
    | MLSubType.Bottom -> ty
    | MLSubType.Primitive _ -> ty
    | MLSubType.Intersection (t1, t2) ->
       let new_t1 = rename t1 in
       let new_t2 = rename t2 in
       MLSubType.Intersection (new_t1, new_t2)
    | MLSubType.Union (t1, t2) ->
       let new_t1 = rename t1 in
       let new_t2 = rename t2 in
       MLSubType.Union (new_t1, new_t2)
    | MLSubType.Function (t1, t2) ->
       let new_t1 = rename t1 in
       let new_t2 = rename t2 in
       MLSubType.Function (new_t1, new_t2)
    | MLSubType.Record lbls ->
       MLSubType.Record (List.map (fun (lbl, v) -> (lbl, rename v)) lbls)
    | MLSubType.Recursive (var, t) ->
       let new_var = (string_of_int !counter) in
       counter := !counter + 1;
       renaming := add var new_var !renaming;
       let new_t = rename t in
       MLSubType.Recursive (new_var, new_t)
    | MLSubType.Variable var ->
       let new_var = (string_of_int !counter) in
       counter := !counter + 1;
       renaming := add var new_var !renaming;
       MLSubType.Variable new_var
                         
  in rename (go t Positive PSet.empty)
  

