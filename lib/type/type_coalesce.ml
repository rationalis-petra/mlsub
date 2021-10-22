
open Data
(* Use a functor to generate a set that stores the type simple_type *)
(* simple_type. For this to work, simple_type has to be comparable, hence the *)
(* large anonymous module. *)

module PSet = Set.Make(
                  struct
                    type t = variable_state * polarity

                    let compare (v1, p1) (v2, p2) = 
                      match Comparisons.compare_varstate v1 v2 with 
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
  let recursive : (polar_variable, mlsub_type) Hashtbl.t  = Hashtbl.create 10 in 
  let rec go t polar in_process =
    match t with
    (* Primitive is the base case *)
    | Primitive p -> PrimitiveType p

    (* Function & Record are the simple recursive cases *)
    | Function (l, r) -> FunctionType ((go l (invert polar) in_process),
                                       go r polar in_process)
    | Record xs ->
       RecordType (List.map (fun (n, t) -> (n, go t polar in_process))
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
               let x = VariableType (fresh_var#unique_name ()) in
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
                  | Positive -> (fun x y -> Union (x, y))
                  | Negative -> (fun x y -> Intersection (x, y)) in
          let res = List.fold_left mrg (VariableType (v_as_type v)) bound_types in
       (* Finally, lookup this variable name in the recursive map *)
          match (Hashtbl.find_opt recursive vpol) with
          | Some (VariableType n) -> RecursiveType(n, res)
          | Some _ ->
             raise (RecLookup "non-variable type in recursive in type_coalesce")
          | None -> res
                         
  in go t Positive PSet.empty
  

