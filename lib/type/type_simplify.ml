open Data


module SMap = Map.Make(String)



module CompactType = struct

  (* A compact type represents either a union or intersection of types. Note
  that, while primitive types are a list, there is only one 'instance' of a
  record or function type, as will automatically merge any which occur within
  the compact type *) 
  type compact_type = 
    {vars: VarStateSet.t;
     prims: PrimSet.t;
     rcd: (compact_type SMap.t) option; (* todo: see if sored map can take
    multiple values in Scala *)
     func: (compact_type * compact_type) option}

  (* Shorthand for use outside the module *)
  type t = compact_type

  (* The empty compact type *)
  let empty = {vars = VarStateSet.empty;
               prims = PrimSet.empty;
               rcd = None;
               func = None}

  (* A series of helper functions:
     + option_merge will merge two values in an option with a given function, or
       return None if either option contains a value. 
     + option_zip is a convenience wrapper around option which will place two
       values in an option into a tuple
     + merge_map
*)
  let option_merge (f: 'a -> 'b -> 'c )
        (o1 : 'a option) (o2 : 'b option) : 'c option = 
    Option.join (Option.map (fun x -> Option.map (fun y -> f x y) o2) o1)

  let option_zip (o1 : 'a option) (o2 : 'b option) : ('a * 'b) option =  
    option_merge (fun x y -> (x, y)) o1 o2

  let merge_map (f: 'a -> 'b -> 'c) : 'a SMap.t -> 'b SMap.t -> 'c SMap.t =
    SMap.merge (fun _ -> (option_merge f))

  let rec merge pol (lhs : compact_type) (rhs : compact_type) : compact_type  =
    let recd : (compact_type SMap.t) option
      = Option.map 
                (fun ((lrec : t SMap.t), (rrec : t SMap.t)) : compact_type SMap.t->
                  if pol = Positive then
                    (* TODO: same semantics both if & else branches?? *) 
                    SMap.merge (fun _ v1 v2 ->
                        (* TODO: this may be an error!!! *)
                        option_merge (fun x y -> merge pol x y) v1 v2)
                      lrec rrec
                  else
                    merge_map (merge pol) lrec rrec)
                (option_zip lhs.rcd rhs.rcd) in
    let funcn = Option.map
                (fun ((l0, r0), (l1, r1)) ->
                  (merge (inv pol) l0 l1, merge pol r0 r1))
                (option_zip lhs.func rhs.func) in
    {vars = VarStateSet.union lhs.vars rhs.vars;
     prims = PrimSet.union lhs.prims rhs.prims;
     rcd = recd;
     func = funcn}

end

module VarSet = Set.Make(CompVarSt)
module VarMap = Map.Make(CompVarSt)
module PVarMap = Map.Make(CompPolVar)
module PVarSet = Set.Make(CompPolVar)


module CompactTypeScheme = struct
  (* IMPORTS *)
  type simple_type = Data.simple_type
  type variable_state = Data.variable_state

  type compact_type_scheme = 
    { term: CompactType.t;
    (* Record bounds of recursive type variables *)
      rec_vars: CompactType.t VarMap.t }
  (* Alias for use outside the module *)
  type t = compact_type_scheme



  let compact_type (ty: simple_type) : compact_type_scheme =
    let recursive : (variable_state PVarMap.t) ref = ref (PVarMap.empty) in
    let rec_vars = ref VarMap.empty in
    let empty = CompactType.empty in

    let rec go ty pol parents in_process = 
      match ty with
      | Primitive p -> {empty with prims = PrimSet.singleton p}
      | Function (l, r) -> 
         {empty with func = Some (go l (inv pol) VarSet.empty in_process,
                                 go r pol VarSet.empty in_process)}
      | Record fs ->
         (* Map f over the second *)
         let mk_map (f : 'a -> 'b) (lst : (string * 'a) list) = 
           SMap.of_seq (List.to_seq (List.map (fun (key, value) -> (key, f value
                                       )) lst)) in
         {empty with rcd = Some (mk_map (fun ty ->
                                     (go ty pol VarSet.empty in_process))
                                   fs) }
      | Variable tv ->

         let tv_pol = (tv, pol) in
         if PVarSet.mem tv_pol in_process then
           if VarSet.mem tv parents then
             empty
           else
             {empty with
               vars = match PVarMap.find_opt tv_pol !recursive with
                      | Some x -> VarStateSet.singleton x
                      | None ->
                         let fv = (fresh_var 0) in
                         recursive := PVarMap.add tv_pol fv !recursive;
                         VarStateSet.singleton fv}
         else 
         let bounds = match pol with
           | Positive -> tv.lower_bounds
           | Negative -> tv.upper_bounds in
         let bound = List.fold_left
                       (CompactType.merge pol) {empty with vars = (VarStateSet.singleton tv)}
                       (List.map (fun b -> go b pol (VarSet.add tv parents) in_process) bounds) in
         match PVarMap.find_opt tv_pol !recursive with
         | Some v ->
            rec_vars := VarMap.add v bound !rec_vars;
            {empty with vars = VarStateSet.singleton v} 
         | None -> bound in

    { term = go ty Positive VarSet.empty PVarSet.empty;
      rec_vars = !rec_vars }

  



end
