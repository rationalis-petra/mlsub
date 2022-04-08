open Data


module SMap = Map.Make(String)
let rec reduce_option f = function
  | [] -> None
  | [x] -> Some x
  | x :: xs -> Option.map (fun y -> f x y) (reduce_option f xs)

(* Given a function to merge two values of 'a and two options of 'a, either:
 * + 
 *)
let option_merge (f: 'a -> 'a -> 'a )
      (o1 : 'a option) (o2 : 'a option) : 'a option = 
  match (o1, o2) with
  | (Some l, Some r) -> Some (f l r)
  | (Some l, None) -> Some l
  | (None,  Some r) -> Some r
  | (None, None) -> None


let merge_map (f: 'a -> 'a -> 'a) : 'a SMap.t -> 'a SMap.t -> 'a SMap.t =
  SMap.merge (fun _ x y -> (option_merge f x y))


let rec collect (f : 'a -> 'b option) l =
  match l with
  | [] -> []
  | (x :: xs) ->
     match (f x) with
     | Some v -> v :: (collect f xs)
     | None -> collect f xs



module CompactType = struct

  (* A compact type represents either a union or intersection of types. Note
  that, while primitive types are a list, there is only one 'instance' of a
  record or function type, as will automatically merge any which occur within
  the compact type *)
  type compact_type = 
    {vars: VarStateSet.t;
     prims: PrimSet.t;
     rcd: (compact_type SMap.t) option; 
     func: (compact_type * compact_type) option}

  let rec to_str {vars; prims; rcd; func} = 
    let set_fold_str val_to_str v str  = 
      (val_to_str v) ^ "," ^ str  in
    let opt_to_str val_to_str = function
      | Some x -> "Some (" ^ (val_to_str x) ^ ")"
      | None -> "None" in
    "{vars = [" ^ (VarStateSet.fold (set_fold_str vst_to_str) vars "]\n") ^
      "prims = [" ^
        (PrimSet.fold (set_fold_str string_of_primitive) prims "]\n") ^
          "rcd = [" ^ (opt_to_str
                        (fun x ->
                          SMap.fold
                            (fun nam v str ->
                              nam ^ ":" ^ (to_str v) ^ "," ^ str)
                            x "]\n") rcd) ^ "]\n" ^
            "func = [" ^ (opt_to_str
                            (fun (x, y) ->
                              (to_str x) ^ " -> " ^
                                (to_str y))
                         func) ^ "]}"


  (* Shorthand for use outside the module *)
  type t = compact_type

  let is_empty {vars; prims; rcd; func} =
    VarStateSet.is_empty vars &&
      PrimSet.is_empty prims &&
        Option.is_none rcd &&
          Option.is_none func
          

  (* The empty compact type *)
  let empty = {vars = VarStateSet.empty;
               prims = PrimSet.empty;
               rcd = None;
               func = None}

  (* To make a map module *)
  let compare_pair c (x1, y1) (x2, y2) =
    match c x1 x2 with
    | 0 -> c y1 y2
    | n -> n

  let rec compare {vars=vars1; prims=prims1; rcd=rcd1; func=func1}
    {vars=vars2; prims=prims2; rcd=rcd2; func=func2} = 
    (* Sequence combinators *)
    let (>>) cmp func = 
      match cmp with
      | 0 -> func ()
      | n -> n in
    (VarStateSet.compare vars1 vars2) >>
    (fun () -> (PrimSet.compare prims1 prims2) >>
    (fun () -> (Option.compare (SMap.compare compare) rcd1 rcd2) >>
    (fun () -> (Option.compare (compare_pair compare) func1 func2))))
      


  (* TODO: check this function!! *)
  let rec merge pol (lhs : compact_type) (rhs : compact_type) : compact_type  =
    let recd : (compact_type SMap.t) option
      = option_merge
                (fun lrec rrec ->
                  match pol with
                  | Positive -> 
                     SMap.filter_map
                       (fun k v ->
                         Option.map (merge pol v) (SMap.find_opt k rrec))
                       lrec
                  | Negative -> merge_map (merge pol) lrec rrec)
                lhs.rcd rhs.rcd in
    let funcn = option_merge
                (fun (l0, r0) (l1, r1) ->
                  (merge (inv pol) l0 l1, merge pol r0 r1))
                lhs.func rhs.func in
    {vars = VarStateSet.union lhs.vars rhs.vars;
     prims = PrimSet.union lhs.prims rhs.prims;
     rcd = recd;
     func = funcn}




end

module VarSet = Set.Make(CompVarSt)
module VarMap = Map.Make(CompVarSt)
module PVarMap = Map.Make(CompPolVar)
module PVarSet = Set.Make(CompPolVar)

module PolarCompact = CompProd (CompactType) (CompPol)
module CPTMap = Map.Make(PolarCompact)
module CPTSet = Set.Make(PolarCompact)


module CompactTypeScheme = struct
  (* IMPORTS *)
  type simple_type = Data.simple_type
  type variable_state = Data.variable_state
  type compact_polar_type = CompactType.t * polarity

  type compact_type_scheme = 
    { term: CompactType.t;
    (* Record bounds of recursive type variables *)
      rec_vars: CompactType.t VarMap.t }
  (* Alias for use outside the module *)
  type t = compact_type_scheme

  (* Utility *)
  let compare cts1 cts2 =
    match CompactType.compare cts1.term cts2.term with
    | 0 -> VarMap.compare CompactType.compare cts1.rec_vars cts2.rec_vars
    | n -> n

    

  let to_str cts =  
    "{term : " ^ CompactType.to_str cts.term ^ 
      "\nrec_vars: " ^
        (VarMap.fold
           (fun _ _ b -> "var" ^ b) (* s ^ CompactType.compact_type_to_str a ^ b) *)
           cts.rec_vars
           "") ^ "}"

  let map_of_rcd (f : 'a -> 'b) (lst : (string * 'a) list) = 
    SMap.of_seq (List.to_seq (List.map (fun (key, value) ->
                                  (key, f value)) lst)) 


  (* Given a set xs : VarStateSet and a function f: variable_state ->
   * VarStateSet, produce a set y such that ∀a∈y. f(a) ⊆ y *)
  let close_over (xs: VarStateSet.t) (f: variable_state -> VarStateSet.t): VarStateSet.t =
    (* A helper for close_over*)
    let rec close_over_cached dne todo f: VarStateSet.t =
      if VarStateSet.is_empty todo then
        dne
      else 
        let flat_map f s = 
          VarStateSet.of_seq
            (Seq.flat_map
               (fun x -> VarStateSet.to_seq (f x))
               (VarStateSet.to_seq s)) in
        let new_done = VarStateSet.union dne todo in
        close_over_cached new_done (VarStateSet.diff (flat_map f todo)  new_done) f
    in
    close_over_cached VarStateSet.empty xs f
    
  let empty = CompactType.empty 



  let compact_type (ty: simple_type) : compact_type_scheme =
    let recursive : (variable_state PVarMap.t) ref = ref (PVarMap.empty) in
    let rec_vars = ref VarMap.empty in


    let rec go ty pol parents in_process = 
      match ty with
      | Primitive p -> {empty with prims = PrimSet.singleton p}
      | Function (l, r) -> 
         {empty with func = Some (go l (inv pol) VarSet.empty in_process,
                                 go r pol VarSet.empty in_process)}
      | Record fs ->
         (* Map f over the second *)
         {empty with rcd = Some (map_of_rcd (fun ty ->
                                     (go ty pol VarSet.empty in_process))
                                   fs)}
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


  (* The canonicalize_type function takes in a simple type and outputs *)
  let canonicalize_type (ty : simple_type) : compact_type_scheme =
    let recursive : (variable_state CPTMap.t) ref = ref CPTMap.empty in
    let rec_vars = ref VarMap.empty in
    
    (* Turn the outermost layer of a SimpleType into a CompactType, leaving type *)
    (* variables untransformed. Then, bounds will be  *)
    let rec go_outer (ty : simple_type) (pol : polarity) : CompactType.t =
      match ty with
      | Primitive p -> {empty with prims = PrimSet.singleton p}
      | Function (l, r) ->
         {empty with func = Some (go_outer l (inv pol), go_outer r pol)}
      | Record fs ->
         {empty with rcd = Some (map_of_rcd (fun t -> go_outer t pol) fs)}
      | Variable tv ->
         let tvs = close_over (VarSet.singleton tv)
                     (fun vs ->
                       let bounds = match pol with
                         | Positive -> vs.lower_bounds
                         | Negative -> vs.upper_bounds in
                       VarSet.of_list (collect (function
                                           | Variable vs -> Some vs
                                           | _ -> None) bounds)) in
         {empty with vars = tvs} in

    (* Run after go_outer, go_inner merges the bounds of all type variables *)
    let rec go1 (ty: CompactType.t) (pol : polarity) (in_process : CPTSet.t) = 
      let pty = (ty, pol) in
      if CompactType.is_empty ty then ty else
        if CPTSet.mem pty in_process then
          let vars = 
            match CPTMap.find_opt pty !recursive with
            | Some x -> x
            | None -> 
               let fv = fresh_var 0 in
               recursive := CPTMap.add pty fv !recursive;
               fv in
          {empty with vars = VarSet.singleton vars}
        else 
          let bound1 =
            reduce_option
              (CompactType.merge pol)
              (List.flatten
                 (List.map
                    (fun tv ->
                      let bounds = match pol with
                        | Positive -> 
                           tv.lower_bounds
                        | Negative -> tv.upper_bounds in
                      List.map (function
                          | Variable _ -> empty
                          | b -> go_outer b pol) bounds)
                    (List.of_seq (VarSet.to_seq ty.vars)))) in
               
          (* Bound is a compact type whose value corresponds to merging either
           * the upper bounds or lower bounds of tv, depending on polarity *)
          let bound = match bound1 with
            | Some x -> x
            | None ->
               empty in
          let res = CompactType.merge pol ty bound in 

          let new_inp = CPTSet.add pty in_process in
          let adapted : CompactType.t = {
              vars = res.vars;
              prims = res.prims;
              rcd = Option.map (SMap.map (fun v -> go1 v pol new_inp)) res.rcd;
              func = Option.map (fun (l, r) -> (go1 l (inv pol) new_inp,
                                                go1 r pol new_inp)) res.func} in
          match CPTMap.find_opt pty !recursive with
          | Some v ->
             rec_vars := VarMap.add v adapted (!rec_vars);
             {empty with vars = VarSet.singleton v}
          | None -> adapted in

    {term = go1 (go_outer ty Positive) Positive CPTSet.empty;
     rec_vars = !rec_vars} 

  
  (* The simplify_type function relies on two ideas: 
   -----------------------------------------------------------------------------
   The first is that if two types 'a and 'b always occur together in the
   positive (or negative) position, we can unify them. For example, if we see
   the type a ∧ b → (a, b), then we can replace it with c → (c, c). This works 

   Flatten what Parreaux refers to as "variable sandwiches". That is, for two
   types, e.g. a and int, we have both a ≤ int and a ≥ int. From this we can
   infer that a = int. In the type-expression, we will see this as a occurance
   of a ⊓ int in a positive position and a ⊔ int in a negative position. For
   example, a ⊓ int → a ⊔ int would simplify to just int → int.

   For more information, see section 4: Type simplification tradeoffs of the
   paper "The Simple Essence of Algebraic Subtyping" *) 

  module NegVarUidSet = Set.Make(
                         struct
                           type t = variable_state
                           let compare x y = Int.compare (-x.uid) (-y.uid)
                         end)
  module VarUidMap = Map.Make(
                         struct
                           type t = variable_state
                           let compare x y = Int.compare x.uid y.uid
                         end)

  

  let simplify_type (cty : compact_type_scheme) : compact_type_scheme = 
    let all_vars = ref (NegVarUidSet.of_seq
                          (Seq.map (fun (a, _) -> a) (VarMap.to_seq cty.rec_vars
                     ))) in 
    let rec_vars : (unit -> CompactType.t) VarUidMap.t ref = ref VarUidMap.empty in
    let co_occurences : (SimpleSet.t ref) PolVarMap.t ref = ref PolVarMap.empty
    in
    
    (* Filled up in analysis phase and used in reconstruction phase *)
    let var_subst : variable_state option VarMap.t ref = ref VarMap.empty in

    (* Perform the analysis, and return a lazy reconstruction *)
    let rec go (ty : CompactType.t) (pol : polarity) : unit -> CompactType.t = 
      VarStateSet.iter (fun tv ->
          all_vars := NegVarUidSet.add tv !all_vars;
          let new_occs = SimpleSet.of_seq(
                             Seq.append
                               (Seq.map
                                  (fun var -> Variable var)
                                  (VarSet.to_seq ty.vars))
                               (Seq.map
                                (fun prim -> Primitive prim)
                                (PrimSet.to_seq ty.prims))) in
          (match PolVarMap.find_opt (tv, pol) !co_occurences with
           | Some os ->
              os := SimpleSet.inter new_occs !os
           | None ->
              co_occurences := PolVarMap.add (tv, pol) (ref new_occs) !co_occurences);
          match VarMap.find_opt tv cty.rec_vars with
          | Some b -> if not (VarUidMap.mem tv !rec_vars) then
           (* TODO: Check if Lazy keyword impacts semantics...*)
                         (*Potential bug here*)
                         let rec go_later : unit -> CompactType.t =
                           (fun () ->
                             rec_vars :=
                               VarUidMap.add tv (fun () -> go_later ()) !rec_vars;
                             go b pol ()) in
                         rec_vars :=
                           VarUidMap.add tv (fun () -> go_later ()) !rec_vars;
                         (* ignore (go_later ()); *)
          | None -> ()

        )
        ty.vars;

      let rcd_ = Option.map (SMap.map (fun x -> go x pol)) ty.rcd in
      let func_ = Option.map (fun (l, r) -> (go l (inv pol), go r pol)) ty.func in


      (* Return thunk which will reconstruct the type *)
      (fun () ->
        let new_vars = List.flatten (List.map
                                       (fun tv ->
                                         match VarMap.find_opt tv !var_subst with 
                                         | Some(Some(tv2)) -> [tv2]
                                         | Some(None) -> []
                                         | None -> [tv])
                                       (List.of_seq (VarStateSet.to_seq ty.vars))) in
        {vars = VarStateSet.of_list (new_vars);
         prims = ty.prims;
         rcd = Option.map (SMap.map (fun x -> x ())) rcd_;
         func = Option.map (fun (l, r) -> (l (), r ())) func_
      }) in
    
    let gone = go cty.term Data.Positive in

    (* If a non-recursive variable occurs only in a positive (negative)
     * position, then we can replace it with Top (Bottom) by removing it   *)
    NegVarUidSet.iter
      (fun v0 ->
        if not (VarUidMap.mem v0 !rec_vars) then
          match (PolVarMap.find_opt (v0, Positive) !co_occurences,
                 PolVarMap.find_opt (v0, Negative) !co_occurences) with
          | (Some _, None) | (None, Some _) ->
             var_subst := VarMap.add v0 None !var_subst;
          | occ -> assert (occ != (None, None)))
    !all_vars;

    (* If two type variables, e.g. 'a and 'b always occur positively
      (resp. negatively)  along with some 'b  and vice versa, this means that
      the two are indistinguishable, and can therefore be unified. This section
      performs that unification *)
    let pols = [ Positive; Negative ] in
    NegVarUidSet.iter
      (fun v ->
        if not (VarMap.mem v !var_subst) then
          (List.iter
            (fun pol ->
              Option.iter (SimpleSet.iter (fun x -> match x with
                 | Variable w ->
                    (if (not (w.uid = v.uid)) &&
                          (not (VarMap.mem w !var_subst)) &&
                            ((VarUidMap.mem v !rec_vars) = (VarUidMap.mem w !rec_vars))
                     then
                       print_endline (vst_to_str v);
                       (if (match (PolVarMap.find_opt (w, pol) !co_occurences)
                           with
                           | None -> false
                           | Some s -> SimpleSet.mem (Variable v) !s)
                        then
                          var_subst := VarMap.add w (Some v) !var_subst);
                       (match VarUidMap.find_opt w !rec_vars with
                        | Some b_w ->
                           assert(not (PolVarMap.mem (w, inv pol) !co_occurences));
                           rec_vars := VarUidMap.remove w !rec_vars;
                           let b_v = VarUidMap.find v !rec_vars in
                           rec_vars := VarUidMap.add v
                                         (fun () ->
                                           CompactType.merge pol (b_v ()) (b_w ()))
                                         !rec_vars
                        | None ->
                           (* This has to be defined; otherwise we'd have removed
                            * it in the variable substitution phase!*)
                           (match PolVarMap.find_opt (w, inv pol) !co_occurences
                            with
                            | Some _ -> ()
                            | None -> print_endline "bad w, inv pol");
                           let w_co_ocss = PolVarMap.find (w, inv pol)
                                             !co_occurences in
                           let inplace = (PolVarMap.find (v, inv pol) !co_occurences)
                           in

                           inplace := (SimpleSet.filter (fun t ->
                                           (CompSimple.compare t (Variable v) = 0) ||
                                                SimpleSet.mem t !w_co_ocss)
                                         !(PolVarMap.find (v, inv pol) !
                                             co_occurences))))
                           
                 | Primitive atom ->
                    (match Option.map (fun x -> SimpleSet.mem (Primitive atom) !x)
                             (PolVarMap.find_opt (v, inv pol) !co_occurences) with
                       
                     | Some true ->
                        var_subst := VarMap.add v None !var_subst;
                     | _ -> ())
                 | _ -> ()))
             (Option.map (!) (PolVarMap.find_opt (v, pol) !co_occurences)))
            pols))
      !all_vars;
    {term = gone ();
     rec_vars = VarMap.map (fun f -> f ())
                  (VarMap.of_seq (VarUidMap.to_seq !rec_vars))}




 

  type compact_type_or_variable =
    | CompactType of CompactType.t
    | Variable of variable_state

  module CTOVComp = struct
    type t = compact_type_or_variable
    let compare ct1 ct2 = 
      match (ct1, ct2) with
      | (CompactType t1, CompactType t2) ->
         CompactType.compare t1 t2
      | (Variable v1, Variable v2) ->
         CompVarSt.compare v1 v2
      | (CompactType _, _) -> 1
      | (_, _) -> -1
  end

  module CTOVBoolMap = 
    Map.Make(CompProd (CTOVComp) (CompPol))

  (* the final stage in the process; this function takes a compacted type and
   * returns an immutable value of type mlsub_type  *)
  let coalesce_compact_type (cty : compact_type_scheme) : MLSubType.t = 
    let open MLSubType in
    let rec go (ty : compact_type_or_variable) (pol : polarity)
          (in_process : (unit -> mlsub_type) CTOVBoolMap.t) : mlsub_type = 
      match CTOVBoolMap.find_opt (ty, pol) in_process with
      | Some t ->
         (* A recursive type *)
         (let res = t () in
          res)
      | None ->
         let is_recursive = ref false in 
         (* the code will only try and access the value of v if the variable
          * is recursive. Therefore, we 'hide' it behind a lazy function
          * which will set is_recursive to true when it is called, and return
          * something of type mlsub_type *)
         let v_ref = ref None in 
         let v () = 
           match (!v_ref) with
           | Some x -> x
           | None ->
              (is_recursive := true;
               let new_res = 
                 (match ty with
                  | Variable v -> v
                  | _ -> fresh_var 0) in
               v_ref := Some new_res;
               new_res) in
         (* shadow the binding of in_process for this next block*)
         let in_process = CTOVBoolMap.add (ty, pol) (fun () ->
                                  vst_to_mlsub_type (v ()))
                               in_process in
         let res : mlsub_type = 
           match ty with
           | Variable tv ->
              (match (VarMap.find_opt tv cty.rec_vars) with
               | Some x -> go (CompactType x) pol in_process
               | None -> (vst_to_mlsub_type tv))
                              
           (* A compact_type is either a union of types or an intersection of
            * types, dependent on polarity. This code will recursively coalesce
            * the sub-structure of the compact type into a list, then fold over
            * that list with either a union or intersection, giving us our result *)
           | CompactType {vars; prims; rcd; func} ->
              let (extr, mrg) = if pol = Positive then
                                  (Bottom, fun x y -> Union (x, y))
                                else
                                  (Top, fun x y -> Intersection (x, y)) in 
              let type_as_list = 
                List.fold_left (@) []
                  [List.map
                     (fun x -> go (Variable x) pol in_process)
                     (List.of_seq (VarStateSet.to_seq vars)) ;
                   List.map
                     (fun x -> Primitive x)
                     (List.of_seq (PrimSet.to_seq prims));
                   List.map
                     (fun fs -> Record (List.map
                                              (fun (n, v)
                                               -> (n, go (CompactType v) pol in_process))
                                              (List.of_seq (SMap.to_seq fs))))
                     (List.of_seq (Option.to_seq rcd));
                   List.map
                     (fun (l, r) -> Function (go (CompactType l) (inv pol) in_process,
                                                  go (CompactType r) pol in_process))
                     (List.of_seq (Option.to_seq func))] in

              let rec type_opt_of_type_list = function
                | x :: (y :: xs) -> mrg x (type_opt_of_type_list (y :: xs))
                | x :: [] -> x
                | [] -> extr in
              (* the final value of res *)
              type_opt_of_type_list type_as_list in

         if !is_recursive then Recursive (vst_to_str (v ()), res) else res
    in
    go (CompactType cty.term) Positive CTOVBoolMap.empty
  
end
