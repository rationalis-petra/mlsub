open Data


module SMap = Map.Make(String)

let rec reduce_option f = function
  | [] -> None
  | x :: xs -> Option.map (fun y -> f x y) (reduce_option f xs)

(* take a function which merges an A and a B, then apply it to an A option and *)
(* a B option *)
let option_merge (f: 'a -> 'b -> 'c )
      (o1 : 'a option) (o2 : 'b option) : 'c option = 
  Option.join (Option.map (fun x -> Option.map (fun y -> f x y) o2) o1)

(* merge an A option and a B option into an (A, B) option *)
let option_zip (o1 : 'a option) (o2 : 'b option) : ('a * 'b) option =  
  option_merge (fun x y -> (x, y)) o1 o2

let merge_map (f: 'a -> 'b -> 'c) : 'a SMap.t -> 'b SMap.t -> 'c SMap.t =
  SMap.merge (fun _ -> (option_merge f))

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
      

  (* A series of helper functions:
     + option_merge will merge two values in an option with a given function, or
       return None if either option contains a value. 
     + option_zip is a convenience wrapper around option which will place two
       values in an option into a tuple
     + merge_map
*)


  let rec merge pol (lhs : compact_type) (rhs : compact_type) : compact_type  =
    let recd : (compact_type SMap.t) option
      = Option.map 
                (fun ((lrec : t SMap.t), (rrec : t SMap.t)) : compact_type SMap.t->
                  if pol = Positive then
                    (* TODO: same semantics both of & else branches?? *) 
                    SMap.merge (fun _ v1 v2 ->
                        (* TODO: this may be an error!!!  options for y/empty
                           empty type? *)
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

  let map_of_rcd (f : 'a -> 'b) (lst : (string * 'a) list) = 
    SMap.of_seq (List.to_seq (List.map (fun (key, value) ->
                                  (key, f value)) lst)) 


  let rec close_over (xs: VarStateSet.t) (f: variable_state -> VarStateSet.t): VarStateSet.t =
    close_over_cached VarStateSet.empty xs f

  and close_over_cached dne todo f: VarStateSet.t =
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



  let canonicalize_type (ty : simple_type) : compact_type_scheme =
    let recursive : (compact_polar_type, variable_state) Hashtbl.t = Hashtbl.create 10 in
    let rec_vars = ref VarMap.empty in
    
    (* Turn the outermost layer of a SimpleType into a CompactType, leaving type *)
    (* variables untransformed *)
    let rec go_outer ty pol =
      match ty with
      | Primitive p -> {empty with prims = PrimSet.singleton p}
      | Function (l, r) ->
         {empty with func = Some (go_outer l (inv pol), go_outer r pol)}
      | Record fs ->
         {empty with rcd = Some (map_of_rcd (fun t -> go_outer t pol) fs)}
      | Variable tv ->
         let tvs = close_over (VarSet.singleton tv)
                     (fun vs ->
                       let bounds = if pol = Positive then
                                      vs.lower_bounds
                                    else
                                      vs.upper_bounds in
                       VarSet.of_list (collect (function
                                           | Variable vs -> Some vs
                                           | _ -> None) bounds)) in
         {empty with vars = tvs} in

    let rec go1 (ty: CompactType.t) (pol : polarity) in_process = 
      if CompactType.is_empty ty then
        ty
      else 
        let pty = (ty, pol) in
        if CPTSet.mem pty in_process then
          let vars = 
            match Hashtbl.find_opt recursive pty with
            | Some x -> x
            | None -> 
               let fv = fresh_var 0 in
               (Hashtbl.add recursive pty fv;
                fv) in
          {empty with vars = VarSet.singleton vars}
        else 
          let bound =
            reduce_option
              (CompactType.merge pol)
              (List.flatten
                 (List.map
                    (fun tv ->
                      let bounds = if pol = Positive then
                                     tv.lower_bounds
                                   else
                                     tv.upper_bounds in 
                      List.map (function
                          | Variable _ -> empty
                          | b -> go_outer b pol) bounds)
                    (List.of_seq (VarSet.to_seq ty.vars)))) in
          let res = match bound with
            | Some x -> CompactType.merge pol ty x
            | None -> CompactType.merge pol ty empty in
          let ipn = CPTSet.add pty in_process in
          let adapted : CompactType.t = {
              vars = res.vars;
              prims = res.prims;
              rcd = Option.map (SMap.map (fun v -> go1 v pol ipn)) res.rcd;
              func = Option.map (fun (l, r) -> (go1 l (inv pol) ipn,
                                                go1 r pol ipn)) res.func} in
         match Hashtbl.find_opt recursive pty with
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


  let simplify_type (ctx : compact_type_scheme) : t_type = 
      begin
        VarStateSet.map
          (fun tv ->
                 all_vars := VarStateSet.add tv !all_vars;
               ty.vars;
               let new_occurances =
                 SimpleSet.of_seq (Seq.append
                                     (VarStateSet.to_seq tv) (List.to_seq
                                                                tv.primitive)) in
               (match Hashtbl.find_opt co_occurances (pol, tv) with
                | Some os -> SimpleSet.inter os new_occurances
                | None -> Hashtbl.add co_occurances (pol, tv) new_occurances);
               (* if tv is recursive, it will have bounds, which we also need
                * to process. *)
               Option.map
                 (fun b -> if not VarMap.mem tv rec_vars then
                             let go_later () = 
                               VarMap.rec_vars
                             in go_later ();
                           else ()

                 )

                 VarMap.find_opt tv ctx.rec_vars 
          )
          all_vars;
        let rec_ = Option.map (fun mp -> SMap.map (fun x -> go x pol)) ty.rcd in
        let fun_ = Option.map ((l, r) -> go l (inv pol), go r pol) ty.func in 
        fun () ->
        (* Note: original used flat_map *)
        let new_vars = (VarStateSet.map (fun tv ->
                            match Hashtbl.find_opt tv var_subst with
                            | Some(Some(tv2)) -> tv2 :: []
                            | Some(None) -> []
                            | None -> tv :: []
                          ) ty.vars) in
        (* let new_vars = ty.vars *)
        {vars = new_vars;
         prims = ty.prims;
         Option.map (SMap.map (fun x -> go x pol)) ty.rcd;
         func = Option.map (fun (l, r) -> go l (inv pol), go r pol) ty.func}
      end in
  
  let gone = go cty.term Positive in
  (* let () = print_endline "[occ] ${co_occurances}" *)
  (* let () = print_endline "[rec] ${rec_vars}" *)

  (* This stage of the simplification involves removing non-recursive variables
   * that only occur in positive or negative positions *) 
  VarStateSet.map
    (fun v ->
      if not (Hashtbl.mem var_subst v) then
           (
             (* print_endline("[v] ${Hashtbl.find co_occurancestrue v}
  ${Hashtbl.find co_occurances false v}")  *)
             if SimpleSet.for_all !(Hashtbl.find co_occurances (pol, v)) then
               begin
                 (* print_endline("[U] $w := $v") *)
               end
             else ()
           )
         else ()
    )
    !all_vars

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
      | (CompactType _, _) -> return 1
      | (_, Variable _) -> return -1
  end

  module CTOVBoolMap = 
    Map.Make(CompProd (CTOVComp) (Bool))

  (* the final stage in the process; this function takes a compacted type and
   * returns an immutable value of type mlsub_type  *)
  let coalesce_compact_type (cty : compact_type) : mlsub_type = 
    let go (ty : compact_type_or_variable) (pol : polarity)
          (in_process : (() -> type_variable) CTOVBoolMap.t) : mlsub_type = 
      match CTOVBoolMap.find_opt in_process with
      | Some y ->
         (let res = t () in
          (* print_endline "REC[$pol] $ty -> $res"; *) res) | None ->
         let is_recursive = ref false in 
         let res_ref = ref None in 
         (* the code will only try and access the value of res if the variable
          * is recursive. Therefore, we 'hide' it behind a lazily in a function
          * which will set is_recursive to true when it is called, and return
          * something of type mlsub_type *)
         let res () = 
             match (!res_ref) with
             | Some x -> x
             | None ->
               (is_recursive := true;
                res_ref := vst_to_mlsub_type
                             (match ty with
                              | Variable v -> v
                              | _ -> fresh_var 0);
               !res_ref) in
         let new_inprocess = CTOVBoolMap.add (ty, pol) (fun () -> v ())
                               in_process in
         let res = 
           match ty with
           | Variable tv -> Option.fold
                              (vst_to_mlsub_type tv)
                              (fun x -> go x pol)
                              (VarMap.find_opt tv cty.rec_vars)
           (* A compact_type is either a union of types or an intersection of
            * types, dependent on polarity. This code will recursively coalesce
            * the sub-structure of the compact type into a list, then fold over
            * that list with either a union or intersection, giving us our result *)
           | CompactType {vars, prims, rcd, func} ->
              let (extr, mrg) = if pol then
                                  (Bottom, Union)
                                else
                                  (Top, Intersection) in 
              let type_as_list = 
                List.fold_left (@) []
                  [List.map
                     (fun x -> go x pol)
                     (List.of_seq (VarStateSet.to_seq vars)); 
                   List.map (PrimitiveType) (List.of_seq (PrimSet.to_seq prims));
                   List.map
                     (fun fs -> RecordType (List.map
                                              (fun (n, v) -> (n, go v pol))
                                              (List.of_seq (SMap.to_seq fs))))
                     (List.of_seq (Option.to_seq rcd));
                   List.map
                     (fun (l, r) -> FunctionType (go l (inv pol), go r pol))
                     (List.of_seq (Option.to_seq func))] in

              match (List.fold_left (fun x opt ->
                         Option.fold None (fun y -> mrg (x, y))
                           opt) None type_as_list) with
              | Some x -> x
              | None -> extr
         in if !is_recursive then RecursiveType (v, res) else res
    in
    go cty.term Positive Map.empty
 
end
