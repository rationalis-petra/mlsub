open Data
open Data.AST

exception AnalysisException of string

module E = AST_Evidence


let rec calc_uni_record (expr : expr) : StrSet.t = 
  match expr with
  | Int _ | Bool _ | Var _ -> StrSet.empty
  | Access str -> StrSet.singleton str
  | Record lst -> StrSet.of_list (List.map (fun (x, _) -> x) lst)
  | Op (_, e1, e2) | Let (_, e1, e2) | LetRec (_, e1, e2) | Apply (e1, e2) ->
     StrSet.union (calc_uni_record e1) (calc_uni_record e2)
  | If (e1, e2, e3) -> 
     StrSet.union
       (StrSet.union (calc_uni_record e1) (calc_uni_record e2))
       (calc_uni_record e3)
  | Fun (_, e) -> calc_uni_record e



let ev_counter = ref 0
let fresh_ev_var () = 
  let var_ev = E.evst (E.RecordEv []) !ev_counter in
  ev_counter := !ev_counter + 1;
  E.VarEv var_ev

(* let constrain lhs rhs *)

let rec annotate_evidence (e : expr) symtable = 
  match e with
  | Int i -> (E.Int i, E.NullEv)
  | Bool b -> (E.Bool b, E.NullEv)
  | Var s -> (E.Var s, StrMap.find s symtable)
  | Access field ->
     (E.Access field,
      E.FunEv (E.RecordEv [(field, E.NullEv)], fresh_ev_var ()))
     
  | Record lst ->
     let (labels, terms) = List.split lst in
     let (fields, evdiences) = List.split (List.map (fun f -> annotate_evidence f symtable) terms) in
     (E.Record (List.combine labels fields), E.RecordEv (List.combine labels evdiences))
  | _ -> raise (AnalysisException "annotate_evidence incomplete")

  (* | Op (op, e1, e2) -> *)
  (*    E.Op (op, *)
  (*          (annotate_evidence e1 symtable), *)
  (*          (annotate_evidence e2 symtable)) *)
  (* | If (e1, e2, e3) -> *)
  (*    (E.If (annotate_evidence e1, *)
  (*           annotate_evidence e2, *)
  (*           annotate_evidence e3), (\* TODO *\) ) *)
  (* | Fun (str, e) -> *)
  (*    E.Fun (StrMap.add str fresh_ev_var) *)

  (*    StrSet.union (calc_uni_record e1) (calc_uni_record e2) *)
  (* | If (e1, e2, e3) ->  *)
  (*    StrSet.union *)
  (*      (StrSet.union (calc_uni_record e1) (calc_uni_record e2)) *)
  (*      (calc_uni_record e3) *)
  (* | Fun (_, e) -> calc_uni_record e *)
