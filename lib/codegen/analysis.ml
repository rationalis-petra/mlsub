open Data
open Data.AST

let rec calc_uni_record (expr : expr) : StrSet.t = 
  match expr with
  | Int _ | Bool _ | Var _ -> StrSet.empty
  | Access str -> StrSet.singleton str
  | Record lst -> StrSet.of_list (List.map (fun (x, _) -> x) lst)
  | Op (_, e1, e2) | Let (_, e1, e2) | Apply (e1, e2) ->
     StrSet.union (calc_uni_record e1) (calc_uni_record e2)
  | If (e1, e2, e3) -> 
     StrSet.union
       (StrSet.union (calc_uni_record e1) (calc_uni_record e2))
       (calc_uni_record e3)
  | Fun (_, e) -> calc_uni_record e


