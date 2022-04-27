
module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

module AST = struct
  type pexpr = Parse.expr
  type op = Parse.op

  type expr
    = Int of int
    | Bool of bool
    | Var of string
    | Access of string
    | Record of (string * expr) list
    | Op of op * expr * expr
    | If of expr * expr * expr
    | Fun of string * expr
    | Let of string * expr * expr
    | LetRec of string * expr * expr
    | Apply of expr * expr;;


let rec string_of_expr =
  let soe = string_of_expr in
  function
  | Int i -> "Int " ^ string_of_int i
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | Var str -> "Var " ^ str
  | Access str -> "Access" ^ str
  | Record lst ->
      let rec list_to_string = function
        | [] -> ""
        | [(name, expr)] -> name ^ " = " ^ soe expr
        | ((name, expr)::xs) -> name ^
                                  " = " ^
                                    soe expr ^
                                      ",\n" ^ list_to_string xs
      in "{" ^ list_to_string lst ^ "}"

  | Op (o, e1, e2) -> "Op (" ^
                        Parse.string_of_op o ^ ", " ^
                          soe e1 ^ ", "^
                            soe e2 ^ ")"
  | If (cond, e1, e2) -> "If (" ^
                           soe cond ^ ", " ^
                               soe e1 ^ ", " ^
                                   soe e2 ^ ")"
  | Let (var, e1, e2) -> "Let (" ^
                           var ^ ", " ^
                               soe e1 ^ ", " ^
                                   soe e2 ^ ")"
  | LetRec (var, e1, e2) -> "LetRec(" ^ var ^ ", " ^
                               soe e1 ^ ", " ^
                                   soe e2 ^ ")"
  | Fun (var, e) -> "Fun (" ^ var ^ ", " ^ soe e ^ ")"

  | Apply (e1, e2) -> "Apply (" ^ soe e1 ^ ", " ^ soe e2 ^ ")"


  let rec expr_of_pexpr = function
    | Parse.Int i -> Int i
    | Parse.Bool b -> Bool b
    | Parse.Var s -> Var s
    | Parse.Access s -> Access s
    | Parse.Record fields ->
       Record (List.map (fun (k, v) -> (k, expr_of_pexpr v)) fields)
    | Parse.Op (op, e1, e2) -> Op (op, expr_of_pexpr e1, expr_of_pexpr e2)
    | Parse.If (e1, e2, e3) ->
       If (expr_of_pexpr e1, expr_of_pexpr e2, expr_of_pexpr e3)
    | Parse.Let (var, e1, e2) ->
       Let (var, expr_of_pexpr e1, expr_of_pexpr e2)
    | Parse.LetRec (var, e1, e2) ->
       LetRec (var, expr_of_pexpr e1, expr_of_pexpr e2)
    | Parse.Fun (v, e) -> Fun (v, expr_of_pexpr e)
    | Parse.Apply (e1, e2) ->
       Apply (expr_of_pexpr e1, expr_of_pexpr e2)

end


module AST_Evidence = struct
  type pexpr = Parse.expr
  type op = Parse.op

  type evidence =
    | NullEv
    (* | FieldEv of string  *)
    | RecordEv of (string * evidence) list
    | FunEv of evidence * evidence
    | VarEv of evstate
  and evstate =  {bounds : evidence ref; uid : int}
  let evst b i = {bounds = ref b; uid = i}

  type expr
    = Int of int
    | Bool of bool
    | Var of string
    | Access of string
    | Record of (string * expr) list
    | Op of op * expr * expr
    | If of expr * expr * expr
    | Fun of string * (evidence option) * expr
    | Let of string * (evidence option) * expr * expr
    | Apply of expr * expr  (* * (evidence option);; *)
end
