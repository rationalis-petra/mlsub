
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
    | Apply of expr * expr;;

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
       Let (var, expr_of_pexpr e2, expr_of_pexpr e1)
    | Parse.LetRec (var, e1, e2) ->
       Let (var, expr_of_pexpr e2, expr_of_pexpr e1)
    | Parse.Fun (v, e) -> Fun (v, expr_of_pexpr e)
    | Parse.Apply (e1, e2) ->
       Apply (expr_of_pexpr e1, expr_of_pexpr e2)

end

