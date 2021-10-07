open Angstrom

type op = Add | Sub | Mul | Div | Geq | Leq;;

type expr
  = Int of int
  | Bool of bool
  | Let of string * expr * expr
  | Fun of string * expr
  | Op of op * expr * expr
  | Apply of expr * expr;;

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Geq -> ">="
  | Leq -> "<="

let rec string_of_expr = function
  | Int (i) -> string_of_int i
  | Bool (true) -> "true"
  | Bool (false) -> "false"
  | Op (o, e1, e2) -> "(" ^
                        string_of_expr e1 ^
                          string_of_op o ^
                            string_of_expr e2 ^
                              ")"
  | _ -> "err"


(* define functions to work on parsers *)

let parens p = char '(' *> p <* char ')'

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

(* define the parsers themselves *)

(* These simple combinators will parse '+', '-' etc. and output the resultant *)
(* 'partially-applied' constructor, i.e. parsing '+' would output a lambda *)
(* function equivalent to 'fun e1 e2 -> Op (Add, e1, e2)' *)

(* here, mkop/mkint serve as utility constructors because constructors cannot be *)
(* partially-applied ಠ-ಠ *)
let mkop o x y = Op (o, x, y)
let mkint i = Int i
let pAdd = char '+' *> return (mkop Add)
let pSub = char '-' *> return (mkop Sub)
let pMul = char '*' *> return (mkop Mul)
let pDiv = char '/' *> return (mkop Div)
let pGeq = string ">=" *> return (mkop Geq)
let pLeq = string "<=" *> return (mkop Leq)

(* simple integer and boolean literal parsers - looks for strings of the form *)
(* [0-9]/true-false, then converts to an equivalent Int/Bool-expression, *)
(* i.e. "45" becomes 'Int 45' and "true" becomes 'Bool true' *)  
let pInteger =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>|
    (fun str -> Int (int_of_string str))

let pBool =
  let pTrue = (string "true" *> return (Bool true))
  and pFalse = (string "false" *> return (Bool false))
  in pTrue <|> pFalse
                         
(* parser for expressions involving binary operators, i.e. +|*|>=|... *)
let pExpr : expr t =
  fix (fun expr ->
    let factor = parens expr <|> pInteger <|> pBool in
    let term   = chainl1 factor (pMul <|> pDiv) in
    let arith  = chainl1 term (pAdd <|> pSub) in
    chainl1 arith (pGeq <|> pLeq))

let eval (str:string) : expr =
  match parse_string ~consume:All pExpr str with
  | Ok v      -> v
  | Error msg -> failwith msg
