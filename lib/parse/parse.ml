open Angstrom

type op = Add | Sub | Mul | Div | Gre | Eql;;

type expr
  = Int of int
  | Bool of bool
  | Var of string
  | Op of op * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Fun of string * expr
  | Apply of expr * expr;;

let string_of_op = function
  | Add -> " + "
  | Sub -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Gre -> " > "
  | Eql -> " = "

let rec string_of_expr = function
  | Int (i) -> string_of_int i
  | Bool (true) -> "true"
  | Bool (false) -> "false"
  | Var str -> str
  | Op (o, e1, e2) -> "(" ^
                        string_of_expr e1 ^
                          string_of_op o ^
                            string_of_expr e2 ^
                              ")"
  | If (cond, e1, e2) -> "if " ^
                           string_of_expr cond ^
                             " then " ^
                               string_of_expr e1 ^
                                 " else " ^
                                   string_of_expr e2
  | Let (var, e1, e2) -> "let " ^
                           var ^
                             " = " ^
                               string_of_expr e1 ^
                                 " in " ^
                                   string_of_expr e2
  | Fun (var, e) -> "fun " ^ var ^ " -> " ^ string_of_expr e
  | Apply (e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2






(* Parsers which consume Whitespace *)
let pWhitespace = take_while (function '\t' | '\n' |' ' -> true | _ -> false)
let pWhitespace1 = take_while1 (function '\t' | '\n' |' ' -> true | _ -> false)


(* define functions to work on parsers *)
let token str = string str <* pWhitespace1
let toTok p = p <* pWhitespace

let parens p = char '(' *> p <* char ')'



(* define the parsers themselves *)

(* These simple combinators will parse '+', '-' etc. and output the resultant *)
(* 'partially-applied' constructor, i.e. parsing '+' would output a lambda *)
(* function equivalent to 'fun e1 e2 -> Op (Add, e1, e2)' *)

(* here, mkop/mkint serve as utility constructors because constructors cannot be *)
(* partially-applied ಠ-ಠ *)
let mkBinParser str opType =
  toTok (string str *> return (fun x y -> Op (opType, x, y)))

let pAdd = mkBinParser "+" Add
let pSub = mkBinParser "-" Sub
let pMul = mkBinParser "*" Mul
let pDiv = mkBinParser "/" Div
let pGre = mkBinParser ">" Gre
let pEql = mkBinParser "=" Eql

(* simple integer and boolean literal parsers - looks for strings of the form *)
(* [0-9]/true-false, then converts to an equivalent Int/Bool-expression, *)
(* i.e. "45" becomes 'Int 45' and "true" becomes 'Bool true' *)  
let pInteger =
  toTok (take_while1 (function '0' .. '9' -> true | _ -> false) >>|
           (fun str -> Int (int_of_string str)))

let pBool =
  let pTrue = (string "true" *> return (Bool true))
  and pFalse = (string "false" *> return (Bool false))
  in pTrue <|> pFalse

let pVarStr : string t=
  toTok (satisfy (function 'a' .. 'z' -> true | _ -> false) >>=
    (fun char ->
      take_while (function
          | '0' .. '9' -> true
          | 'a' .. 'z' -> true
          | 'A' .. 'Z' -> true
          | _ -> false) >>|
    (fun string -> String.make 1 char ^ string)))

let pVar : expr t = pVarStr >>| (fun var -> Var var)

(* These are parsers for the more complex expressions: if/else, let, etc. In *)
(* angstrom, recursive parsers are constructed via the fixpoint, so each of *)
(* these are phrased as functions which take in a parser called expr - the *)
(* parser which parses all expressions *)
                         
let pBinary expr : expr t =
  (* Utility for constructing binary operator parsers *)
  let mkBin e op =
    let rec go acc =
      (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
    e >>= fun init -> go init in

  (* Standard Operators: +, -, etc. accordign to standard precedence: *, / *)
  (* tighter than +, - tigher than >, =*)
  let factor = parens expr <|> pInteger <|> pBool <|> pVar in
  let term   = mkBin factor (pMul <|> pDiv) in
  let arith  = mkBin term (pAdd <|> pSub) in
  mkBin arith (pGre <|> pEql)

let pIf expr : expr t =
  (token "if" *> expr) >>=
    (fun cond -> (token "then" *> expr) >>=
    (fun e1   -> (token "else" *> expr) >>|
    (fun e2   -> If (cond, e1, e2))))

let pLet expr : expr t = 
  (token "let" *> pVarStr) >>=
    (fun var -> (token "=" *> expr) >>=
    (fun e1  -> (token "in" *> expr) >>|
    (fun e2  -> Let (var, e1, e2))))

let pFun expr : expr t =
  (token "fun" *> pVarStr) >>=
    (fun var -> (token "->" *> expr) >>|
    (fun e   -> Fun (var, e)))

(* parser for expressions *)
let pExprNoApply : expr t =
  fix (fun expr ->
      choice [pIf expr;
              pLet expr;
              pFun expr;
              pBinary expr;
              pVar])

let pExpr =
  let rec buildApply = function
    | [] -> Int 0
    | [x] -> x
    | (x :: xs) -> Apply (x, buildApply xs) in
  pWhitespace *> many1 (pExprNoApply <* pWhitespace) >>| buildApply


let pProgram =
  pWhitespace *> sep_by (token ";;") pExpr

let expr_of_string (str:string) : expr =
  match parse_string ~consume:All (pExpr) str with
  | Ok v      -> v
  | Error msg -> failwith msg

let prog_of_string (str:string) : expr list =
  match parse_string ~consume:All (pProgram) str with
  | Ok v      -> v
  | Error msg -> failwith msg
