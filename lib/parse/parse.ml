open Angstrom

type op = Add | Sub | Mul | Div | Gre | Eql | And | Or ;;

type expr
  = Int of int
  | Bool of bool
  | Var of string
  | Access of string
  | Record of (string * expr) list
  | Op of op * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | Fun of string * expr
  | Apply of expr * expr;;

let reserved_words = ["let"; "rec"; "in"; "and"; "or"; "if"; "else"; "then"]

let string_of_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Gre -> "Gre"
  | Eql -> "Eql"
  | And -> "And"
  | Or  -> "Or"

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
                        string_of_op o ^ ", " ^
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
                                   soe e2 ^ "("
  | Fun (var, e) -> "Fun (" ^ var ^ ", " ^ soe e ^ ")"

  | Apply (e1, e2) -> "Apply (" ^ soe e1 ^ ", " ^ soe e2 ^ ")"






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
let pAnd = mkBinParser "and" And
let pOr  = mkBinParser "or" Or

(* simple integer and boolean literal parsers - looks for strings of the form *)
(* [0-9]/true-false, then converts to an equivalent Int/Bool-expression, *)
(* i.e. "45" becomes 'Int 45' and "true" becomes 'Bool true' *)  
let pInteger =
  toTok (take_while1 (function '0' .. '9' -> true | _ -> false) >>|
           (fun str -> Int (int_of_string str)))

let pBool =
  let pTrue = (string "true" *> return (Bool true))
  and pFalse = (string "false" *> return (Bool false))
  in toTok (pTrue <|> pFalse)

let pVarStr : string t =
  let rec elem = function
    | (x, y::_) when x = y -> true
    | (_, []) -> false
    | (x, _::xs) -> elem (x, xs) in
  toTok (satisfy (function 'a' .. 'z' -> true | _ -> false) >>=
    (fun char ->
      take_while (function
          | '0' .. '9' -> true
          | 'a' .. 'z' -> true
          | 'A' .. 'Z' -> true
          | _ -> false) >>=
        (fun string ->
          let result = String.make 1 char ^ string in
          if elem (result, reserved_words) then
            fail (Format.sprintf "used reserved keyword")
          else
            return result)))

let pVar : expr t = pVarStr >>| (fun var -> Var var)

let pAccess : expr t = char '#' *> pVarStr >>|  (fun var -> Access var)

(* These are parsers for the more complex expressions: if/else, let, etc. In *)
(* angstrom, recursive parsers are constructed via the fixpoint, so each of *)
(* these are phrased as functions which take in a parser called expr - the *)
(* parser which parses all expressions *)

let pRecord expr : expr t =
  let recordList = 
    let recordEntry = (pVarStr <* string "=" <* pWhitespace) >>= 
      (fun varName -> expr >>| (fun (expr: expr) -> (varName, expr)))
    in sep_by (toTok (char ',')) recordEntry
  in
  (string "{" *> pWhitespace *> recordList <* string "}") >>| (fun list -> Record list)

                         
let pBinary expr : expr t =
  (* Utility for constructing binary operator parsers *)
  let mkBin e op =
    let rec go acc =
      (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
    e >>= fun init -> go init in

  (* Standard Operators: +, -, etc. according to standard precedence: * and / *)
  (* bind tighter than + and - bind tighter than > and =* bind tighter than and *)
  (* and or *)
  let factor = parens expr <|> pInteger <|> pBool <|> pVar in
  let term   = mkBin factor (pMul <|> pDiv) in
  let arith  = mkBin term (pAdd <|> pSub) in
  let logic  = mkBin arith (pGre <|> pEql) in
  mkBin logic (pAnd <|> pOr)

let pIf expr : expr t =
  (token "if" *> expr) >>=
    (fun cond -> (token "then" *> expr) >>=
    (fun e1   -> (token "else" *> expr) >>|
    (fun e2   -> If (cond, e1, e2))))

let pLet expr : expr t =
  let letBody ctor = pVarStr >>=
    (fun var -> (token "=" *> expr) >>=
    (fun e1  -> (token "in" *> expr) >>|
    (fun e2  -> ctor var e1 e2)))
  in 
  (token "let" *> letBody (fun v e1 e2 -> Let (v, e1, e2)))
  <|>
  (token "let rec" *> letBody (fun v e1 e2 -> LetRec (v, e1, e2)))

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
              pRecord expr;
              pBinary expr;
              pAccess;
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
  | Error msg -> failwith ("ParseError: " ^ msg)
