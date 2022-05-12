open Angstrom

type op = Add | Sub | Mul | Div | Les | Gre | Eql | And | Or ;;

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
(* let special_chars = ['+'; '-'; '*'; '/'; '>'; '='; ','; '{'; '}'; '('; ')'] *)

let string_of_op = function
  | Add -> "Add"     
  | Sub -> "Sub"     
  | Mul -> "Mul"     
  | Div -> "Div"
  | Les -> "Les"
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
                                   soe e2 ^ ")"
  | Fun (var, e) -> "Fun (" ^ var ^ ", " ^ soe e ^ ")"

  | Apply (e1, e2) -> "Apply (" ^ soe e1 ^ ", " ^ soe e2 ^ ")"












(* PARSER PRIMITIVES *)

(* helper predicates *)
let is_alpha = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false

let is_alpha_num = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '0' .. '9' -> true
  | _ -> false

let is_numeric = function
  | '0' .. '9' -> true
  | _ -> false

let is_whitespace = function
  | '\t' | '\n' |' ' -> true
  | _ -> false

(* Parsers which consume Whitespace *)
let pWhitespace = take_while is_whitespace
let pWhitespace1 = take_while1 is_whitespace


(* define functions to work on parsers *)
let token str = string str <* pWhitespace1
let stoken str = string str <* pWhitespace
let toTok p = p <* pWhitespace
let parens p = stoken "(" *> pWhitespace *> p <* pWhitespace <* stoken ")"

(* define the parsers themselves *)

(* These simple combinators will parse '+', '-' etc. and output the resultant *)
(* 'partially-applied' constructor, i.e. parsing '+' would output a lambda *)
(* function equivalent to 'fun e1 e2 -> Op (Add, e1, e2)' *)


let mkBinParser str opType =
  toTok (string str *> return (fun x y -> Op (opType, x, y)))

let pAdd = mkBinParser "+" Add
let pSub = mkBinParser "-" Sub
let pMul = mkBinParser "*" Mul
let pDiv = mkBinParser "/" Div
let pLes = mkBinParser "<" Les
let pGre = mkBinParser ">" Gre
let pEql = mkBinParser "=" Eql
let pAnd = mkBinParser "and" And
let pOr  = mkBinParser "or" Or

(* simple integer and boolean literal parsers - looks for strings of the form *)
(* [0-9]/true-false, then converts to an equivalent Int/Bool-expression, *)
(* i.e. "45" becomes 'Int 45' and "true" becomes 'Bool true' *)  
let pInteger =
  let posInt = toTok (take_while1 is_numeric >>| int_of_string) in
  let negInt = char '-' *> posInt >>| (fun n -> -n) in 
  (posInt <|> negInt) >>| (fun n -> Int n)
  

let pBool =
  let pTrue = (string "true" *> return (Bool true))
  and pFalse = (string "false" *> return (Bool false))
  in toTok (pTrue <|> pFalse)

(* the pVarStr parses a variable name into a string. It is relatively simple:
 * 1.  *)

let pVarStr : string t =
  let rec elem = function
    | (x, y::_) when x = y -> true
    | (_, []) -> false
    | (x, _::xs) -> elem (x, xs) in
  toTok (satisfy is_alpha >>=
    (fun char ->
      take_while (is_alpha_num) >>=
        (fun string ->
          let result = String.make 1 char ^ string in
          if elem (result, reserved_words) then
            fail (Format.sprintf "unexpected " ^ result)
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
    let recordEntry = (pVarStr <* stoken "=") >>= 
      (fun varName -> expr >>| (fun (expr: expr) -> (varName, expr)))
    in sep_by (toTok (char ',')) recordEntry
  in
  (stoken "{" *> recordList <* stoken "}") >>| (fun list -> Record list)

                         
let pBinary expr : expr t =
  (* Utility for constructing binary operator parsers *)
  let mkBin e op =
    let rec go acc =
      choice[
          (lift2 (fun f x -> f acc x) op e >>= go);
          (lift (fun f -> Apply (acc, f)) e >>= go);
          return acc] in
    e >>= go  in

  (* Standard Operators: +, -, etc. according to standard precedence: * and / 
   * bind tighter than + and - bind tighter than > and = bind tighter than 'and' 
   * and 'or' *)
  let factor = choice [(parens expr); pInteger; pBool; pAccess; pVar; pRecord expr] in
  let term   = mkBin factor (pMul <|> pDiv) in
  let arith  = mkBin term (pAdd <|> pSub) in
  let logic  = mkBin arith (pGre <|> pEql <|> pLes) in
  mkBin logic (pAnd <|> pOr)

let pIf expr : expr t =
  (token "if" *> expr) >>=
    (fun cond -> (token "then" *> expr) >>=
    (fun e1   -> (token "else" *> expr) >>|
    (fun e2   -> If (cond, e1, e2))))

let mkpLet keyword ctor expr : expr t =
  (token keyword *> pVarStr) >>=
    (fun var -> (token "=" *> expr <* pWhitespace) >>=
    (fun e1  -> (token "in" *> expr) >>|
    (fun e2  -> ctor var e1 e2)))

let pLet = mkpLet "let" (fun v e1 e2 -> Let (v, e1, e2))
let pLetRec = mkpLet "let rec" (fun v e1 e2 -> LetRec (v, e1, e2))

let pFun expr : expr t =
  (token "fun" *> pVarStr) >>=
    (fun var -> (token "->" *> expr) >>|
    (fun e   -> Fun (var, e)))

(* parser for expressions *)

let pExpr : expr t =
  pWhitespace *>
  fix (fun expr ->
    choice [
        pIf expr;
        pLet expr;
        pLetRec expr;
        pRecord expr;
        pFun expr;
        pBinary expr;
    ])


(* let pExpr : expr t = *)
(*   pWhitespace *> *)
(*   fix (fun expr -> *)
(*   let rec buildApply = function *)
(*     | [] -> Int 0 *)
(*     | [x] -> x *)
(*     | (x :: xs) -> Apply (buildApply xs, x) in *)
(*   let build_single_expr = *)
(*     choice [(\* parens expr; *\) *)
(*         pIf expr; *)
(*         pLet expr; *)
(*         pLetRec expr; *)
(*         pRecord expr; *)
(*         pFun expr; *)
(*         pBinary expr] *)
(*   in *)
(*   (many1 (build_single_expr <* pWhitespace)) >>| *)
(*     (fun x -> buildApply (List.rev x))) *)



let pProgram =
  pWhitespace *> sep_by (token ";;") pExpr

let expr_of_string (str:string) : expr =
  match parse_string ~consume:All (pExpr) str with
  | Ok v      -> v
  | Error msg -> failwith msg

let expr_of_string_opt (str:string) : expr option=
  match parse_string ~consume:All (pExpr) str with
  | Ok v      -> Some v
  | Error _   -> None

let prog_of_string (str:string) : expr list =
  match parse_string ~consume:All (pProgram) str with
  | Ok v      -> v
  | Error msg -> failwith ("ParseError: " ^ msg)

let prog_of_string_opt (str:string) : (expr list) option =
  match parse_string ~consume:All (pProgram) str with
  | Ok v      -> Some v
  | Error _   -> None
