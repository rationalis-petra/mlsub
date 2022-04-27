
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

val string_of_expr : expr -> string

val string_of_op : op -> string

val expr_of_string : string -> expr

val prog_of_string : string -> expr list
