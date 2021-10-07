
type op = Plus | Minus | Times | Div | Greq | Leq;;

type expr
  = EInt of int
  | EBool of bool
  | Let of string * expr * expr
  | Fun of string * expr
  | Op of expr * expr
  | Apply of expr * expr;;
  
