open Parse
open OUnit2

exception Test of string

(* let () = raise (Test "hello") *)

(* let sum = foldl 0 + *)

let parseTest name expr str = 
  name >:: (fun _ -> assert_equal expr (expr_of_string str))



let tests = "test suite for sum" >::: [
  (* SIMPLE LITERALS *)
  parseTest "integer"    (Int 2) "2";
  parseTest "bool_true"  (Bool true) "true";
  parseTest "bool_false" (Bool false) "false";
  parseTest "variable"   (Var "x") "x";

  (* BINARY EXPRESSIONS *)
  parseTest "bin_simple"
    (Op (Add, Int 2, Int 3))
    "2 + 3"; 
  parseTest "bin_chained"
    (Op (Add, Op (Add, Int 2, Int 3), Int 3))
    "2 + 3 + 3"; 
  parseTest "bin_precedence1"
    (Op (Add, Op (Mul, Int 2, Int 10), Int 3))
    "2 * 10 + 3"; 
  parseTest "bin_precedence2"
    (Op (Sub, Op (Div, Int 2, Int 10), Int 3))
    "2 / 10 - 3"; 
  parseTest "bin_precedence3"
    (Op (Gre, Op (Div, Int 2, Int 10), Int 3))
    "2 / 10 > 3"; 
  parseTest "bin_precedence4"
    (Op (Or,
         Op (Gre, Op (Div, Int 2, Int 10), Int 3),
         Op (Eql, Int 6, Int 2)))
    "2 / 10 > 3 or 6 = 2"; 

  (* If Expression *)
  parseTest "if_simple"
    (If (Bool true, Int 3, Int 7))
    "if true then 3 else 7";

  (* RECORDS *)
  parseTest "record_simple"
    (Record [("x", (Int 1)); ("y", (Int 2))])
    "{x = 1, y = 2}";
  parseTest "record_spacing1"
    (Record [("x", (Int 1)); ("y", (Int 2))])
    "{  x =  1  ,  y   = 2  }";
  parseTest "record_spacing2"
    (Record [("x", (Int 1)); ("y", (Int 2))])
    "{x=1,y=2}"
    ]


let _ = run_test_tt_main tests
