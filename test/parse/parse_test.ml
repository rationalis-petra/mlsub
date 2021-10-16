open Parse
open OUnit2

let parseTest name expr str = 
  name >:: (fun _ -> assert_equal expr (expr_of_string str))



let tests = "test suite for parsing" >::: [
  (* Boolean Literals *)
  parseTest "bool_true"  (Bool true) "true";
  parseTest "bool_false" (Bool false) "false";

  (* Integer Literals *)
  parseTest "integer"             (Int 24) "24";
  parseTest "negative_integer"    (Int (-24)) "-24";

  (* Variables *)
  parseTest "variable"              (Var "x") "x";
  parseTest "variable_capital"      (Var "Avar") "Avar";
  parseTest "variable_with_keyword" (Var "xlet") "xlet";
  parseTest "variable_with_bool"    (Var "xtrue") "xtrue";
  parseTest "variable_with_int"     (Var "x23") "x23";
  (* "variable_with_reserved" >>: (fun _ -> assert_raises ParseFailure  ) *)
  (*   (Var "then") *)
  (*   "x23"; *)

  parseTest "accessor"   (Access "x") "#x";

  (* RECORDS *)
  parseTest "record_simple"
    (Record [("x", (Int 1)); ("y", (Int 2))])
    "{x = 1, y = 2}";
  parseTest "record_spacing1"
    (Record [("x", (Int 1)); ("y", (Int 2))])
    "{  x =  1  ,  y   = 2  }";
  parseTest "record_spacing2"
    (Record [("x", (Int 1)); ("y", (Int 2))])
    "{x=1,y=2}";

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
    (If (Int 1, Int 2, Int 7))
    "if 1 then 2 else 7";
  parseTest "if_complex"
    (If (Op (Gre, Int 1, Op (Add, Int 2, Int 3)),
         Fun ("x", Op (Add, Var "x", Int 2)),
         If (Bool true, Int 2, Int 3)))
    "if 1 > 2 + 3 then fun x -> x + 2 else if true then 2 else 3";

  (* Let Expression *)
  parseTest "let_simple"
    (Let ("x", Bool true, Var "x"))
    "let x = true in x";

  (* Let Rec Expression *)
  parseTest "let_rec_simple"
    (LetRec ("x", Int 2, Var "x"))
    "let rec x = 2 in x";

  (* Lambda Expression *)
  parseTest "fun_simple"
    (Fun ("x", Var "x"))
    "fun x -> x";

  (* Lambda Expression *)
  parseTest "fun_simple"
    (Fun ("x", Var "x"))
    "fun x -> x";

  (* Function Application *)
  parseTest "app_simple"
    (Apply (Var "f", Var "x"))
    "f x"
    ]


let _ = run_test_tt_main tests
