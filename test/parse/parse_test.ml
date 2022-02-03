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
  (* Variables *)
  parseTest "paren_var" (Var "x") "(x)";
  parseTest "paren_int" (Int 10) "(10)";

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

  parseTest "if_complex1"
    (If (Op (Gre, Int 1, Op (Add, Int 2, Int 3)),
         Fun ("x", Op (Add, Var "x", Int 2)),
         If (Bool true, Int 2, Int 3)))
    "if 1 > 2 + 3 then fun x -> x + 2 else if true then 2 else 3";

  (* parseTest "if_complex2" *)
  (*   (If (Op (Gre, Int 1, Op (Add, Int 2, Int 3)), *)
  (*        Apply(Var "f", Int 3), *)
  (*        Op(Add, Int 5, Int 2))) *)
  (*   "if (100) then 3 else 5 + 2"; *)


  (* Let Expression *)
  parseTest "let_simple"
    (Let ("x", Bool true, Var "x"))
    "let x = true in x";

  (* Complex Let Expression: Parenthesised Expression *)
  parseTest "let_complex_paren_assign"
    (Let ("f",
          Int 10,
          Var "f"))
    "let f = (10) in f";

  parseTest "let_complex_body"
    (Let ("f",
          Int 10,
          Op (Add, Var "f", Int 12)))
    "let f = 10 in (f + 12)";

  (* (\* Complex Let Expression: assign function to variable *\) *)
  (* parseTest "let_complex_fun" *)
  (*   (Let ("f", *)
  (*         Fun ("y", Op (Add, Var "y", Int 2)), *)
  (*         Apply(Var "f", Int 3))) *)
  (*   "let f = (fun y -> y + 2) in f 3"; *)

  (* (\* Complex Let Expression: assign if to variable*\) *)
  (* parseTest "let_complex_fun" *)
  (*   (Let ("f", *)
  (*         If (Bool true, Int 2, Int 3), *)
  (*         Var "f")) *)
  (*   "let f =(if true then 2 else 3)in f"; *)

  (* Let Rec Expression *)
  parseTest "let_rec_simple"
    (LetRec ("x", Int 2, Var "x"))
    "let rec x = 2 in x";

  (* Lambda Expression *)
  parseTest "fun_simple"
    (Fun ("x", Var "x"))
    "fun x -> x";

  (* Lambda Expression *)
  parseTest "fun_complex"
    (Fun ("x", Op(Add, Var "x", Int 2)))
    "fun x -> x + 2";

  (* Function Application *)
  parseTest "app_simple"
    (Apply (Var "f", Var "x"))
    "f x"
    ]


let _ = run_test_tt_main tests
