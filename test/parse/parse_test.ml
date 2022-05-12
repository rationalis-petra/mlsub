open Parse
open OUnit2

let parseTest name expr str = 
  name >:: (fun _ -> assert_equal (Some expr) (expr_of_string_opt str))

let parseTestF name str = 
  name >:: (fun _ ->
    assert_equal None (expr_of_string_opt str))

let success_tests = "test suite for parsing" >::: [
  (* Boolean Literals *)
  parseTest "bool_true"  (Bool true) "true";
  parseTest "bool_false" (Bool false) "false";

  (* Integer Literals *)
  parseTest "integer"             (Int 24) "24";
  parseTest "negative_integer"    (Int (-24)) "-24";

  (* Variables *)
  parseTest "variable"              (Var "x") "x";
  parseTest "variable_capital"      (Var "Avar") "Avar";
  parseTest "variable_with_keyword" (Var "letx") "letx";
  parseTest "variable_with_bool"    (Var "xtrue") "xtrue";
  parseTest "variable_with_int"     (Var "x23") "x23";
  (* Accesses *)
  parseTest "access"              (Access "x") "#x";
  parseTest "access_capital"      (Access "Avar") "#Avar";
  parseTest "access_with_keyword" (Access "letx") "#letx";
  parseTest "access_with_bool"    (Access "xtrue") "#xtrue";
  parseTest "access_with_int"     (Access "x23") "#x23";

  (* Variables *)
  parseTest "paren_var" (Var "x") "(x)";
  parseTest "paren_int" (Int 10) "(10)";


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

  parseTest "bin_parens1"
    (Op (Add, Int 2, Int 3))
    "( 2 ) + ( 3 )";
  parseTest "bin_parens2"
    (Op (Add,
         (Op (Gre, Int 2, Int 3)),
         (Op (And, Int 4, Bool true))))
    "( 2 > 3 ) + (4 and true)";
  parseTest "bin_parens_3"
    (Op (Add, Op (Gre, Int 1, Int 2), Int 3))
    "(1 > 2) + 3";

  (* Binary/Function Precedence Tests *)
  parseTest "bin_fun_pred1"
    (Op (Add, Apply (Int 3, Int 2), Int 3))
    "( 3 2 ) + ( 3 )";
  parseTest "bin_fun_pred2"
    (Op (Add, Apply (Int 3, Int 2), Int 3))
    "3 2 + ( 3 )";

  (* If Expression *)
  parseTest "if_simple"
    (If (Int 1, Int 2, Int 7))
    "if 1 then 2 else 7";

  parseTest "if_with_binop"
    (If (Op (Add, Int 2, Int 3),
         Op (Sub, Int 3, Int 5),
         Op (And, Bool true, Bool false)))
    "if 2 + 3 then 3 - 5 else true and false";
  parseTest "if_with_app"
    (If (Apply (Var "f", Var "x"),
         Apply (Var "f", Var "y"),
         Apply (Var "f", Var "z")))
    "if f x then f y else f z";
  parseTest "if_complex1"
    (If (Op (Gre, Int 1, Op (Add, Int 2, Int 3)),
         Fun ("x", Op (Add, Var "x", Int 2)),
         If (Bool true, Int 2, Int 3)))
    "if 1 > 2 + 3 then fun x -> x + 2 else if true then 2 else 3";
  parseTest "if_complex2"
    (If (Op (Gre, Int 1, Op (Add, Int 2, Int 3)),
         Apply(Var "f", Int 3),
         Op(Add, Int 5, Int 2)))
    "if 1>2+3 then f 3 else 5 + 2";


  (* Let Expression *)
  parseTest "let_simple"
    (Let ("x", Bool true, Var "x"))
    "let x = true in x";

  (* Making the assigned expression more complex: *)
  parseTest "let_binop_assign"
    (Let ("f",
          Op (Add, Int 45, Int 12), 
          Var "f"))
    "let f = (45 + 12) in f";
  parseTest "let_paren_assign"
    (Let ("f",
          Int 10,
          Var "f"))
    "let f = (10) in f";
  parseTest "let_let_assign"
    (Let ("f",
          Let ("x", Int 3, Var "x"),
          Var "f"))
    "let f = let x = 3 in x in f";
  parseTest "let_fun_assign"
    (Let ("f",
          Let ("x", Int 3, Var "x"),
          Var "f"))
    "let f = let x = 3 in x in f";
  parseTest "let_if_assign"
    (Let ("f",
          If (Op (Eql, Var "x", Int 3), Var "x", Int 0),
          Var "f"))
    "let f = if x = 3 then x else 0 in f";
  parseTest "let_fun_assign"
    (Let ("f",
          Fun ("y", Op (Eql, Var "y", Int 10)),
          Var "f"))
    "let f = fun y -> y = 10 in f";

  (* Making the body more complex *)
  parseTest "let_binop_body"
    (Let ("f",
          Int 10,
          Op (Add, Var "f", Int 12)))
    "let f = 10 in (f + 12)";

  (* Complex Let Expression: assign function to variable *)
  parseTest "let_complex_fun"
    (Let ("f",
          Fun ("y", Op (Add, Var "y", Int 2)),
          Apply(Var "f", Int 3)))
    "let f = (fun y -> y + 2) in f 3";

  (* Complex Let Expression: assign if to variable*)
  parseTest "let_complex_fun"
    (Let ("f",
          If (Bool true, Int 2, Int 3),
          Var "f"))
    "let f = (if true then 2 else 3) in f";

  (* Let Rec Expression *)
  parseTest "let_rec_simple"
    (LetRec ("x", Int 2, Var "x"))
    "let rec x = 2 in x";

  (* Lambda Expression *)
  parseTest "fun_simple"
    (Fun ("x", Var "x"))
    "fun x -> x";
  parseTest "fun_binop"
    (Fun ("x", Op(Add, Var "x", Int 2)))
    "fun x -> x + 2";
  parseTest "fun_let"
    (Fun ("x", Let ("y", Int 3, Op (Add, Var "x", Var "y"))))
    "fun x -> let y = 3 in x + y";
  parseTest "fun_fun"
    (Fun ("x", Fun ("y", Op (Add, Var "x", Var "y"))))
    "fun x -> fun y -> x + y";
  parseTest "fun_rcd"
    (Fun ("x", Fun ("y", Record ["x", Var "x"; "y", Var "y"])))
    "fun x -> fun y -> {x = x, y = y}";

  (* Function Application *)
  parseTest "app_simple"
    (Apply (Var "f", Var "x"))
    "f x";
  parseTest "app_multi"
    (Apply ((Apply (Var "f", Var "x")), Var "y"))
    "f x y";
  parseTest "app_accessor"
    (Apply (Access "x", Var "rcd"))
    "#x rcd";
  parseTest "app_to_if"
    (Apply (Var "f",
          If (Int 0, Int 1, Int 2)))
    "f (if 0 then 1 else 2)";
  parseTest "app_in_let"
    (Let ("x",
          (Apply (Var "f", Var "y")),
          (Apply (Var "x", Var "z"))))
    "let x = f y in x z";
  parseTest "app_to_record"
    (Apply (Access "x",
            Record ["x", Int 3]))
    "#x {x = 3}";
  parseTest "app_to_if"
    (Apply (Var "x",
            If (Var "x", Var "y", Var "z")))
    "x (if x then y else z)";

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
  parseTest "record_spacing3"
    (Record [("x", Var "x"); ("y", Var "y")])
    "{x=x,y=y}";

  (* Records containing expressions *)
  parseTest "rcd_app"
    (Record
       ["x", (Apply (Var "f", Var "a"));
        "y", (Apply (Var "f", Var "b"))])
    "{x = f a, y = f b}";
  parseTest "record_if"
    (Record [("x", If (Var "x", Var"y", Var "z")); ("y", (Int 2))])
    "{x=if x then y else z,y=2}";
  parseTest "record_let"
    (Record [("x", Let ("x", Int 3, Var "x")); ("y", (Int 2))])
    "{x=let x = 3 in x,y=2}";
  parseTest "record_binop1"
    (Record [("x", Op (Add, Op (Mul, Int 2, Int 3), Int 4)); ("y", (Int 2))])
    "{x= 2 * 3 + 4,y=2}";
  parseTest "record_binop2"
    (Record [("y", (Int 2)); ("x", Op (Add, Op (Mul, Int 2, Int 3), Int 4))])
    "{y=2, x= 2 * 3 + 4}";
  parseTest "record_eqbin"
    (Record [("y", Op (Eql, Int 2, Int 3)); ("x", Int 4)])
    "{y= 2=3, x=4}";
    ]

let failure_tests = "test suite for parsing" >::: [
  (* Boolean Literals *)
  parseTestF "bad_numbers1" "34%";
  parseTestF "bad_numbers2" "34.4";

  parseTestF "if_app" "f if 2 then 3 else 4";
  parseTestF "if_incomplete" "if 3 then 4 else";

  parseTestF "record_incomplete" "{x = 3,}";
  parseTestF "record_incomplete2" "{x = 3, 5}";
  parseTestF "record_incomplete3" "{x = 3, y=}";
  parseTestF "record_badfield" "{2 = 3}";

  parseTestF "binop_noright" "2 +";
  parseTestF "binop_noleft" "+ 2";
  parseTestF "binop_nodouble" " 2 + + 2";
  parseTestF "binop_nodouble2" " 2 < + 2";

  parseTestF "let_app" "f let z = 3 in z";
  parseTestF "let_incomplete" "let x = 3";
  parseTestF "let_badfield" "let 2 = 3 in z";

  ]

let _ = run_test_tt_main success_tests
let _ = run_test_tt_main failure_tests
