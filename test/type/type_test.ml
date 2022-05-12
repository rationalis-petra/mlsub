open Type
open Type__.Data.MLSubType
open OUnit2
module P = Parse

let typeTest name expr mlsub_type = 
  (* make sure to reset the varUidCounter !!*)
  name >:: (fun _ -> assert_equal mlsub_type (infer_type expr))

let typeTestFail name expr  = 
  (* make sure to reset the varUidCounter !!*)
  name >:: (fun _ -> assert_equal None (infer_type_opt expr))

let typeTestPrint name expr mlsub_type = 
  let ty = (infer_type expr) in
  print_endline (string_of_type ty);
  print_endline (string_of_type mlsub_type);
  name >:: (fun _ -> assert_equal mlsub_type ty)

let typeTestPrintSeq name (expr : P.expr) _ =
  let _ = (infer_type_stepped expr) in
  (* print_endline (string_of_type ty); *)
  (* print_endline (string_of_type ty); *)
  name >:: (fun _ -> assert_equal true true)


let a0  = Variable "ɑ0"
let a1  = Variable "ɑ1"
let a2  = Variable "ɑ2"
let a3  = Variable "ɑ3"
let a4  = Variable "ɑ4"
let a5  = Variable "ɑ5"


(* DONE *)
let fail_tests = "tests which should fail" >::: [
      typeTestFail "ft_1"
      (Op (Add, Bool true, Bool false));

      typeTestFail "ft_2"
      (Fun ("x", (Op (Add, Bool true, Var "x"))));

      typeTestFail "ft_3"
      (Fun ("x", (Op (Or, Op (Add, Int 0, Int 1), Var "x"))));

      typeTestFail "ft_4"
      (Op (Gre, Bool true, Bool false));

      typeTestFail "ft_5"
      (Let ("f", Fun ("x", Op (Add, Var "x", Var "x")), Apply (Var "f", Bool true)));

      typeTestFail "ft_6"
        (Apply (Fun ("x", Op (And, Bool true, Apply (Access "f", Var "x"))), Record ["f", Int 123]));

      typeTestFail "ft_7"
        (Apply
           (Fun ("f", Fun ("x",
                        Op (And, Apply (Var "f", Apply (Access "u", Var "x")),
                            Bool true))),
            Bool false));
      
      typeTestFail "ft_8"
        (Apply (Access "c", Record ["a", Int 123; "b", Bool true]));

      typeTestFail "ft_9"
        (Apply (Fun ("k", Apply (Var "k",
                                 Fun ("x", Let ("tmp",
                                                     Op (Add, Var "x", Int 1),
                                                     Var "x")))),
                Fun ("f", Apply (Var "f", Bool true))));

      typeTestFail "ft_10"
        (Apply (Fun ("k", Let ("test",
                               Apply (Var "k",
                                      Fun ("x", Let ("tmp",
                                                     Op (Add, Var "x", Int 1),
                                                     Var "x"))),
                               Var "test")),
                Fun ("f", Apply (Var "f", Bool true))));
    ]

(* DONE *)
let basic_tests = "basic test suite for global typing" >::: [
      typeTest "bool_true"
        (Bool true)
        (Primitive PrimBool);
      typeTest "bool_false"
        (Bool false)
        (Primitive PrimBool);
      typeTest "integer"
        (Int 0)
        (Primitive PrimInt);
      typeTest "integer_func"
        (Fun ("x", Op (Add, Var "x", Var "x")))
        (Function (Primitive PrimInt, Primitive PrimInt));

      typeTest "func_id"
        (Fun ("x", Var "x"))
        (Function (a0, a0));

      typeTest "func_top_arg"
        (Fun ("x", Int 32))
        (Function (Top , Primitive PrimInt));

      typeTest "func_poly_apply"
        (Fun ("x", Apply (Var "x", Int 32)))
        (Function (Function (Primitive PrimInt, a0), a0));

      typeTest "func_id_apply"
        (Apply (Fun ("x", Var "x"), Int 32))
        (Primitive PrimInt);

      typeTest "func_higer_order_input"
        (Fun ("x", Apply (Var "x", Int 32)))
        (Function
           (Function (Primitive PrimInt, a0),
           (a0)));

      typeTest "func_higher_order_polymorphic1"
        (Fun ("f", Fun ("x", Apply (Var "f", Apply (Var "f", Var "x")))))
        (Function
           (Function (Union (a0, a1), a1),
           (Function (a0, a1))));
    ]

(* DONE *)
let bool_tests = "boolean test suite for global typing" >::: [
    typeTest "bool_true"
    (Bool true)
    (Primitive PrimBool);

    typeTest "bool_fun1"
      (* fun x -> fun y -> fun z -> if x then y else z*)
      (* bool -> a -> a -> a*)
    (Fun ("x", Fun("y", Fun("z", If (Var "x", Var "y", Var"z")))))
    (Function (Primitive PrimBool, Function (a0, Function (a0, a0))));

    typeTest "bool_fun2"
      (* "fun x -> fun y -> if x then y else x" *)
      (*   "'a ⊓ bool -> 'a -> 'a" *)
    (Fun ("x", Fun("y", If (Var "x", Var "y", Var "x"))))
    (Function (Intersection (a0, Primitive PrimBool), Function (a0, a0)));

    typeTest "compare_fun1"
    (Fun ("x", Fun("y", If (Op (Gre, Var "x", Var "y"), Var "y", Var "x"))))
    (Function (Intersection (a0, Primitive PrimInt),
               Function (Intersection (a0, Primitive PrimInt), a0)));

    typeTest "bool_if_fun"
    (Fun ("x", Fun("y", If (Var "x", Var "y", Var "x"))))
    (Function (Intersection (a0, Primitive PrimBool),
               Function (a0, a0)));

    typeTest "func_wrap_if"
    (Fun ("x", Fun("y", Fun("z", If (Var "x", Var "y", Var "z")))))
    (Function (Primitive PrimBool,
               Function (a0, Function (a0, a0))));

    typeTest "func_not"
      (Fun ("x", If (Var "x", Bool true, Bool false)))
      (Function (Primitive PrimBool, Primitive PrimBool));

    typeTest "bool_or"
      (Fun ("x", Fun ("y", Op (And, Var "x", Var "y"))))
      (Function (Primitive PrimBool, Function (Primitive PrimBool, Primitive PrimBool)));

    typeTest "bool_or"
      (Fun ("x", Fun ("y", Op (Or, Var "x", Var "y"))))
      (Function (Primitive PrimBool, Function (Primitive PrimBool, Primitive PrimBool)));

    typeTest "bool_comp_func"
      (Fun ("x", Fun ("y", Fun ("z", Op (Or, Op (Gre, Var "x", Var "y"), Var "z")))))
      (Function (Primitive PrimInt,
                 Function (Primitive PrimInt,
                           Function (Primitive PrimBool,
                                     Primitive PrimBool))));
  ]

(* DONE *)
let record_tests = "record test suite for global typing" >::: [
      typeTest "record_literal"
        (Record ["x", Int 0])
        (Record ["x", Primitive PrimInt]);

      typeTest "record_access"
        (Access "x")
        (Function (Record [("x", a0)], a0));

      typeTest "access_in_function"
        (Fun ("r", Apply (Access "x", Var "r")))
        (Function (Record ["x", a0], a0));
      
      typeTest "record_constructor"
        (Fun ("r", Record ["x", Var "r"]))
        (Function (a0, Record ["x", a0]));

      typeTest "record_nesting"
        (Fun ("r", Record ["x", Apply (Access "x", Var "r"); "y", Var "r"]))
        (Function (Intersection (a0, Record["x", a1]), Record ["x", a1; "y", a0]));

      typeTest "empty_record"
        (Record [])  (* {} *) 
        (Record []); (* {} *) 

      typeTest "singleton_record"
        (Record ["f", Int 10])
        (Record ["f", Primitive PrimInt]);

      typeTest "singleton_record_access"
        (Apply (Access "f", (Record ["f", Bool true])))
        (Primitive PrimBool);

      typeTest "record_structure_destructure"
        (Fun ("f", Apply (Access "x", Record (["x", Apply (Var "f", Int 42)]))))
        (Function (Function (Primitive PrimInt, a0), a0));

      typeTest "record_unused_field"
        (Fun ("f",
              Apply (Access "y",
                     Record ([("x", Apply (Var "f", Int 42));
                              ("y", Int  42)]))))
        (Function (Function (Primitive PrimInt, Top), Primitive PrimInt));


    ]

(* DONE *)
let self_app_tests = "self-application test suite for global typing" >::: [
      typeTest "basic_self_application"
        (Fun ("x", Apply (Var "x", Var "x")))
        (Function (Intersection (a0, (Function (a0, a1))), a1));

       
      typeTest "triple_self_app_1" 
        (Fun ("x", Apply (Apply (Var "x", Var "x"), Var "x")))
        (Function (Intersection (a0, (Function (a0, Function (a0, a1)))), a1));

      typeTest "triple_self_app_2"
        (Fun ("x", Fun ("y", Apply (Apply (Var "x", Var "y"), Var "x"))))
        (Function (Intersection (a0, (Function (a1, Function (a0, a2)))),
                   Function (a1, a2)));

      typeTest "triple_self_app_3"
        (Fun ("x", Fun ("y", Apply (Apply (Var "x", Var "x"), Var "y"))))
        (Function (Intersection (a0, (Function (a0, Function (a1, a2)))),
                   Function (a1, a2)));

      typeTest "self_app_botom"
        (Apply (Fun ("x", Apply (Var "x", Var "x")),
                Fun ("x", Apply (Var "x", Var "x"))))
        Bottom;

      typeTest "fun_to_record"
        (Fun ("x", Record ["l", Apply (Var "x", Var "x"); "r", Var "x"]))
        (Function (Intersection (a0, (Function (a0, a1))),
                   Record ["l", a1; "r", a0]));

      typeTest "Y_combinator"
      (Fun ("f", Apply (Fun ("x", Apply (Var "f", Apply (Var "x", Var "x")))
                      , Fun ("x", Apply (Var "f", Apply (Var "x", Var "x"))))))
      (Function (Function (a0, a0), a0));

      typeTest "Z_combinator"
        (Fun ("f",
              Apply (Fun ("x", Apply (Var "f",
                                      Fun ("v", Apply (Apply (Var "x", Var "x"), Var "v")))),
                    (Fun ("x", Apply (Var "f",
                                      Fun ("v", Apply ((Apply (Var "x", Var "x")), Var "v"))))))))
        (Function ((Function (Function (a0, a1), Intersection (a2, Function (a0, a1)))), a2));

      typeTest "arbitrary_args"
      (Apply
        (Fun
           ("f", Apply
                   (Fun
                      ("x", Apply (Var "f",
                                   Fun ("v", Apply (Apply (Var "x", Var"x"),
                                                    Var "v")))),
            (Fun ("x", Apply (Var "f",
                              Fun ("v", Apply (Apply (Var "x", Var "x") ,
                                               Var "v"))))))),
        (Fun ("f", Fun ("x", Var "f")))))
        (Function (Top, Recursive ("ɑ0", Function (Top, a0))));


      typeTest "trutru"
      (LetRec ("trutru",
               Fun ("g", Apply (Var "trutru", Apply (Var "g", Bool true))),
               Var "trutru"))
       (Function (Recursive ("ɑ0", Function (Primitive PrimBool, a0)), Bottom))


    ]

(* DONE *)
let let_poly_tests = "tests for let polymorphism " >::: [
      typeTest "basic_let_poly"
        (Let ("f", Fun ("x", Var "x"), Record [("a", Apply (Var "f", Int 0))
                                             ; ("b", Apply (Var "f", Bool true))]))
        (Record [("a", Primitive PrimInt); ("b", Primitive PrimBool)]);

      typeTest "rcd_let_poly"
        (Fun ("y", Let ("f", Fun ("x", Var "x"),
              Record [("a", Apply (Var "f", Var "y"));
                      ("b", Apply (Var "f", Bool true))])))
        (Function (a0, Record ["a", a0; "b", Primitive PrimBool]));


      typeTest "nested_rcd_let_poly"
        (Fun ("y", Let ("f", Fun ("x", Apply (Var "x", Var "y")),
              Record [("a", Apply (Var "f", Fun ("z", Var "z")));
                      ("b", Apply (Var "f", Fun ("z", Bool true)))])))
        (Function (a0, Record ["a", a0; "b", Primitive PrimBool]));
      

      typeTest "let_poly_swap"
        (Fun ("y", Let ("f", Fun ("x", Apply (Var "y", Var "x")),
              Record [("a", Apply (Var "f", Int 0));
                      ("b", Apply (Var "f", Apply (Var "f", Bool true)))])))
        (Function (
             Function ((Union (a0, Union (Primitive PrimBool, Primitive PrimInt))),
                       a0),
                   Record ["a", a0; "b", a0]));


      typeTest "fun_add_poly"
        (Fun ("k", Let ("test", Apply (Var "k",
                                      Fun ("x", Let ("tmp",
                                                     Op (Add, Var "x", Int 1),
                                                     Var "x"))), Var "test")))
        (Function
           (Function
              (Function (Intersection (a0, Primitive PrimInt), a0),
            a1), a1));

      typeTest "fun_extrude_test"
        (Fun ("k", Let ("test",
                        Apply (Var "k",
                               Fun ("x", Let ("tmp",
                                              Op (Add, Var "x", Int 1),
                                              If (Bool true, Var "x", Int 2)))),
                        Var "test")))
      (Function (Function (Function (Primitive PrimInt, Primitive PrimInt), a0), a0));

      typeTest "extrude_looses_poly" 
        (Fun ("k", Let ("test",
                        Apply (Fun ("id",
                                    Apply (Access "res",
                                           Record ["tmp", Apply (Var "k", Var "id");
                                                  "res", Var "id"])),
                               Fun ("x", Var "x")),
                        Record ["u", Apply (Var "test", Int 0);
                                "v", Apply (Var "test", Bool true)])))
        (Function
           (Function
              (Function (a0, Union (a0, Union (Primitive PrimBool,
                                               Primitive PrimInt))),
               Top),
            Record ["u", Union (a0, Primitive PrimInt);
                    "v", Union (a0, Primitive PrimBool)]));

      typeTest "extrude_mlsub_original"
        (Fun ("k", Let ("test",
                        Apply (
                            Access "res",
                            Record ["tmp", Apply (Var "k", Fun ("x", Var "x"));
                                    "res", Fun ("x", Var "x")]),
                        Record ["u", Apply (Var "test", Int 0);
                                "v", Apply (Var "test", Bool true)])))
        (Function (Function (Function (a0, a0), Top),
                   Record ["u", Primitive PrimInt; "v", Primitive PrimBool]));

      typeTest "let_poly_test_1"
        (Fun ("k",
              Let ("test",
                   (Apply
                    (Fun ("thefun", Record["l", Apply (Var "k", Var "thefun");
                                           "r", Apply (Var "thefun", Int 1)]),
                    (Fun ("x", Let ("tmp", Op (Add, Var "x", Int 1), Var "x"))))),
                   Var "test")))
        (Function (Function (Function (Intersection (a0, Primitive PrimInt),
                                       Union (a0, Primitive PrimInt)), a1),
                   Record ["l", a1; "r", Primitive PrimInt]));
      

      typeTest "let_poly_test_2"
      (Fun ("a",
            Apply (
                Fun ("k",
                     Let ("test",
                          Apply (Var "k", (Fun ("x",
                                           Let ("tmp",
                                                Op (Add, Var "x", Int 1),
                                                Var"x")))), Var "test")),
                (Fun ("f", Apply (Var "f", Var "a"))))))
        (Function (Intersection (a0, Primitive PrimInt), a0))


    ]

(* DONE *)
let recursion_tests = "recursion test suite for global typing" >::: [
      typeTest "basic_self_application"
      (LetRec ("f", Fun ("x",  Apply  (Var "f", Apply (Access "u", Var "x"))), Var "f" ))
      (Function (Recursive ("ɑ0",  (Record["u", a0])), Bottom));
      (* TODO: should this be??*)
      (* (Recursive ("a10", Function (Record["u", a10], Bottom))) *)

      typeTest "recursive_2"
      (LetRec ("f", Fun ("x", Var "f"), Var "f"))
      (Recursive ("ɑ0", Function (Top, Variable "ɑ0")));

      typeTest "big_rec"
        (LetRec ("l", Fun ("a", Var "l"),
                 LetRec ("r", Fun ("a", Fun ("a", Var "r")),
                         If (Bool true, Var "l", Var "r"))))
        (Recursive ("ɑ0", Function (Top, Function(Top, Variable "ɑ0"))));

      typeTest "rec_record"
        (LetRec ("x", Record ["a", Var "x"; "b", Var "x"], Var "x"))
        (Recursive ("ɑ0", Record ["a", a0; "b", a0]));

      typeTest "top_to_record"
        (LetRec ("x", Fun ("v", Record ["a", Apply (Var "x", Var "v");
                                        "b", Apply (Var "x", Var "v")]),
                 Var "x"))
        (Function (Top, Recursive ("ɑ0", Record ["a", a0; "b", a0])));

    ]


let _ = run_test_tt_main basic_tests
let _ = run_test_tt_main fail_tests
let _ = run_test_tt_main bool_tests
let _ = run_test_tt_main record_tests
let _ = run_test_tt_main let_poly_tests
let _ = run_test_tt_main self_app_tests
let _ = run_test_tt_main recursion_tests
