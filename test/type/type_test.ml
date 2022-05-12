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


let fail_tests = "tests which should fail" >::: [
      typeTestFail "add bools"
      (Op (Add, Bool true, Bool false));

      typeTestFail "compare bools"
      (Op (Gre, Bool true, Bool false));

      typeTestFail "bad_function_app"
      (Let ("f", Fun ("x", Op (Add, Var "x", Var "x")), Apply (Var "f", Bool true)));
    ]

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

    typeTest "wrap_if"
    (Fun ("x", Fun("y", Fun("z", If (Var "x", Var "y", Var "z")))))
    (Function (Primitive PrimBool,
               Function (a0, Function (a0, a0))));

    typeTest "func_not"
      (Fun ("x", If (Var "x", Bool true, Bool false)))
      (Function (Primitive PrimBool, Primitive PrimBool));

  ]

let record_tests = "record test suite for global typing" >::: [
      typeTest "record_access"
        (Access "x") (* #x *)
        (Function (Record [("x", a0)], a0));  (* {x : 'a} -> 'a *)

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
        (Function (Function (Primitive PrimInt, Top), Primitive PrimInt))


    ]

let self_app_tests = "self-application test suite for global typing" >::: [
      typeTest "basic_self_application"
        (Fun ("x", Apply (Var "x", Var "x")))
        (Function (Intersection (a0, (Function (a0, a1))), a1));

       
      typeTest "triple_self_application" 
        (* "fun x -> x x x" *)
        (Fun ("x", Apply (Apply (Var "x", Var "x"), Var "x")))
        (* "'a ⊓ ('a -> 'a -> 'b) -> 'b") *)
        (Function (Intersection (a0, (Function (a0, Function (a0, a1)))), a1));

      typeTest "basic_self_application"
        (Fun ("x", Fun ("y", Apply (Apply (Var "x", Var "x"), Var "y"))))
        (Function (Intersection (a0, (Function (a0, Function (a1, a2)))),
                   Function (a1, a2)));

      typeTest "self_app_botom"
      (Apply (Fun ("x", Apply (Var "x", Var "x")), Fun ("x", Apply (Var "x", Var "x"))))
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

      (* typeTest "Z_combinator" *)
      (* (Fun ("f", Apply (Fun ("x", Apply (Var "f", Apply (Var "x", Var "x"))) *)
      (*                 , Fun ("x", Apply (Var "f", Apply (Var "x", Var "x")))))) *)
      (* (Function (Function (a0, a0), a0)); *)




    ]

let poly_tests = "tests for let polymorphism " >::: [
      typeTest "basic_let_poly"
        (Let ("f", Fun ("x", Var "x"), Record [("a", Apply (Var "f", Int 0))
                                             ; ("b", Apply (Var "f", Bool true))]))
        (Record [("a", Primitive PrimInt); ("b", Primitive PrimBool)]);

      
    ]

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
    ]


let _ = run_test_tt_main basic_tests
let _ = run_test_tt_main fail_tests
let _ = run_test_tt_main bool_tests
let _ = run_test_tt_main record_tests
let _ = run_test_tt_main poly_tests
let _ = run_test_tt_main self_app_tests
let _ = run_test_tt_main recursion_tests
