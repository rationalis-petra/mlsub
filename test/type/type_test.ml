open Type
open Type__.Data.MLSubType
open OUnit2
module P = Parse

let typeTest name expr mlsub_type = 
  (* make sure to reset the varUidCounter !!*)
  name >:: (fun _ -> assert_equal mlsub_type (infer_type expr))

let typeTestPrint name expr mlsub_type = 
  let ty = (infer_type ~prtest:false expr) in
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
let a6  = Variable "ɑ6"
let a7  = Variable "ɑ7"
let a8  = Variable "ɑ8"
let a9  = Variable "ɑ9"
let a10 = Variable "ɑ10"


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

      typeTest "func_id"
        (Fun ("x", Var "x"))
        (Function (a0, a0));

      typeTest "func_top_arg"
        (Fun ("x", Int 32))
        (Function (Top , Primitive PrimInt));


      typeTest "func_id_apply"
        (Apply (Fun ("x", Var "x"), Int 32))
        (Primitive PrimInt);

      typeTest "func_higer_order_input"
        (Fun ("x", Apply (Var "x", Int 32)))
        (Function
           (Function (Primitive PrimInt, a1),
           (a1)));

      typeTest "func_higher_order_polymorphic"
        (Fun ("f", Fun ("x", Apply (Var "f", Apply (Var "f", Var "x")))))
        (Function
           (Function (Union (a1, a3), a3),
           (Function (a1, a3))));
    ]

let bool_tests = "boolean test suite for global typing" >::: [
    typeTest "bool_fun1"
      (* fun x -> fun y -> fun z -> if x then y else z*)
      (* bool -> a -> a -> a*)
    (Fun ("x", Fun("y", Fun("z", If (Var "x", Var "y", Var"z")))))
    (Function (Primitive PrimBool, Function (a3, Function (a3, a3))));

    typeTest "bool_fun2"
      (* "fun x -> fun y -> if x then y else x" *)
      (*   "'a ⊓ bool -> 'a -> 'a" *)
    (Fun ("x", Fun("y", If (Var "x", Var "y", Var "x"))))
    (Function (Intersection (a2, Primitive PrimBool), Function (a2, a2)));




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
        (Function (Function (Primitive PrimInt, a3), a3));

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
        (Function (Intersection (a0, (Function (a0, Function (a0, a2)))), a2))
    ]

let poly_tests = "tests for let polymorphism " >::: [
      typeTest "basic_let_poly"
        (Let ("f", Fun ("x", Var "x"), Record [("a", Apply (Var "f", Int 0))
                                             ; ("b", Apply (Var "f", Bool true))]))
        (Record [("a", Primitive PrimInt); ("b", Primitive PrimBool)])
    ]

let recursion_tests = "recursion test suite for global typing" >::: [
      typeTest "basic_self_application"
      (LetRec ("f", Fun ("x",  Apply  (Var "f", Apply (Access "u", Var "x"))), Var "f" ))
      (Function (Recursive ("ɑ10",  (Record["u", a10])), Bottom));
      (* TODO: should this be??*)
      (* (Recursive ("a10", Function (Record["u", a10], Bottom))) *)

      typeTest "recursive_2"
      (LetRec ("f", Fun ("x", Var "f"), Var "f"))
      (Recursive ("ɑ5", Function (Top, Variable "ɑ5")));

      typeTest "big_rec"
        (LetRec ("l", Fun ("a", Var "l"),
                 LetRec ("r", Fun ("a", Fun ("a", Var "r")),
                         If (Bool true, Var "l", Var "r"))))
        (Recursive ("ɑ12", Function (Top, Function(Top, Variable "ɑ12"))));
    ]


let _ = run_test_tt_main basic_tests
let _ = run_test_tt_main bool_tests
let _ = run_test_tt_main record_tests
let _ = run_test_tt_main poly_tests
let _ = run_test_tt_main self_app_tests
let _ = run_test_tt_main recursion_tests
