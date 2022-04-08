open Type
open Type__.Data.MLSubType
open OUnit2
module P = Parse

let typeTest name expr mlsub_type = 
  (* make sure to reset the varUidCounter !!*)
  name >:: (fun _ -> assert_equal mlsub_type (infer_type expr))

let typeTestPrint name expr mlsub_type = 
  let ty = (infer_type expr) in
  print_endline (string_of_type ty);
  print_endline (string_of_type mlsub_type);
  name >:: (fun _ -> assert_equal mlsub_type ty)

let typeTestPrintSeq name (expr : P.expr) _ =
  let _ = (infer_type_stepped expr) in
  (* print_endline (string_of_type ty); *)
  (* print_endline (string_of_type mlsub_type); *)
  name >:: (fun _ -> assert_equal true true)

let a0 = Variable "ɑ0"
let a1 = Variable "ɑ1"
let a2 = Variable "ɑ2"
let a3 = Variable "ɑ3"
let a4 = Variable "ɑ4"


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

let record_tests = "record test suite for global typing" >::: [
      typeTest "record_access"
      (Access "x")
      (Function (Record [("x", a0)], a0))
    ]

(* TODO: seems to infinite loop?!*)
let recursion_tests = "recursion test suite for global typing" >::: [
      typeTest "basic_self_application"
      (Fun ("x", Apply (Var "x", Var "x")))
      (Function (Intersection (a0, (Function (a0, a1))), a1));
    ]



let _ = run_test_tt_main basic_tests
let _ = run_test_tt_main record_tests
let _ = run_test_tt_main recursion_tests
