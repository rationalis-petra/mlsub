open Type
open OUnit2

let typeTest name expr mlsub_type = 
  (* make sure to reset the varUidCounter !!*)
  name >:: (fun _ -> assert_equal mlsub_type (infer_type expr))

let typeTestPrint name expr mlsub_type = 
  let ty = (infer_type ~prtest:true expr) in
  print_endline (string_of_type ty);
  print_endline (string_of_type mlsub_type);
  name >:: (fun _ -> assert_equal mlsub_type ty)



let tests = "test suite for global typing" >::: [
      typeTest "bool_true"
        (Bool true)
        (PrimitiveType PrimBool);
      typeTest "bool_false"
        (Bool false)
        (PrimitiveType PrimBool);
      typeTest "integer"
        (Int 0)
        (PrimitiveType PrimInt);

      typeTest "func_id"
        (Fun ("x", Var "x"))
        (FunctionType (VariableType "ɑ0", VariableType "ɑ0"));
      typeTest "func_top_arg"
        (Fun ("x", Int 32))
        (FunctionType (Top , PrimitiveType PrimInt));


      typeTestPrint "func_id_apply"
        (Apply (Fun ("x", Var "x"), Int 32))
        (PrimitiveType PrimInt);

      (* typeTestPrint "func_application" *)
      (*   (Apply (Fun ("x", Op (Add, Var "x", Int 3)), Int 3)) *)
      (*   (PrimitiveType PrimInt); *)


  (* Variables in a Dummy Context *)
  (* (let context = (Context.empty *)
  (*                 |> Context.add "x" (SimpleTypeScheme (Primitive PrimInt))) *)
  (*                 in *)
  (* typeTestCtx "variable" (Var "x") (PrimitiveType PrimInt) context); *)
    ]

let _ = run_test_tt_main tests
