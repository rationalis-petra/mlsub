open Type.Internal.Data
open Type.Internal.Check
open OUnit2

let typePrint name expr = 
  name >:: (fun _ ->
    print_endline (string_of_simple_type (infer_simple_type expr));
    assert_bool "type_print" true)

let typePrintCtx name expr ctx = 
  name >:: (fun _ ->
    print_endline (string_of_simple_type (typecheck expr ctx 0));
    assert_bool "type_print" true)

let typeTest name expr simple_type = 
  name >:: (fun _ -> assert_equal simple_type (infer_simple_type expr))

let typeTestCtx name expr simple_type ctx =
  name >:: (fun _ -> assert_equal simple_type (typecheck expr ctx 0))

let typeTestCtxtLvl name expr simple_type ctx lvl =
  name >:: (fun _ -> assert_equal simple_type (typecheck expr ctx lvl))

let tests = "test suite for typing" >::: [
  (* Booleans *)
  typeTest "bool_true"  (Bool true)  (Primitive PrimBool);
  typeTest "bool_false" (Bool false) (Primitive PrimBool);
  typeTestCtx "bool_fn"
    (Apply (Var "not", Bool false))
    (Variable
      {lower_bounds = [Primitive PrimBool];
       upper_bounds = [];
       level = 0;
       uid = 0})
    (* (Primitive PrimBool) *)
    (Context.singleton "not"
       (SimpleTypeScheme (Function (Primitive PrimBool, Primitive PrimBool))));

  (* Integer Literals *)
  typeTest "integer1"   (Int 0)      (Primitive PrimInt);
  typeTest "integer2"   (Int 54)     (Primitive PrimInt);
  typeTest "integer3"   (Int (-10))  (Primitive PrimInt);

  (* Variables in a Dummy Context *) 
  (let context = (Context.empty
                  |> Context.add "x" (SimpleTypeScheme (Primitive PrimInt)))
                  in
  typeTestCtx "variable" (Var "x") (Primitive PrimInt) context);

  (* Operators with Primitives *)
  typeTest "op_add_simple"
    (Op (Add, Int 0, Int 2))
    (Primitive PrimInt);
  typeTest "op_geq_simple"
    (Op (Gre, Int 0, Int 3))
    (Primitive PrimBool);
  typeTest "op_and_simple"
    (Op (And, Bool true, Bool false))
    (Primitive PrimBool);
    ]

let _ = run_test_tt_main tests
