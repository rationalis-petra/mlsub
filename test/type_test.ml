open Type
open OUnit2

let typeTest name expr simple_type = 
  name >:: (fun _ -> assert_equal simple_type (typecheck expr (Hashtbl.create 100)))

let typeTestCtxt name expr simple_type ctx =
  name >:: (fun _ -> assert_equal simple_type (typecheck expr ctx))


let tests = "test suite for typing" >::: [
  (* Boolean Literals *)
  typeTest "bool_true"  (Bool true)  (Primitive PrimBool);
  typeTest "bool_false" (Bool false) (Primitive PrimBool);

  (* Integer Literals *)
  typeTest "integer1"   (Int 0)      (Primitive PrimInt);
  typeTest "integer2"   (Int 54)     (Primitive PrimInt);
  typeTest "integer3"   (Int (-10))  (Primitive PrimInt);

  (* Variables in a Dummy Context *) 
  let context = Hashtbl.create 1 in
  (Hashtbl.add context "x" (Primitive PrimInt);
  typeTestCtxt "variable" (Var "x") (Primitive PrimInt) context)]

let _ = run_test_tt_main tests
