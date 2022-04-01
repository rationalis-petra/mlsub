open Type.Internal.Data
open Type.Internal.Simplify
open OUnit2

module Canon = struct
  let canonicalize_type = CompactTypeScheme.canonicalize_type
end
open Canon

let canonTest name c_type s_type =
  name >:: (fun _ -> assert_equal c_type (canonicalize_type s_type))

let canonTestPrint name c_type s_type =
  name >:: (fun _ ->
    let c_type2 = (canonicalize_type s_type) in
    print_endline (CompactTypeScheme.to_str c_type);
    print_endline (CompactTypeScheme.to_str c_type2);
    assert_equal c_type c_type2)

(* let typeTestCtx name expr simple_type ctx = *)
(*   name >:: (fun _ -> assert_equal simple_type (typecheck expr ctx 0)) *)

(* let typeTestCtxtLvl name expr simple_type ctx lvl = *)
(*   name >:: (fun _ -> assert_equal simple_type (typecheck expr ctx lvl)) *)


let compact_bool : CompactType.t = 
  {vars = VarStateSet.empty;
   prims = PrimSet.singleton PrimBool;
   rcd = None;
   func = None}

let cscheme_bool : CompactTypeScheme.t =
  { term = compact_bool;
    rec_vars = VarMap.empty }

let unbound_var : variable_state = {
    lower_bounds = [];
    upper_bounds = [];
    level = 0;
    uid = 0;}

let compact_unbound_var : CompactType.t = 
  {vars = VarStateSet.singleton unbound_var;
   prims = PrimSet.empty;
   rcd = None;
   func = None}

let cscheme_unbound_var : CompactTypeScheme.t = 
  { term = compact_unbound_var;
    rec_vars = VarMap.empty }

let compact_id_func : CompactType.t = 
  {vars = VarStateSet.empty;
   prims = PrimSet.empty;
   rcd = None;
   func = Some (compact_unbound_var, compact_unbound_var)}

let cscheme_id_func : CompactTypeScheme.t = 
  { term = compact_id_func;
    rec_vars = VarMap.empty }

let tests = "test suite for typing" >::: [
  (* Primitives *)
  canonTest "prim_bool" cscheme_bool (Primitive PrimBool);

  canonTest "unbound_var" cscheme_unbound_var (Variable unbound_var);

  (* canonTestPrint "id_func" *)
  (*   cscheme_id_func *)
  (*   (Function *)
  (*      (Variable unbound_var, *)
  (*       Variable unbound_var)); *)
  (*   ] *)
(*   typeTest "bool_false" (Bool false) (Primitive PrimBool); *)
(*   typeTestCtx "bool_fn" *)
(*     (Apply (Var "not", Bool false)) *)
(*     (Variable *)
(*       {lower_bounds = [Primitive PrimBool]; *)
(*        upper_bounds = []; *)
(*        level = 0; *)
(*        uid = 0}) *)
(*     (\* (Primitive PrimBool) *\) *)
(*     (Context.singleton "not" *)
(*        (SimpleTypeScheme (Function (Primitive PrimBool, Primitive PrimBool)))); *)

(*   (\* Integer Literals *\) *)
(*   typeTest "integer1"   (Int 0)      (Primitive PrimInt); *)
(*   typeTest "integer2"   (Int 54)     (Primitive PrimInt); *)
(*   typeTest "integer3"   (Int (-10))  (Primitive PrimInt); *)

(*   (\* Variables in a Dummy Context *\)  *)
(*   (let context = (Context.empty *)
(*                   |> Context.add "x" (SimpleTypeScheme (Primitive PrimInt))) *)
(*                   in *)
(*   typeTestCtx "variable" (Var "x") (Primitive PrimInt) context); *)

(*   (\* Operators with Primitives *\) *)
(*   typeTest "op_add_simple" *)
(*     (Op (Add, Int 0, Int 2)) *)
(*     (Primitive PrimInt); *)
(*   typeTest "op_geq_simple" *)
(*     (Op (Gre, Int 0, Int 3)) *)
(*     (Primitive PrimBool); *)
(*   typeTest "op_and_simple" *)
(*     (Op (And, Bool true, Bool false)) *)
(*     (Primitive PrimBool); *)
    ]

let _ = run_test_tt_main tests
