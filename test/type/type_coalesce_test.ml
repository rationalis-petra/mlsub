open Type.Internal.Data
open Type.Internal.Simplify
open OUnit2

let coalTest name cts mlt =
  name >:: (fun _ -> assert_equal
                       mlt
                       (CompactTypeScheme.coalesce_compact_type cts))

let coalTestPrint name cts mlt2 =
  name >:: (fun _ ->
    let mlt1 = (CompactTypeScheme.coalesce_compact_type cts) in
    print_endline (string_of_type mlt1);
    print_endline (string_of_type mlt2);
    assert_equal mlt1 mlt2)

let compact_bool : CompactType.t = 
  {vars = VarStateSet.empty;
   prims = PrimSet.singleton PrimBool;
   rcd = None;
   func = None}
let cscheme_bool : CompactTypeScheme.t =
  { term = compact_bool;
    rec_vars = VarMap.empty }


let compact_int : CompactType.t = 
  {vars = VarStateSet.empty;
   prims = PrimSet.singleton PrimInt;
   rcd = None;
   func = None}
let cscheme_int : CompactTypeScheme.t =
  { term = compact_int;
    rec_vars = VarMap.empty }


let tv0 : variable_state = {
    lower_bounds = [];
    upper_bounds = [Primitive PrimInt];
    level = 0;
    uid = 0}
(* *)
let compact_intvar : CompactType.t = 
  {vars = VarStateSet.singleton tv0;
   prims = PrimSet.empty;
   rcd = None;
   func = None}
let cscheme_intvar : CompactTypeScheme.t =
  { term = compact_intvar;
    rec_vars = VarMap.empty }

let tests = "test suite for coalescing compact types" >::: [
  (* Leave primitive types 'untouched' *)
  coalTest "prim_bool" cscheme_bool (PrimitiveType PrimBool);
  coalTest "prim_int" cscheme_int (PrimitiveType PrimInt);

  (* Types with upper bounds should be that bound *)
  (* coalTestPrint "var_int" cscheme_intvar (PrimitiveType PrimInt); *)

  ]

let _ = run_test_tt_main tests
