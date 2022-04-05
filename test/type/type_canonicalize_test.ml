open Type.Internal.Data
open Type.Internal.Simplify
open OUnit2

module Canon = struct
  let canonicalize_type = CompactTypeScheme.canonicalize_type
end
open Canon

let canonTest name c_type s_type =
  name >:: (fun _ -> assert_equal c_type (canonicalize_type s_type)
                       ~cmp:(fun x y -> CompactTypeScheme.compare x y = 0))

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


(* The type of an integer *)
let compact_int : CompactType.t = 
  {vars = VarStateSet.empty;
   prims = PrimSet.singleton PrimInt;
   rcd = None;
   func = None}

let cscheme_int : CompactTypeScheme.t =
  { term = compact_int;
    rec_vars = VarMap.empty }



(* The type of an unbound (polymorphic) variable, e.g. 'a in the type 'a -> 'a *)
let unbound_var : variable_state = {
    lower_bounds = [];
    upper_bounds = [];
    level = 0;
    uid = 0;}

let int_var : variable_state = {
    lower_bounds = [Primitive PrimInt];
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

(* let compact_int_func : CompactType.t =  *)
(*   {vars = VarStateSet.empty; *)
(*    prims = PrimSet.empty; *)
(*    rcd = None; *)
(*    func = Some (compact_unbound_var, compact_unbound_var)} *)

(* let cscheme_int_func : CompactTypeScheme.t =  *)
(*   { term = compact_id_func; *)
(*     rec_vars = VarMap.empty } *)

let tests = "test suite for type canonicalization" >::: [
  (* Primitives *)
  canonTest "prim_bool" cscheme_bool (Primitive PrimBool);
  canonTest "prim_int"  cscheme_int  (Primitive PrimInt);

  canonTest "unbound_var" cscheme_unbound_var (Variable unbound_var);

  canonTest "id_func"
    cscheme_id_func
    (Function
       (Variable unbound_var,
        Variable unbound_var));

  (* canonTest "int_func" *)
  (*   cscheme_id_func *)
  (*   (Function *)
  (*      (Variable unbound_var, *)
  (*       Variable unbound_var)); *)

    ]

let _ = run_test_tt_main tests
