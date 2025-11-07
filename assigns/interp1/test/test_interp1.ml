open Interp1
open OUnit2


let parse_tests =
  "testing parse" >:::
    [
      "basic arithmetic expr" >:: (fun _ ->
        let expected = Some (Bop (Mul, Bop (Add, Num 1, Num 2), Num 3)) in
        let actual = parse "(1 + 2) * 3" in
        assert_equal expected actual);

      (* TODO: write more tests *)
      "nested arithmetic" >:: (fun _ ->
        let expected = Some (Bop (Add, Num 1, Bop (Mul, Num 2, Num 3))) in
        let actual = parse "1 + 2 * 3" in
        assert_equal expected actual);
  
      "boolean and comparison" >:: (fun _ ->
        let expected = Some (Bop (Lt, Num 2, Num 3)) in
        let actual = parse "2 < 3" in
        assert_equal expected actual);
  
      "if expression" >:: (fun _ ->
        let expected = Some (If (True, Num 1, Num 0)) in
        let actual = parse "if true then 1 else 0" in
        assert_equal expected actual);
    ]



let subst_tests =
  "testing subst" >:::
    [
      "single variable" >:: (fun _ ->
        let expected = Bop (Add, Var "x", If (Unit, Unit, Unit)) in
        let actual = subst VUnit "y" (Bop (Add, Var "x", If(Var "y", Var "y", Var "y"))) in
        assert_equal expected actual);
      (* TODO: write more tests *)
      "replace simple variable" >:: (fun _ ->
        let expected = Num 5 in
        let actual = subst (VNum 5) "x" (Var "x") in
        assert_equal expected actual);
  
      "skip bound variable in let" >:: (fun _ ->
        let expr = Let ("x", Num 10, Var "x") in
        let actual = subst (VNum 5) "x" expr in
        assert_equal expr actual);
  
      "replace inside nested bop" >:: (fun _ ->
        let expected = Bop (Add, Num 5, Num 2) in
        let actual = subst (VNum 5) "x" (Bop (Add, Var "x", Num 2)) in
        assert_equal expected actual);
  
    ]



let eval_tests =
  "testing eval" >:::
    [
      "application" >:: (fun _ ->
        let expected = Ok (VNum 4) in
        let actual = eval (App (Fun ("x", Bop (Add, Var "x", Num 1)), Num 3)) in
        assert_equal expected actual);
      (* TODO: write more tests *)
      "addition" >:: (fun _ ->
        let expected = Ok (VNum 5) in
        let actual = eval (Bop (Add, Num 2, Num 3)) in
        assert_equal expected actual);
  
      "boolean and short-circuit" >:: (fun _ ->
        let expected = Ok (VBool false) in
        let actual = eval (Bop (And, False, Bop (Div, Num 1, Num 0))) in
        assert_equal expected actual);
    ]




let interp_tests =
  "interp tests" >:::
    [
      "variable" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = interp "let x = true || false in x && true" in
        assert_equal expected actual);
      (* TODO: write more tests *)

      "arithmetic" >:: (fun _ ->
        let expected = Ok (VNum 9) in
        let actual = interp "3 * (2 + 1)" in
        assert_equal expected actual);

      "let binding" >:: (fun _ ->
        let expected = Ok (VNum 7) in
        let actual = interp "let x = 3 in x + 4" in
        assert_equal expected actual);

      "unknown variable" >:: (fun _ ->
        let expected = Error (UnknownVar "x") in
        let actual = interp "x + 1" in
        assert_equal expected actual);

    ]



let tests =
  "interp1 test suite" >:::
    [
      parse_tests;
      subst_tests;
      eval_tests;
      interp_tests;
    ]

let _ = run_test_tt_main tests
