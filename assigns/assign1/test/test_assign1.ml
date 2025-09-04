open Assign1

let testing = true

let run cases b = if b then cases () else []

let num_factors_tests () =
  [
    assert (num_factors 1 = 0);
    assert (num_factors 2 = 1);
    assert (num_factors 3 = 1);
    assert (num_factors 4 = 2);
    assert (num_factors 5 = 1);
    assert (num_factors 6 = 2);
    assert (num_factors 7 = 1);
    assert (num_factors 8 = 3);
    assert (num_factors 9 = 2);
    assert (num_factors 10 = 2);
  ]

let perfect_pow_tests () =
  [
    assert (perfect_power 3 8);
    assert (perfect_power 3 (-8));
    assert (perfect_power 5 32);
    assert (perfect_power 20 0);
    assert (perfect_power 20 1);
  ]

let collatz_tests () =
  [
    assert (collatz 1 = 0);
    assert (collatz 2 = 1);
    assert (collatz 3 = 7);
    assert (collatz 4 = 2);
    assert (collatz 5 = 5);
    assert (collatz 6 = 8);
    assert (collatz 7 = 16);
    assert (collatz 8 = 3);
    assert (collatz 9 = 19);
    assert (collatz 10 = 6);
  ]

let tst_records_tests () =
  [
    assert (tst_records 0 = 1);
    assert (tst_records 1 = 2);
    assert (tst_records 2 = 3);
    assert (tst_records 3 = 6);
    assert (tst_records 4 = 7);
    assert (tst_records 5 = 9);
    assert (tst_records 6 = 18);
    assert (tst_records 7 = 25);
    assert (tst_records 8 = 27);
    assert (tst_records 9 = 54);
  ]

let _run_tests =
  if not testing then [] else
    [
      run num_factors_tests true;
      run collatz_tests true;
      run tst_records_tests true;
      run perfect_pow_tests true;
    ]
