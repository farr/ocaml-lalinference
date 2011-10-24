open OUnit

let tests = "all tests" >:::
  [Parameters_test.tests]

let _ = 
  Random.self_init ();
  ignore(run_test_tt_main tests)
