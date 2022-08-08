open OUnit2

let tests =
  test_list
    [
      ParserTests.suite;
      SolverTests.suite;
      InterpreterTests.suite;
      SCCTests.suite;
    ]

let () = run_test_tt_main tests
