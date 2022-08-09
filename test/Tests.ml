open OUnit2

let tests =
  test_list
    [
      Parser_tests.suite;
      Solver_tests.suite;
      Interpreter_tests.suite;
      Scc_tests.suite;
    ]

let () = run_test_tt_main tests
