open OUnit2
open Ml_lang
module MonoTests = SolverTests.Mono (HindleyMilner)
module PolyTests = SolverTests.Poly (HindleyMilner)

let tests =
  test_list
    [
      ParserTests.suite;
      MonoTests.suite;
      PolyTests.suite;
      InterpreterTests.suite;
    ]

let () = run_test_tt_main tests
