open OUnit2
open Ml_lang
module MonoTests = SolverTests.Mono (Solver)
module PolyTests = SolverTests.Poly (Solver)

let tests =
  test_list
    [
      ParserTests.suite;
      MonoTests.suite;
      PolyTests.suite;
      InterpreterTests.suite;
      SCCTests.suite;
    ]

let () = run_test_tt_main tests
