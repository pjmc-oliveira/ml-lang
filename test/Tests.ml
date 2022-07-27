open OUnit2
open Ml_lang
module MonoTests = SolverTests.Mono (Robinson)
module PolyTests = SolverTests.Poly (Robinson)

let tests =
  test_list
    [
      ParserTests.suite;
      MonoTests.suite;
      PolyTests.suite;
      InterpreterTests.suite;
    ]

let () = run_test_tt_main tests
