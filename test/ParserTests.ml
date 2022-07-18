open OUnit2
open Ml_lang
open Result.Syntax

let parse_module str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  (* print_string
     ("\n****\n"
     ^ String.concat "\n"
         (List.map
            (fun (tk, sp) ->
              "\t( " ^ Token.show tk ^ ", " ^ Source.Span.show sp ^ " )")
            tks)); *)
  let* m = Parser.(parse module_ tks) in
  Ok m

open Cst

let string_of_result r =
  match r with
  | Ok m -> "Ok " ^ Module.show m
  | Error e -> "Error " ^ String.concat "\n" (List.map Error.to_string e)

let test_parser label str cst =
  label >:: fun _ ->
  assert_equal (Ok cst) (parse_module str) ~printer:string_of_result

let suite =
  "Parser"
  >::: [
         test_parser "empty module" "module Hello = {}"
           (Module.Module
              {
                name = "Hello";
                bindings = [];
                span = { index = 0; line = 1; column = 1; length = 17 };
              });
         test_parser "one definition" "module Hello = { def hello = 1 }"
           (Module.Module
              {
                name = "Hello";
                bindings =
                  [
                    Binding.Def
                      {
                        name = "hello";
                        expr =
                          Expr.Const
                            {
                              value = 1;
                              span =
                                {
                                  index = 29;
                                  line = 1;
                                  column = 30;
                                  length = 1;
                                };
                            };
                        span =
                          { index = 17; line = 1; column = 18; length = 13 };
                      };
                  ];
                span = { index = 0; line = 1; column = 1; length = 32 };
              });
         test_parser "two definitions"
           "module Hello = { def hello = 1 def bye = 2 }"
           (Module.Module
              {
                name = "Hello";
                bindings =
                  [
                    Binding.Def
                      {
                        name = "hello";
                        expr =
                          Expr.Const
                            {
                              value = 1;
                              span =
                                {
                                  index = 29;
                                  line = 1;
                                  column = 30;
                                  length = 1;
                                };
                            };
                        span =
                          { index = 17; line = 1; column = 18; length = 13 };
                      };
                    Binding.Def
                      {
                        name = "bye";
                        expr =
                          Expr.Const
                            {
                              value = 2;
                              span =
                                {
                                  index = 41;
                                  line = 1;
                                  column = 42;
                                  length = 1;
                                };
                            };
                        span =
                          { index = 31; line = 1; column = 32; length = 11 };
                      };
                  ];
                span = { index = 0; line = 1; column = 1; length = 44 };
              });
       ]

let () = run_test_tt_main suite
