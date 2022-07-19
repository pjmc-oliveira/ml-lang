open OUnit2
open Ml_lang
open Result.Syntax

let parse_module str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  Ok m

let string_of_cst_result r =
  match r with
  | Ok m -> "Ok " ^ Cst.Module.show m
  | Error e -> "Error " ^ String.concat "\n" (List.map Error.to_string e)

let test_parser_cst label str cst =
  label >:: fun _ ->
  assert_equal (Ok cst) (parse_module str) ~printer:string_of_cst_result

let string_of_ast_result r =
  match r with
  | Ok m -> "Ok " ^ Ast.Module.show m
  | Error e -> "Error " ^ String.concat "\n" (List.map Error.to_string e)

let test_parser_ast label str ast =
  label >:: fun _ ->
  assert_equal (Ok ast)
    (Result.map Cst.Module.to_ast (parse_module str))
    ~printer:string_of_ast_result

let cst_tests =
  [
    test_parser_cst "empty module" "module Hello = {}"
      (Module
         {
           name = "Hello";
           bindings = [];
           span = { index = 0; line = 1; column = 1; length = 17 };
         });
    test_parser_cst "one definition" "module Hello = { def hello = 1 }"
      (Module
         {
           name = "Hello";
           bindings =
             [
               Def
                 {
                   name = "hello";
                   expr =
                     Int
                       {
                         value = 1;
                         span =
                           { index = 29; line = 1; column = 30; length = 1 };
                       };
                   span = { index = 17; line = 1; column = 18; length = 13 };
                 };
             ];
           span = { index = 0; line = 1; column = 1; length = 32 };
         });
    test_parser_cst "two definitions"
      "module Hello = { def hello = 1 def bye = 2 }"
      (Module
         {
           name = "Hello";
           bindings =
             [
               Def
                 {
                   name = "hello";
                   expr =
                     Int
                       {
                         value = 1;
                         span =
                           { index = 29; line = 1; column = 30; length = 1 };
                       };
                   span = { index = 17; line = 1; column = 18; length = 13 };
                 };
               Def
                 {
                   name = "bye";
                   expr =
                     Int
                       {
                         value = 2;
                         span =
                           { index = 41; line = 1; column = 42; length = 1 };
                       };
                   span = { index = 31; line = 1; column = 32; length = 11 };
                 };
             ];
           span = { index = 0; line = 1; column = 1; length = 44 };
         });
  ]

let ast_tests =
  [
    test_parser_ast "empty module" "module Hello = {}"
      (Module { name = "Hello"; bindings = [] });
    test_parser_ast "one definition" "module Hello = { def hello = 1 }"
      (Module
         {
           name = "Hello";
           bindings = [ Def { name = "hello"; expr = Int { value = 1 } } ];
         });
    test_parser_ast "True literal" "module Hello = { def hello = True }"
      (Module
         {
           name = "Hello";
           bindings = [ Def { name = "hello"; expr = Bool { value = true } } ];
         });
    test_parser_ast "True literal" "module Hello = { def hello = False }"
      (Module
         {
           name = "Hello";
           bindings = [ Def { name = "hello"; expr = Bool { value = false } } ];
         });
    test_parser_ast "parenthesized expression"
      "module Hello = { def hello = if (let x = True in x) then 1 else 2 }"
      (Module
         {
           name = "Hello";
           bindings =
             [
               Def
                 {
                   name = "hello";
                   expr =
                     If
                       {
                         cond =
                           Let
                             {
                               name = "x";
                               def = Bool { value = true };
                               body = Var { name = "x" };
                             };
                         con = Int { value = 1 };
                         alt = Int { value = 2 };
                       };
                 };
             ];
         });
    test_parser_ast "two definitions"
      "module Hello = { def hello = 1 def bye = hello }"
      (Module
         {
           name = "Hello";
           bindings =
             [
               Def { name = "hello"; expr = Int { value = 1 } };
               Def { name = "bye"; expr = Var { name = "hello" } };
             ];
         });
    test_parser_ast "let expression"
      "module Hello = { def hello = let x = 1 in x }"
      (Module
         {
           name = "Hello";
           bindings =
             [
               Def
                 {
                   name = "hello";
                   expr =
                     Let
                       {
                         name = "x";
                         def = Int { value = 1 };
                         body = Var { name = "x" };
                       };
                 };
             ];
         });
    test_parser_ast "if expression"
      "module Hello = { def hello = if True then 1 else x }"
      (Module
         {
           name = "Hello";
           bindings =
             [
               Def
                 {
                   name = "hello";
                   expr =
                     If
                       {
                         cond = Bool { value = true };
                         con = Int { value = 1 };
                         alt = Var { name = "x" };
                       };
                 };
             ];
         });
  ]

let suite = "Parser" >::: cst_tests @ ast_tests
