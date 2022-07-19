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
  let open Cst in
  [
    test_parser_cst "empty module" "module Hello = {}"
      (Module.Module
         {
           name = "Hello";
           bindings = [];
           span = { index = 0; line = 1; column = 1; length = 17 };
         });
    test_parser_cst "one definition" "module Hello = { def hello = 1 }"
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
                           { index = 29; line = 1; column = 30; length = 1 };
                       };
                   span = { index = 17; line = 1; column = 18; length = 13 };
                 };
             ];
           span = { index = 0; line = 1; column = 1; length = 32 };
         });
    test_parser_cst "two definitions"
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
                           { index = 29; line = 1; column = 30; length = 1 };
                       };
                   span = { index = 17; line = 1; column = 18; length = 13 };
                 };
               Binding.Def
                 {
                   name = "bye";
                   expr =
                     Expr.Const
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
  let open Ast in
  [
    test_parser_ast "empty module" "module Hello = {}"
      (Module.Module { name = "Hello"; bindings = [] });
    test_parser_ast "one definition" "module Hello = { def hello = 1 }"
      (Module.Module
         {
           name = "Hello";
           bindings =
             [ Binding.Def { name = "hello"; expr = Expr.Const { value = 1 } } ];
         });
    test_parser_ast "two definitions"
      "module Hello = { def hello = 1 def bye = hello }"
      (Module.Module
         {
           name = "Hello";
           bindings =
             [
               Binding.Def { name = "hello"; expr = Expr.Const { value = 1 } };
               Binding.Def { name = "bye"; expr = Expr.Var { name = "hello" } };
             ];
         });
    test_parser_ast "let expression"
      "module Hello = { def hello = let x = 1 in x }"
      (Module.Module
         {
           name = "Hello";
           bindings =
             [
               Binding.Def
                 {
                   name = "hello";
                   expr =
                     Expr.Let
                       {
                         name = "x";
                         def = Expr.Const { value = 1 };
                         body = Expr.Var { name = "x" };
                       };
                 };
             ];
         });
  ]

let suite = "Parser" >::: cst_tests @ ast_tests