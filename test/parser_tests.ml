open OUnit2
open Ml_lang
open Extensions

module W = Writer_option.Make (struct
  type t = Error.t list

  let empty = []
  let concat = ( @ )
end)

open W.Syntax

let parse_module str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  W.pure m

let string_of_cst_result src (r, e) =
  (* TODO: Print errors if present *)
  match r with
  | Some m -> "Some " ^ Cst.show_modu m
  | None -> "None " ^ String.concat "\n" (List.map (Error.to_string src) e)

let test_parser_cst label str cst =
  label >:: fun _ ->
  assert_equal (Some cst, []) (parse_module str)
    ~printer:(string_of_cst_result (Source.of_string str))

let string_of_ast_result src (r, e) =
  (* TODO: Print errors if present *)
  match r with
  | Some m -> "Ok " ^ Ast.show_modu m
  | None -> "Error " ^ String.concat "\n" (List.map (Error.to_string src) e)

let test_parser_ast label str ast =
  label >:: fun _ ->
  assert_equal (Some ast, [])
    (W.map Cst.to_ast (parse_module str))
    ~printer:(string_of_ast_result (Source.of_string str))

let cst_tests =
  let open Cst in
  [
    test_parser_cst "empty module" "module Hello = {}"
      (Module ({ index = 0; line = 1; column = 1; length = 17 }, "Hello", []));
    test_parser_cst "one definition" "module Hello = { def hello = 1 }"
      (Module
         ( { index = 0; line = 1; column = 1; length = 32 },
           "Hello",
           [
             Def
               ( { index = 17; line = 1; column = 18; length = 13 },
                 "hello",
                 None,
                 ELit ({ index = 29; line = 1; column = 30; length = 1 }, Int 1)
               );
           ] ));
    test_parser_cst "two definitions"
      "module Hello = { def hello = 1 def bye = 2 }"
      (Module
         ( { index = 0; line = 1; column = 1; length = 44 },
           "Hello",
           [
             Def
               ( { index = 17; line = 1; column = 18; length = 13 },
                 "hello",
                 None,
                 ELit ({ index = 29; line = 1; column = 30; length = 1 }, Int 1)
               );
             Def
               ( { index = 31; line = 1; column = 32; length = 11 },
                 "bye",
                 None,
                 ELit ({ index = 41; line = 1; column = 42; length = 1 }, Int 2)
               );
           ] ));
  ]

let ast_tests =
  [
    test_parser_ast "empty module" "module Hello = {}" (Module ("Hello", []));
    test_parser_ast "one definition" "module Hello = { def hello = 1 }"
      (Module ("Hello", [ Def (None, "hello", ELit (Int 1)) ]));
    test_parser_ast "True literal" "module Hello = { def hello = True }"
      (Module ("Hello", [ Def (None, "hello", ELit (Bool true)) ]));
    test_parser_ast "True literal" "module Hello = { def hello = False }"
      (Module ("Hello", [ Def (None, "hello", ELit (Bool false)) ]));
    test_parser_ast "type annotated expression"
      "module Hello = { def hello = 1 : Int }"
      (Module ("Hello", [ Def (None, "hello", EAnn (ELit (Int 1), TCon "Int")) ]));
    test_parser_ast "parenthesized expression"
      "module Hello = { def hello = if (let x = True in x) then 1 else 2 }"
      (Module
         ( "Hello",
           [
             Def
               ( None,
                 "hello",
                 EIf
                   ( ELet ("x", None, ELit (Bool true), EVar "x"),
                     ELit (Int 1),
                     ELit (Int 2) ) );
           ] ));
    test_parser_ast "two definitions"
      "module Hello = { def hello = 1 def bye = hello }"
      (Module
         ( "Hello",
           [
             Def (None, "hello", ELit (Int 1)); Def (None, "bye", EVar "hello");
           ] ));
    test_parser_ast "let expression"
      "module Hello = { def hello = let x = 1 in x }"
      (Module
         ( "Hello",
           [ Def (None, "hello", ELet ("x", None, ELit (Int 1), EVar "x")) ] ));
    test_parser_ast "if expression"
      "module Hello = { def hello = if True then 1 else x }"
      (Module
         ( "Hello",
           [
             Def (None, "hello", EIf (ELit (Bool true), ELit (Int 1), EVar "x"));
           ] ));
    test_parser_ast "lambda expression with annotated parameter"
      "module Hello = { def hello = \\x : Int. x }"
      (Module
         ( "Hello",
           [ Def (None, "hello", ELam ("x", Some (TCon "Int"), EVar "x")) ] ));
    test_parser_ast "lambda expression" "module Hello = { def hello = \\x x }"
      (Module ("Hello", [ Def (None, "hello", ELam ("x", None, EVar "x")) ]));
    test_parser_ast "lambda expression annotated as a whole"
      "module Hello = { def hello = (\\x x) : Int -> Int }"
      (Module
         ( "Hello",
           [
             Def
               ( None,
                 "hello",
                 EAnn (ELam ("x", None, EVar "x"), TArr (TCon "Int", TCon "Int"))
               );
           ] ));
    test_parser_ast "function application"
      "module Hello = { def hello = f x y z }"
      (Module
         ( "Hello",
           [
             Def
               ( None,
                 "hello",
                 EApp (EApp (EApp (EVar "f", EVar "x"), EVar "y"), EVar "z") );
           ] ));
    test_parser_ast "function application"
      "module Hello = { def hello = f (g x) }"
      (Module
         ( "Hello",
           [ Def (None, "hello", EApp (EVar "f", EApp (EVar "g", EVar "x"))) ]
         ));
    test_parser_ast "top level type annotation"
      "module Hello = { def hello : Bool = True }"
      (Module
         ( "Hello",
           [ Def (Some (TMono (TCon "Bool")), "hello", ELit (Bool true)) ] ));
    test_parser_ast "top level polymorphic type"
      "module Hello = {
            def const : forall a b. a -> b -> a
              = \\x \\y x
          }"
      (Module
         ( "Hello",
           [
             Def
               ( Some
                   (TForall
                      ([ "a"; "b" ], TArr (TVar "a", TArr (TVar "b", TVar "a")))),
                 "const",
                 ELam ("x", None, ELam ("y", None, EVar "x")) );
           ] ));
    test_parser_ast "let-binding type annotation"
      "module Hello = { def hello = let x : Bool = True in x }"
      (Module
         ( "Hello",
           [
             Def
               ( None,
                 "hello",
                 ELet ("x", Some (TCon "Bool"), ELit (Bool true), EVar "x") );
           ] ));
    test_parser_ast "multi-parameter lambda"
      "module Hello = { def const : Int -> Int -> Int = \\x \\y x }"
      (Module
         ( "Hello",
           [
             Def
               ( Some (TMono (TArr (TCon "Int", TArr (TCon "Int", TCon "Int")))),
                 "const",
                 ELam ("x", None, ELam ("y", None, EVar "x")) );
           ] ));
    test_parser_ast "high order function type"
      "module Hello = { def hello : (Int -> Int) -> Int = \\f f 1 }"
      (Module
         ( "Hello",
           [
             Def
               ( Some (TMono (TArr (TArr (TCon "Int", TCon "Int"), TCon "Int"))),
                 "hello",
                 ELam ("f", None, EApp (EVar "f", ELit (Int 1))) );
           ] ));
    test_parser_ast "match expression with custom constructor"
      "module Hello = {
        def hello =
          match foo with
            | Wibble x y -> 1
            | Wobble -> 0
          end
      }"
      (Module
         ( "Hello",
           [
             Def
               ( None,
                 "hello",
                 EMatch
                   ( EVar "foo",
                     [
                       (PCon ("Wibble", [ "x"; "y" ]), ELit (Int 1));
                       (PCon ("Wobble", []), ELit (Int 0));
                     ] ) );
           ] ))
    (* TODO: Match on Bool *);
    test_parser_ast "type definition"
      "module Hello = {
        type AType =
          | Wibble Int Bool
          | Wobble
      }"
      Ast.(
        Module
          ( "Hello",
            [
              Type
                ( "AType",
                  [],
                  [ ("Wibble", [ TCon "Int"; TCon "Bool" ]); ("Wobble", []) ] );
            ] ));
    test_parser_ast "type definition may ommit first pipe"
      "module Hello = {
        type AType = Wibble Int Bool | Wobble
      }"
      Ast.(
        Module
          ( "Hello",
            [
              Type
                ( "AType",
                  [],
                  [ ("Wibble", [ TCon "Int"; TCon "Bool" ]); ("Wobble", []) ] );
            ] ));
    test_parser_ast "type constructors are expressions"
      "module Hello = {
        def wibble = Wibble
      }"
      Ast.(Module ("Hello", [ Def (None, "wibble", EVar "Wibble") ]));
    test_parser_ast "polymorphic type definitions"
      "module Hello = {
        type Either a b =
          | Left a
          | Right b
      }"
      Ast.(
        Module
          ( "Hello",
            [
              Type
                ( "Either",
                  [ "a"; "b" ],
                  [ ("Left", [ TVar "a" ]); ("Right", [ TVar "b" ]) ] );
            ] ));
    test_parser_ast "type application"
      "module Hello = {
        type Maybe a = None | Some a
        def foo : forall a . Maybe a = None
      }"
      Ast.(
        Module
          ( "Hello",
            [
              Type ("Maybe", [ "a" ], [ ("None", []); ("Some", [ TVar "a" ]) ]);
              Def
                ( Some (TForall ([ "a" ], TApp (TCon "Maybe", TVar "a"))),
                  "foo",
                  EVar "None" );
            ] ));
    test_parser_ast "type definitions with parens"
      "module Hello = {
        type List a =
          | Nil
          | Cons a (List a)
      }"
      Ast.(
        Module
          ( "Hello",
            [
              Type
                ( "List",
                  [ "a" ],
                  [
                    ("Nil", []);
                    ("Cons", [ TVar "a"; TApp (TCon "List", TVar "a") ]);
                  ] );
            ] ));
    test_parser_ast "dangling lambda syntax"
      "module Hello = {
        def main =
          bind tx \\x
          pure x
      }"
      Ast.(
        Module
          ( "Hello",
            [
              Def
                ( None,
                  "main",
                  EApp
                    ( EApp (EVar "bind", EVar "tx"),
                      ELam ("x", None, EApp (EVar "pure", EVar "x")) ) );
            ] ));
    test_parser_ast "line comments"
      "-- start comment
      module Hello = {
        -- a comment
        type List a =
          | Nil             -- another comment
          | Cons a (List a) -- third comment
      }-- end comment"
      Ast.(
        Module
          ( "Hello",
            [
              Type
                ( "List",
                  [ "a" ],
                  [
                    ("Nil", []);
                    ("Cons", [ TVar "a"; TApp (TCon "List", TVar "a") ]);
                  ] );
            ] ));
  ]

let suite = "Parser" >::: cst_tests @ ast_tests
