open OUnit2
open Ml_lang
open Extensions
module Cst = Syn.Cst
module Ast = Syn.Ast

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
  | Some m -> "Some " ^ Cst.Module.show m
  | None -> "None " ^ String.concat "\n" (List.map (Error.to_string src) e)

let test_parser_cst label str cst =
  label >:: fun _ ->
  assert_equal (Some cst, []) (parse_module str)
    ~printer:(string_of_cst_result (Source.of_string str))

let string_of_ast_result src (r, e) =
  (* TODO: Print errors if present *)
  match r with
  | Some m -> "Ok " ^ Ast.Module.show m
  | None -> "Error " ^ String.concat "\n" (List.map (Error.to_string src) e)

let test_parser_ast label str ast =
  label >:: fun _ ->
  assert_equal (Some ast, [])
    (W.map Syn.cst_to_ast (parse_module str))
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
                 Expr.Lit
                   ( { index = 29; line = 1; column = 30; length = 1 },
                     Lit.Int
                       ({ index = 29; line = 1; column = 30; length = 1 }, 1) )
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
                 Expr.Lit
                   ( { index = 29; line = 1; column = 30; length = 1 },
                     Lit.Int
                       ({ index = 29; line = 1; column = 30; length = 1 }, 1) )
               );
             Def
               ( { index = 31; line = 1; column = 32; length = 11 },
                 "bye",
                 None,
                 Expr.Lit
                   ( { index = 41; line = 1; column = 42; length = 1 },
                     Lit.Int
                       ({ index = 41; line = 1; column = 42; length = 1 }, 2) )
               );
           ] ));
  ]

let ast_tests =
  let open Ast in
  [
    test_parser_ast "empty module" "module Hello = {}"
      (Module ((), "Hello", []));
    test_parser_ast "one definition" "module Hello = { def hello = 1 }"
      (Module
         ((), "Hello", [ Def ((), "hello", None, Expr.Lit ((), Int ((), 1))) ]));
    test_parser_ast "True literal" "module Hello = { def hello = True }"
      (Module
         ( (),
           "Hello",
           [ Def ((), "hello", None, Expr.Lit ((), Bool ((), true))) ] ));
    test_parser_ast "True literal" "module Hello = { def hello = False }"
      (Module
         ( (),
           "Hello",
           [ Def ((), "hello", None, Expr.Lit ((), Bool ((), false))) ] ));
    test_parser_ast "type annotated expression"
      "module Hello = { def hello = 1 : Int }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.Ann ((), Expr.Lit ((), Int ((), 1)), Type.Con ((), "Int"))
               );
           ] ));
    test_parser_ast "parenthesized expression"
      "module Hello = { def hello = if (let x = True in x) then 1 else 2 }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.If
                   ( (),
                     Expr.Let
                       ( (),
                         "x",
                         None,
                         Expr.Lit ((), Bool ((), true)),
                         Expr.Var ((), "x") ),
                     Expr.Lit ((), Int ((), 1)),
                     Expr.Lit ((), Int ((), 2)) ) );
           ] ));
    test_parser_ast "two definitions"
      "module Hello = { def hello = 1 def bye = hello }"
      (Module
         ( (),
           "Hello",
           [
             Def ((), "hello", None, Expr.Lit ((), Int ((), 1)));
             Def ((), "bye", None, Expr.Var ((), "hello"));
           ] ));
    test_parser_ast "let expression"
      "module Hello = { def hello = let x = 1 in x }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.Let
                   ( (),
                     "x",
                     None,
                     Expr.Lit ((), Int ((), 1)),
                     Expr.Var ((), "x") ) );
           ] ));
    test_parser_ast "if expression"
      "module Hello = { def hello = if True then 1 else x }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.If
                   ( (),
                     Expr.Lit ((), Bool ((), true)),
                     Expr.Lit ((), Int ((), 1)),
                     Expr.Var ((), "x") ) );
           ] ));
    test_parser_ast "lambda expression with annotated parameter"
      "module Hello = { def hello = \\x : Int. x }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.Lam
                   ((), "x", Some (Type.Con ((), "Int")), Expr.Var ((), "x")) );
           ] ));
    test_parser_ast "lambda expression" "module Hello = { def hello = \\x x }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ((), "hello", None, Expr.Lam ((), "x", None, Expr.Var ((), "x")));
           ] ));
    test_parser_ast "lambda expression annotated as a whole"
      "module Hello = { def hello = (\\x x) : Int -> Int }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.Ann
                   ( (),
                     Expr.Lam ((), "x", None, Expr.Var ((), "x")),
                     Type.Arr ((), Type.Con ((), "Int"), Type.Con ((), "Int"))
                   ) );
           ] ));
    test_parser_ast "function application"
      "module Hello = { def hello = f x y z }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.App
                   ( (),
                     Expr.App
                       ( (),
                         Expr.App ((), Expr.Var ((), "f"), Expr.Var ((), "x")),
                         Expr.Var ((), "y") ),
                     Expr.Var ((), "z") ) );
           ] ));
    test_parser_ast "function application"
      "module Hello = { def hello = f (g x) }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.App
                   ( (),
                     Expr.Var ((), "f"),
                     Expr.App ((), Expr.Var ((), "g"), Expr.Var ((), "x")) ) );
           ] ));
    test_parser_ast "top level type annotation"
      "module Hello = { def hello : Bool = True }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 Some (Scheme.Type ((), Type.Con ((), "Bool"))),
                 Expr.Lit ((), Bool ((), true)) );
           ] ));
    test_parser_ast "top level polymorphic type"
      "module Hello = {
            def const : forall a b. a -> b -> a
              = \\x \\y x
          }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "const",
                 Some
                   (Scheme.Forall
                      ( (),
                        [ "a"; "b" ],
                        Type.Arr
                          ( (),
                            Type.Var ((), "a"),
                            Type.Arr ((), Type.Var ((), "b"), Type.Var ((), "a"))
                          ) )),
                 Expr.Lam
                   ((), "x", None, Expr.Lam ((), "y", None, Expr.Var ((), "x")))
               );
           ] ));
    test_parser_ast "let-binding type annotation"
      "module Hello = { def hello = let x : Bool = True in x }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 None,
                 Expr.Let
                   ( (),
                     "x",
                     Some (Type.Con ((), "Bool")),
                     Expr.Lit ((), Bool ((), true)),
                     Expr.Var ((), "x") ) );
           ] ));
    test_parser_ast "multi-parameter lambda"
      "module Hello = { def const : Int -> Int -> Int = \\x \\y x }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "const",
                 Some
                   (Scheme.Type
                      ( (),
                        Type.Arr
                          ( (),
                            Type.Con ((), "Int"),
                            Type.Arr
                              ((), Type.Con ((), "Int"), Type.Con ((), "Int"))
                          ) )),
                 Expr.Lam
                   ((), "x", None, Expr.Lam ((), "y", None, Expr.Var ((), "x")))
               );
           ] ));
    test_parser_ast "high order function type"
      "module Hello = { def hello : (Int -> Int) -> Int = \\f f 1 }"
      (Module
         ( (),
           "Hello",
           [
             Def
               ( (),
                 "hello",
                 Some
                   (Scheme.Type
                      ( (),
                        Type.Arr
                          ( (),
                            Type.Arr
                              ((), Type.Con ((), "Int"), Type.Con ((), "Int")),
                            Type.Con ((), "Int") ) )),
                 Expr.Lam
                   ( (),
                     "f",
                     None,
                     Expr.App
                       ((), Expr.Var ((), "f"), Expr.Lit ((), Int ((), 1))) ) );
           ] ));
    test_parser_ast "match expression with custom constructor"
      "module Hello = {
        def hello =
          match foo with
            | Wibble x y -> 1
            | Wobble -> 0
          end
      }"
      Ast.(
        Module
          ( (),
            "Hello",
            [
              Def
                ( (),
                  "hello",
                  None,
                  Expr.Match
                    ( (),
                      Expr.Var ((), "foo"),
                      Non_empty.of_list
                        [
                          ( Pat.Con
                              ((), ("Wibble", ()), [ ("x", ()); ("y", ()) ]),
                            Expr.Lit ((), Int ((), 1)) );
                          ( Pat.Con ((), ("Wobble", ()), []),
                            Expr.Lit ((), Int ((), 0)) );
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
          ( (),
            "Hello",
            [
              Type
                ( (),
                  "AType",
                  [],
                  [
                    ("Wibble", [ Type.Con ((), "Int"); Type.Con ((), "Bool") ]);
                    ("Wobble", []);
                  ] );
            ] ));
    test_parser_ast "type definition may ommit first pipe"
      "module Hello = {
        type AType = Wibble Int Bool | Wobble
      }"
      Ast.(
        Module
          ( (),
            "Hello",
            [
              Type
                ( (),
                  "AType",
                  [],
                  [
                    ("Wibble", [ Type.Con ((), "Int"); Type.Con ((), "Bool") ]);
                    ("Wobble", []);
                  ] );
            ] ));
    test_parser_ast "type constructors are expressions"
      "module Hello = {
        def wibble = Wibble
      }"
      Ast.(
        Module
          ((), "Hello", [ Def ((), "wibble", None, Expr.Var ((), "Wibble")) ]));
    test_parser_ast "polymorphic type definitions"
      "module Hello = {
        type Either a b =
          | Left a
          | Right b
      }"
      Ast.(
        Module
          ( (),
            "Hello",
            [
              Type
                ( (),
                  "Either",
                  [ "a"; "b" ],
                  [
                    ("Left", [ Type.Var ((), "a") ]);
                    ("Right", [ Type.Var ((), "b") ]);
                  ] );
            ] ));
    test_parser_ast "type application"
      "module Hello = {
        type Maybe a = None | Some a
        def foo : forall a . Maybe a = None
      }"
      Ast.(
        Module
          ( (),
            "Hello",
            [
              Type
                ( (),
                  "Maybe",
                  [ "a" ],
                  [ ("None", []); ("Some", [ Type.Var ((), "a") ]) ] );
              Def
                ( (),
                  "foo",
                  Some
                    (Scheme.Forall
                       ( (),
                         [ "a" ],
                         Type.App
                           ((), Type.Con ((), "Maybe"), Type.Var ((), "a")) )),
                  Expr.Var ((), "None") );
            ] ));
    test_parser_ast "type definitions with parens"
      "module Hello = {
        type List a =
          | Nil
          | Cons a (List a)
      }"
      Ast.(
        Module
          ( (),
            "Hello",
            [
              Type
                ( (),
                  "List",
                  [ "a" ],
                  [
                    ("Nil", []);
                    ( "Cons",
                      [
                        Type.Var ((), "a");
                        Type.App ((), Type.Con ((), "List"), Type.Var ((), "a"));
                      ] );
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
          ( (),
            "Hello",
            [
              Def
                ( (),
                  "main",
                  None,
                  Expr.App
                    ( (),
                      Expr.App ((), Expr.Var ((), "bind"), Expr.Var ((), "tx")),
                      Expr.Lam
                        ( (),
                          "x",
                          None,
                          Expr.App
                            ((), Expr.Var ((), "pure"), Expr.Var ((), "x")) ) )
                );
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
          ( (),
            "Hello",
            [
              Type
                ( (),
                  "List",
                  [ "a" ],
                  [
                    ("Nil", []);
                    ( "Cons",
                      [
                        Type.Var ((), "a");
                        Type.App ((), Type.Con ((), "List"), Type.Var ((), "a"));
                      ] );
                  ] );
            ] ));
  ]

let suite = "Parser" >::: cst_tests @ ast_tests
