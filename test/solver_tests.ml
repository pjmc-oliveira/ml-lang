open OUnit2
open Ml_lang
open Extensions

module W = Writer_option.Make (struct
  type t = Error.t list

  let empty = []
  let concat = ( @ )
end)

open W.Syntax

(* TODO: pretty print *)
let string_of_ctx ctx =
  let terms =
    String.concat ";\n"
      (List.map
         (fun (name, ty) ->
           "\t\t( \"" ^ name ^ "\", " ^ Type.show_poly ty ^ " )")
         (Ctx.to_terms_list ctx))
  in
  let types =
    String.concat ";\n"
      (List.map
         (fun (name, kind) ->
           "\t\t( \"" ^ name ^ "\", " ^ Type.show_kind kind ^ " )")
         (Ctx.to_types_list ctx))
  in
  "Ok {\n"
  ^ "\ttypes = [\n"
  ^ types
  ^ "\n\t]\n"
  ^ "\tterms = [\n"
  ^ terms
  ^ "\n\t]\n"
  ^ "\n}"

let string_of_result src (r, e) =
  match r with
  | Some ctx -> string_of_ctx ctx
  | None ->
      "Error [" ^ String.concat "\n" (List.map (Error.to_string src) e) ^ "]"

let error_to_lines (e : Error.t) : Error.Line.t list = e.lines
let errors_to_lines es = List.(map error_to_lines es)

let string_of_result_lines ((r, e) : Ctx.t option * Error.Line.t list list) =
  match r with
  | Some ctx -> string_of_ctx ctx
  | None ->
      let lines = List.map (List.map Error.Line.show) e in
      let lines = List.map (String.concat "; ") lines in
      "Error [" ^ String.concat "\n" lines ^ "]"

let ty_ctx_equal (l, e) (r, e') =
  match (l, r) with
  | Some l, Some r -> Ctx.tm_equal ( = ) l r && Ctx.ty_equal ( = ) l r
  | _, _ -> e = e'

let ty_ctx_equal_weak (l, _e) (r, _e') =
  match (l, r) with
  | Some l, Some r -> Ctx.tm_equal ( = ) l r && Ctx.ty_equal ( = ) l r
  | None, None -> true
  | _, _ -> false

let solve_module str ctx =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  let* ctx = Solver.solve_module m ctx in
  W.pure ctx

let test_solver label str ?(skip = false) ?(initial_ctx = Ctx.empty)
    (expected_ctx : Ctx.t) =
  label >:: fun _ ->
  skip_if skip "Skipped test";
  assert_equal
    ~printer:(string_of_result (Source.of_string str))
    ~cmp:ty_ctx_equal (Some expected_ctx, [])
    (solve_module str initial_ctx)

let test_failure label str ?(skip = false) ?(initial_ctx = Ctx.empty)
    expected_lines =
  label >:: fun _ ->
  skip_if skip "Skipped test";
  let result = solve_module str initial_ctx in
  assert_equal ~printer:string_of_result_lines ~cmp:ty_ctx_equal
    (* TODO: Revert this to strong equality *)
    (* ~cmp:ty_ctx_equal_weak *)
    (None, expected_lines)
    ((fun (x, e) -> (x, errors_to_lines e)) result)

let suite =
  "Solver"
  >::: [
         (* Successes *)
         test_solver "empty module" "module Hello = {}" Ctx.empty;
         test_solver "one binding" "module Hello = { def hello = 1 }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Int)) ]);
         test_solver "two bindings"
           "module Hello = { def hello = 1 def bye = hello }"
           (Ctx.of_terms_list
              [ ("hello", Type.(mono Int)); ("bye", Type.(mono Int)) ]);
         test_solver "boolean literals"
           "module Hello = { def hello = True def bye = False }"
           (Ctx.of_terms_list
              [ ("hello", Type.(mono Bool)); ("bye", Type.(mono Bool)) ]);
         test_solver "top-level define before use"
           "module Hello = { def hello = bye def bye = 1 }"
           (Ctx.of_terms_list
              [ ("hello", Type.(mono Int)); ("bye", Type.(mono Int)) ]);
         test_solver "let expression"
           "module Hello = { def hello = let x = 1 in x }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Int)) ]);
         test_solver "if expression True"
           "module Hello = { def hello = if True then 1 else 2 }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Int)) ]);
         test_solver "if expression False"
           "module Hello = { def hello = if False then False else True }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Bool)) ]);
         test_solver "lambda expression"
           "module Hello = { def hello = \\x : Int. True }"
           (Ctx.of_terms_list [ ("hello", Type.(mono (Arrow (Int, Bool)))) ]);
         test_solver "lambda expression annotated as a whole"
           "module Hello = { def hello = (\\x x) : Int -> Int }"
           (Ctx.of_terms_list [ ("hello", Type.(mono (Arrow (Int, Int)))) ]);
         test_solver "top level function annotation"
           "module Hello = { def hello : Int -> Bool  = \\x False }"
           (Ctx.of_terms_list [ ("hello", Type.(mono (Arrow (Int, Bool)))) ]);
         test_solver "top level recursive value with annotation"
           "module Hello = { def hello : Int  = hello }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Int)) ]);
         test_solver "annotated expression"
           "module Hello = { def hello = 1 : Int }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Int)) ]);
         test_solver "function application"
           "module Hello = {
                def identity = \\x : Int. x
                def hello = identity 1
              }"
           (Ctx.of_terms_list
              [
                ("identity", Type.(mono (Arrow (Int, Int))));
                ("hello", Type.(mono Int));
              ]);
         test_solver "infer function application"
           "module Hello = {
                def hello = (\\x \\y x) True 0
              }"
           (Ctx.of_terms_list [ ("hello", Type.(mono Bool)) ]);
         test_solver "nested function application"
           "module Hello = {
                def identity = \\x : Int. x
                def hello = identity (identity 1)
              }"
           (Ctx.of_terms_list
              [
                ("identity", Type.(mono (Arrow (Int, Int))));
                ("hello", Type.(mono Int));
              ]);
         test_solver "can solve annotated higher order function"
           "module Hello = {
                def hello : (Int -> Int) -> Int = \\f
                  f 1
              }"
           (Ctx.of_terms_list
              [ ("hello", Type.(mono (Arrow (Arrow (Int, Int), Type.Int)))) ]);
         test_solver "can solve un-annotated higher order function"
           "module Hello = {
              def hello = \\f
                f 1
            }"
           (Ctx.of_terms_list
              [
                ( "hello",
                  Type.(
                    Poly ([ "t0" ], Arrow (Arrow (Int, Var "t0"), Var "t0"))) );
              ]);
         test_solver ~initial_ctx:Built_ins.ty_ctx "built-in functions"
           "module Hello = {
              def my_add = \\x : Int. \\y : Int.
                add x y
              def main = my_add 1 2
            }"
           Ctx.(
             union Built_ins.ty_ctx
               (of_terms_list
                  [
                    ("my_add", Type.(mono (Arrow (Int, Arrow (Int, Int)))));
                    ("main", Type.(mono Int));
                  ]));
         test_solver ~initial_ctx:Built_ins.ty_ctx "recursive let binding"
           "module Hello = {
              def main =
                let fact : Int -> Int = \\x
                  if eq 0 x then
                    1
                  else
                    mul x (fact (sub x 1))
                in
                  fact
            }"
           Ctx.(
             union Built_ins.ty_ctx
               (of_terms_list [ ("main", Type.(mono (Arrow (Int, Int)))) ]));
         test_solver ~initial_ctx:Built_ins.ty_ctx
           "annotated mutually recursive functions"
           "module Hello = {
              def is_even : Int -> Bool = \\x
                if eq 0 x then
                  True
                else
                  not (is_odd (sub x 1))

              def is_odd : Int -> Bool = \\x
                if eq 0 x then
                  False
                else
                  not (is_even (sub x 1))
            }"
           Ctx.(
             union Built_ins.ty_ctx
               (of_terms_list
                  [
                    ("is_even", Type.(mono (Arrow (Int, Bool))));
                    ("is_odd", Type.(mono (Arrow (Int, Bool))));
                  ]));
         test_solver ~initial_ctx:Built_ins.ty_ctx
           "un-annotated mutually recursive functions"
           "module Hello = {
              def is_even = \\x
                if eq 0 x then
                  True
                else
                  not (is_odd (sub x 1))

              def is_odd = \\x
                if eq 0 x then
                  False
                else
                  not (is_even (sub x 1))
            }"
           Ctx.(
             union Built_ins.ty_ctx
               (of_terms_list
                  [
                    ("is_even", Type.(mono (Arrow (Int, Bool))));
                    ("is_odd", Type.(mono (Arrow (Int, Bool))));
                  ]));
         test_solver "mutually recursive definitions"
           "module Hello = {
              def ping = ping
              def pong = pong
            }"
           (Ctx.of_terms_list
              [
                ("ping", Type.(Poly ([ "t0" ], Var "t0")));
                ("pong", Type.(Poly ([ "t0" ], Var "t0")));
              ]);
         test_solver "custom type definition"
           "module Hello = {
              type AType = Wibble | Wobble
              def wibble = Wibble
            }"
           (Ctx.of_list
              ~types:[ ("AType", Type.KType) ]
              ~terms:
                [
                  ("wibble", Type.(mono (Con "AType")));
                  ("Wibble", Type.(mono (Con "AType")));
                  ("Wobble", Type.(mono (Con "AType")));
                ]);
         test_solver "custom type definition with extra types"
           "module Hello = {
              type AType = Wibble | Wobble Int Bool
              def wobble1 = Wobble 1 True
              def wobble2 = Wobble
            }"
           (Ctx.of_list
              ~types:[ ("AType", Type.KType) ]
              ~terms:
                [
                  ("wobble1", Type.(mono (Con "AType")));
                  ( "wobble2",
                    Type.(mono (Arrow (Int, Arrow (Bool, Con "AType")))) );
                  ("Wibble", Type.(mono (Con "AType")));
                  ( "Wobble",
                    Type.(mono (Arrow (Int, Arrow (Bool, Con "AType")))) );
                ]);
         test_solver "custom recursive type"
           "module Hello = {
              type Nat = Zero | Succ Nat
              def add1 = Succ
            }"
           (Ctx.of_list
              ~types:[ ("Nat", Type.KType) ]
              ~terms:
                [
                  ("add1", Type.(mono (Arrow (Con "Nat", Con "Nat"))));
                  ("Succ", Type.(mono (Arrow (Con "Nat", Con "Nat"))));
                  ("Zero", Type.(mono (Con "Nat")));
                ]);
         test_solver ~initial_ctx:Built_ins.ty_ctx "match on custom type"
           "module Hello = {
              type Nat = Zero | Succ Nat
              def two = Succ (Succ Zero)
              def count = \\x
                match x with
                  | Zero -> 0
                  | Succ y -> add 1 (count y)
                end
            }"
           Ctx.(
             union Built_ins.ty_ctx
               (of_list
                  ~types:[ ("Nat", Type.KType) ]
                  ~terms:
                    [
                      ("two", Type.(mono (Con "Nat")));
                      ("count", Type.(mono (Arrow (Con "Nat", Int))));
                      ("Succ", Type.(mono (Arrow (Con "Nat", Con "Nat"))));
                      ("Zero", Type.(mono (Con "Nat")));
                    ]));
         (* TODO: mutually recursive types *)
         test_solver "polymorphic type definition"
           "module Hello = {
              type Maybe a = None | Some a
              def an_int = Some 1
            }"
           (Ctx.of_list
              ~types:[ ("Maybe", Type.(KArrow (KType, KType))) ]
              ~terms:
                [
                  ("an_int", Type.(mono (App (Con "Maybe", Int))));
                  ("None", Type.(Poly ([ "t0" ], App (Con "Maybe", Var "t0"))));
                  ( "Some",
                    Type.(
                      Poly
                        ([ "t0" ], Arrow (Var "t0", App (Con "Maybe", Var "t0"))))
                  );
                ]);
         test_solver "polymorphic type use"
           "module Hello = {
              type List a =
                | Nil
                | Cons a (List a)

              def fold_right = \\f \\base \\list
                match list with
                  | Nil -> base
                  | Cons x xs -> f x (fold_right f base xs)
                end
            }"
           (Ctx.of_list
              ~types:[ ("List", Type.(KArrow (KType, KType))) ]
              ~terms:
                [
                  ( "fold_right",
                    Type.(
                      let f = Arrow (Var "t0", Arrow (Var "t1", Var "t1")) in
                      let base = Var "t1" in
                      let list = App (Con "List", Var "t0") in
                      Poly
                        ( [ "t0"; "t1" ],
                          Arrow (f, Arrow (base, Arrow (list, Var "t1"))) )) );
                  ("Nil", Type.(Poly ([ "t0" ], App (Con "List", Var "t0"))));
                  ( "Cons",
                    Type.(
                      Poly
                        ( [ "t0" ],
                          Arrow
                            ( Var "t0",
                              Arrow
                                ( App (Con "List", Var "t0"),
                                  App (Con "List", Var "t0") ) ) )) );
                ]);
         test_solver "multi-parameter polymorphic type definition"
           "module Hello = {
              type Either a b = Left a | Right b
            }"
           (Ctx.of_list
              ~types:
                [ ("Either", Type.(KArrow (KType, KArrow (KType, KType)))) ]
              ~terms:
                [
                  ( "Left",
                    Type.(
                      Poly
                        ( [ "t0"; "t1" ],
                          Arrow
                            ( Var "t0",
                              App (App (Con "Either", Var "t0"), Var "t1") ) ))
                  );
                  ( "Right",
                    Type.(
                      Poly
                        ( [ "t0"; "t1" ],
                          Arrow
                            ( Var "t1",
                              App (App (Con "Either", Var "t0"), Var "t1") ) ))
                  );
                ]);
         test_solver "higher-kinded type definition"
           "module Hello = {
              type P f = MkP (f Int)
            }"
           (Ctx.of_list
              ~types:[ ("P", Type.(KArrow (KArrow (KType, KType), KType))) ]
              ~terms:
                [
                  ( "MkP",
                    Type.(
                      Poly
                        ( [ "t0" ],
                          Arrow (App (Var "t0", Int), App (Con "P", Var "t0"))
                        )) );
                ]);
         test_solver "mutually recursive type definition"
           "module Hello = {
              type Expr = EBlock Stmt
              type Stmt = SExpr Expr
            }"
           (Ctx.of_list
              ~types:[ ("Expr", Type.(KType)); ("Stmt", Type.(KType)) ]
              ~terms:
                [
                  ("EBlock", Type.(mono (Arrow (Con "Stmt", Con "Expr"))));
                  ("SExpr", Type.(mono (Arrow (Con "Expr", Con "Stmt"))));
                ]);
         test_solver "infer top-level polymorphic function"
           "module Hello = {
              def identity = \\x x
            }"
           (Ctx.of_terms_list
              [
                ("identity", Type.(Poly ([ "t0" ], Arrow (Var "t0", Var "t0"))));
              ]);
         test_solver "apply polymorphic function"
           "module Hello = {
              def identity = \\x x
              def main =
                if identity True then
                  1
                else
                  2
            }"
           (Ctx.of_terms_list
              [
                ("identity", Type.(Poly ([ "t0" ], Arrow (Var "t0", Var "t0"))));
                ("main", Type.(mono Int));
              ]);
         (* TODO: fresh variable should not mix with explicitly annotated type variables *)
         (* TODO: Inferred type should match top-level annotation if provided *)
         test_solver ~skip:true "apply annotated polymorphic function"
           "module Hello = {
              def identity : forall a. a -> a = \\x x
              def main =
                if identity True then
                  1
                else
                  2
            }"
           (Ctx.of_terms_list
              [
                ("identity", Type.(Poly ([ "a" ], Arrow (Var "a", Var "a"))));
                ("main", Type.(mono Int));
              ]);
         test_solver "infer un-annotated function"
           "module Hello = {
              def identity = \\x True
              def main =
                if identity True then
                  1
                else
                  2
            }"
           (Ctx.of_terms_list
              [
                ("identity", Type.(Poly ([ "t0" ], Arrow (Var "t0", Bool))));
                ("main", Type.(mono Int));
              ]);
         (* Failure *)
         (* Failures *)
         test_failure "disallow duplicate top-level definitions"
           "module Hello = {
              def x = 1
              def x = 2
            }"
           [
             [
               Text "Duplicate definitions of: x";
               Quote { index = 31; line = 2; column = 15; length = 9 };
               Quote { index = 55; line = 3; column = 15; length = 9 };
             ];
           ];
         test_failure "disallow duplicate type definitions"
           "module Hello = {
              type X = X1
              type X = X2
            }"
           [
             [
               Text "Duplicate definitions of: X";
               Quote { index = 31; line = 2; column = 15; length = 11 };
               Quote { index = 57; line = 3; column = 15; length = 11 };
             ];
           ];
         test_failure
           "disallow duplicate constructor definitions in different types"
           "module Hello = {
              type X = Z
              type Y = Z
            }"
           [
             [
               Text "Duplicate definitions of: Z";
               Quote { index = 31; line = 2; column = 15; length = 10 };
               Quote { index = 56; line = 3; column = 15; length = 10 };
             ];
           ];
         test_failure "disallow duplicate constructor definitions in same type"
           "module Hello = {
              type X = Z | Z
            }"
           [
             [
               Text "Duplicate definitions of: Z";
               Quote { index = 31; line = 2; column = 15; length = 14 };
             ];
           ];
         test_failure "let expression out-of-scope"
           "module Hello = { def foo = let x = 1 in x def main = x }"
           [
             [
               Text "Unbound variable: x";
               Quote { index = 53; line = 1; column = 54; length = 1 };
             ];
           ];
         test_failure "unbound constructor" "module Hello = { def foo = Foo }"
           [
             [
               Text "Unbound variable: Foo";
               Quote { index = 27; line = 1; column = 28; length = 3 };
             ];
           ];
         test_failure "if expression condition-not-bool"
           "module Hello = { def main = if 1 then 1 else 2 }"
           [
             [
               Text "Cannot solve constraint: Bool = Int";
               Text "Got Int from:";
               Quote { index = 31; line = 1; column = 32; length = 1 };
               Text "But if-condition must be Bool";
             ];
           ];
         test_failure "if expression branch-mismatch"
           "module Hello = { def main = if True then 1 else False }"
           [
             [
               Text "Cannot solve constraint: Int = Bool";
               Text "Got Int from:";
               Quote { index = 41; line = 1; column = 42; length = 1 };
               Text "And Bool from:";
               Quote { index = 48; line = 1; column = 49; length = 5 };
               Text "If branches must have the same type";
             ];
           ];
         test_failure "cannot apply to non-function"
           "module Hello = { def main = 1 1 }"
           [
             [
               Text "Cannot solve constraint: Int = Int -> t1";
               Text "Got Int from:";
               Quote { index = 28; line = 1; column = 29; length = 1 };
               Text "And Int from:";
               Quote { index = 30; line = 1; column = 31; length = 1 };
             ];
           ];
         test_failure "wrong argument type"
           "module Hello = {
              def identity = \\x : Int. x
              def main = identity True
            }"
           [
             [
               Text "Cannot solve constraint: Int = Bool";
               Text "Got Int -> Int from:";
               Quote { index = 83; line = 3; column = 26; length = 8 };
               Text "And Bool from:";
               Quote { index = 92; line = 3; column = 35; length = 4 };
             ];
           ];
         test_failure "wrong annotated expression"
           "module Hello = { def hello = 1 : Bool }"
           [
             [
               Text "Cannot solve constraint: Int = Bool";
               Text "Got Int from:";
               Quote { index = 29; line = 1; column = 30; length = 1 };
               Text "And Bool from:";
               Quote { index = 33; line = 1; column = 34; length = 4 };
             ];
           ];
         test_failure "wrong constructor type"
           "module Hello = {
              type Maybe = None | Some Int
              def foo = Some True
            }"
           [
             [
               Text "Cannot solve constraint: Int = Bool";
               Text "Got Int -> Maybe from:";
               Quote { index = 84; line = 3; column = 25; length = 4 };
               Text "And Bool from:";
               Quote { index = 89; line = 3; column = 30; length = 4 };
             ];
           ];
         test_failure "Match pattern with too many variables"
           "module Hello = {
              type Nat = Zero | Succ Nat
              def count =
                match Succ Zero with
                  | Zero -> 0
                  | Succ x y -> 1
                end
            }"
           [
             [
               Text "Arity mismatch for constructor: Succ";
               Quote { index = 171; line = 6; column = 21; length = 8 };
               Text "Expected 1 variable(s)";
               Text "But got 2 variable(s)";
             ];
           ];
         test_failure "Match pattern with too few variables"
           "module Hello = {
              type Nat = Zero | Succ Nat
              def count =
                match Succ Zero with
                  | Zero -> 0
                  | Succ -> 1
                end
            }"
           [
             [
               Text "Arity mismatch for constructor: Succ";
               Quote { index = 171; line = 6; column = 21; length = 4 };
               Text "Expected 1 variable(s)";
               Text "But got 0 variable(s)";
             ];
           ];
         test_failure "Type definition with wrong kind"
           "module Hello = {
              type List a =
                | Nil
                | Cons a List
            }"
           [ [ Text "Cannot solve kind constraint: Type -> Type = Type" ] ];
         test_failure "Type definition with wrong kind"
           "module Hello = {
              type List a =
                | Nil
                | Cons a (Int a)
            }"
           [ [ Text "Cannot solve kind constraint: Type = Type -> k3" ] ];
         test_failure "mismatch kinds in type definition"
           "module Hello = {
                type P f = MkP (f Int) f
              }"
           [ [ Text "Cannot solve kind constraint: Type -> Type = Type" ] ];
         (* TODO: top-level annotation should restrict the inferred type *)
         test_failure ~skip:true "incorrect top-level annotation"
           "module Hello = {
              def identity : forall a. a -> a = \\x True
              def main =
                if identity True then
                  1
                else
                  2
            }"
           [
             [
               Text "Type mismatch";
               Text
                 ("Expected: "
                 ^ Type.(show_poly (Poly ([ "a" ], Arrow (Var "a", Var "a")))));
               Text
                 ("But got: "
                 ^ Type.(show_poly (Poly ([ "a" ], Arrow (Var "a", Bool)))));
             ];
           ];
         test_failure "match-expression not exhaustive"
           "module Hello = {
              type Maybe a = None | Some a
              def main = \\m
                match m with
                  | None -> 1
                end
            }"
           [
             [
               Text "Match not exhaustive";
               Quote { index = 104; line = 4; column = 17; length = 62 };
               Text "Missing patterns: Some";
             ];
           ];
         test_failure "match-expression with overlapping patterns"
           "module Hello = {
              type Maybe a = None | Some a
              def main = \\m
                match m with
                  | None -> 1
                  | Some x -> 1
                  | Some y -> 3
                end
            }"
           [
             [
               Text "Overlapping patterns: Some";
               Quote { index = 167; line = 6; column = 21; length = 4 };
               Quote { index = 199; line = 7; column = 21; length = 4 };
             ];
           ];
         test_failure "match-expression with mismatch branch types"
           "module Hello = {
              type Maybe a = None | Some a
              def main = \\m
                match m with
                  | None -> 1
                  | Some x -> True
                end
            }"
           [
             [
               Text "Cannot solve constraint: Int = Bool";
               Text "Got Int from:";
               Quote { index = 145; line = 5; column = 29; length = 1 };
               Text "And Bool from:";
               Quote { index = 177; line = 6; column = 31; length = 4 };
             ];
           ];
       ]
