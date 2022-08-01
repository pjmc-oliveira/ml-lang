open OUnit2
open Ml_lang
open Extensions
open Result.Syntax
module Ctx = Solver.Ctx

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
  "Ok {\n" ^ "\ttypes = [\n" ^ types ^ "\n\t]\n" ^ "\tterms = [\n" ^ terms
  ^ "\n\t]\n" ^ "\n}"

let string_of_result r =
  match r with
  | Ok ctx -> string_of_ctx ctx
  | Error e -> "Error [" ^ String.concat "\n" (List.map Error.to_string e) ^ "]"

let error_to_lines (e : Error.t) : Error.Line.t list = e.lines
let errors_to_lines es = List.(map error_to_lines es)

let string_of_result_lines (r : (Solver.ty_ctx, Error.Line.t list list) result)
    =
  match r with
  | Ok ctx -> string_of_ctx ctx
  | Error e ->
      let lines = List.map (List.map Error.Line.show) e in
      let lines = List.map (String.concat "; ") lines in
      "Error [" ^ String.concat "\n" lines ^ "]"

let ty_ctx_equal l r =
  match (l, r) with
  | Ok l, Ok r -> Ctx.tm_equal ( = ) l r && Ctx.ty_equal ( = ) l r
  | _, _ -> l = r

let ty_ctx_equal_weak l r =
  match (l, r) with
  | Ok l, Ok r -> Ctx.tm_equal ( = ) l r && Ctx.ty_equal ( = ) l r
  | Error _, Error _ -> true
  | _, _ -> false

module Tester (S : Solver.S) = struct
  let solve_module str ctx =
    let src = Source.of_string str in
    let* tks = Lexer.tokens src in
    let* m = Parser.(parse module_ tks) in
    let* ctx = S.solve_module m ctx in
    Ok ctx

  let test_solver label str ?(skip = false) ?(initial_ctx = Ctx.empty)
      (expected_ctx : Ctx.t) =
    label >:: fun _ ->
    skip_if skip "Skipped test";
    assert_equal ~printer:string_of_result ~cmp:ty_ctx_equal (Ok expected_ctx)
      (solve_module str initial_ctx)

  let test_failure label str ?(skip = false) ?(initial_ctx = Ctx.empty)
      expected_lines =
    label >:: fun _ ->
    skip_if skip "Skipped test";
    let result = solve_module str initial_ctx in
    assert_equal
      ~printer:string_of_result_lines
        (* ~cmp:ty_ctx_equal *)
        (* TODO: Revert this to strong equality *)
      ~cmp:ty_ctx_equal_weak (Error expected_lines)
      (Result.map_error errors_to_lines result)
end

module Mono (S : Solver.S) = struct
  open Tester (S)

  let suite =
    "Mono Solver"
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
           test_solver "annotatated expression"
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
                      Poly ([ "t0" ], Arrow (Arrow (Int, Var "t0"), Var "t0")))
                  );
                ]);
           test_solver ~initial_ctx:BuiltIns.ty_ctx "built-in functions"
             "module Hello = {
                def my_add = \\x : Int. \\y : Int.
                  add x y
                def main = my_add 1 2
              }"
             Ctx.(
               union BuiltIns.ty_ctx
                 (of_terms_list
                    [
                      ("my_add", Type.(mono (Arrow (Int, Arrow (Int, Int)))));
                      ("main", Type.(mono Int));
                    ]));
           test_solver ~initial_ctx:BuiltIns.ty_ctx "recursive let binding"
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
               union BuiltIns.ty_ctx
                 (of_terms_list [ ("main", Type.(mono (Arrow (Int, Int)))) ]));
           test_solver ~initial_ctx:BuiltIns.ty_ctx
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
               union BuiltIns.ty_ctx
                 (of_terms_list
                    [
                      ("is_even", Type.(mono (Arrow (Int, Bool))));
                      ("is_odd", Type.(mono (Arrow (Int, Bool))));
                    ]));
           test_solver ~initial_ctx:BuiltIns.ty_ctx
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
               union BuiltIns.ty_ctx
                 (of_terms_list
                    [
                      ("is_even", Type.(mono (Arrow (Int, Bool))));
                      ("is_odd", Type.(mono (Arrow (Int, Bool))));
                    ]));
           test_solver "mutually recursive definitionns"
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
           test_solver ~initial_ctx:BuiltIns.ty_ctx "match on custom type"
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
               union BuiltIns.ty_ctx
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
           (* TODO: polymorphic types *)
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
                          ( [ "t0" ],
                            Arrow (Var "t0", App (Con "Maybe", Var "t0")) )) );
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
                            Arrow (f, Arrow (base, Arrow (list, Var "t1"))) ))
                    );
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
                                App (App (Con "Either", Var "t0"), Var "t1") )
                          )) );
                    ( "Right",
                      Type.(
                        Poly
                          ( [ "t0"; "t1" ],
                            Arrow
                              ( Var "t1",
                                App (App (Con "Either", Var "t0"), Var "t1") )
                          )) );
                  ]);
           (* TODO: exhaustivity check for match-with *)
           (* TODO: check kinds on type definitions *)
           (* Failures *)
           test_failure "let expression out-of-scope"
             "module Hello = { def foo = let x = 1 in x def main = x }"
             [ [ Text "Unbound variable: x" ] ];
           test_failure "unbound constructor" "module Hello = { def foo = Foo }"
             [ [ Text "Unbound variable: Foo" ] ];
           test_failure "if expression condition-not-bool"
             "module Hello = { def main = if 1 then 1 else 2 }"
             [
               [
                 Text
                   ("Expected if-condition to be type: " ^ Type.show_mono Bool);
                 Text ("But got type: " ^ Type.show_mono Int);
               ];
             ];
           test_failure "if expression branch-mismatch"
             "module Hello = { def main = if True then 1 else False }"
             [
               [
                 Text "If branches must have the same type";
                 Text ("then-branch has type: " ^ Type.show_mono Int);
                 Text ("but else-branch has type: " ^ Type.show_mono Bool);
               ];
             ];
           test_failure "cannot apply to non-function"
             "module Hello = { def main = 1 1 }"
             [ [ Text "Cannot a apply to non-function values" ] ];
           test_failure "wrong argument type"
             "module Hello = {
                def identity = \\x : Int. x
                def main = identity True
              }"
             [
               [
                 Text "Wrong argument type";
                 Text ("Expected: " ^ Type.show_mono Int);
                 Text ("But got: " ^ Type.show_mono Bool);
               ];
             ];
           test_failure "wrong annotatated expression"
             "module Hello = { def hello = 1 : Bool }"
             [
               [
                 Text "Type mismatch";
                 Text ("Expected: " ^ Type.show_mono Bool);
                 Text ("But got: " ^ Type.show_mono Int);
               ];
             ];
           test_failure "Wrong constructor type"
             "module Hello = {
                type Maybe = None | Some Int
                def foo = Some True
              }"
             [ (* TODO: Add error message *) ];
           test_failure "Match pattern with too many variables"
             "module Hello = {
                type Nat = Zero | Succ Nat
                def count =
                  match Succ Zero with
                    | Zero -> 0
                    | Succ x y -> 1
                  end
              }"
             [ (* TODO: Add error message *) ];
           test_failure "Match pattern with too few variables"
             "module Hello = {
                type Nat = Zero | Succ Nat
                def count =
                  match Succ Zero with
                    | Zero -> 0
                    | Succ -> 1
                  end
              }"
             [ (* TODO: Add error message *) ];
         ]
end

module Poly (S : Solver.S) = struct
  open Tester (S)

  let suite =
    "Poly Solver"
    >::: [
           (* Success *)
           test_solver "infer top-level polymorphic function"
             "module Hello = {
              def identity = \\x x
            }"
             (Ctx.of_terms_list
                [
                  ( "identity",
                    Type.(Poly ([ "t0" ], Arrow (Var "t0", Var "t0"))) );
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
                  ( "identity",
                    Type.(Poly ([ "t0" ], Arrow (Var "t0", Var "t0"))) );
                  ("main", Type.(mono Int));
                ]);
           (* TODO: fresh variable should not mix with explicitly annotated type variables *)
           (* TODO: Inferred type should match top-level annotationn if provided *)
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
                   ^ Type.(show_poly (Poly ([ "a" ], Arrow (Var "a", Var "a"))))
                   );
                 Text
                   ("But got: "
                   ^ Type.(show_poly (Poly ([ "a" ], Arrow (Var "a", Bool)))));
               ];
             ];
         ]
end
