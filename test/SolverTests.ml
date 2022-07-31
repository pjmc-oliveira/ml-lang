open OUnit2
open Ml_lang
open Extensions
open Result.Syntax
module TyCtx = Solver.TyCtx

let string_of_result r =
  match r with
  | Ok ctx ->
      "Ok ["
      ^ String.concat "; "
          (List.map
             (fun (name, ty) ->
               "( \"" ^ name ^ "\", " ^ Type.show_poly ty ^ " )")
             (TyCtx.to_list ctx))
      ^ "]"
  | Error e -> "Error [" ^ String.concat "\n" (List.map Error.to_string e) ^ "]"

let error_to_lines (e : Error.t) : Error.Line.t list = e.lines
let errors_to_lines es = List.(map error_to_lines es)

let string_of_result_lines (r : (Solver.ty_ctx, Error.Line.t list list) result)
    =
  match r with
  | Ok ctx ->
      "Ok ["
      ^ String.concat "; "
          (List.map
             (fun (name, ty) ->
               "( \"" ^ name ^ "\", " ^ Type.show_poly ty ^ " )")
             (TyCtx.to_list ctx))
      ^ "]"
  | Error e ->
      let lines = List.map (List.map Error.Line.show) e in
      let lines = List.map (String.concat "; ") lines in
      "Error [" ^ String.concat "\n" lines ^ "]"

let ty_ctx_equal l r =
  match (l, r) with
  | Ok l, Ok r -> TyCtx.equal ( = ) l r
  | _, _ -> l = r

let ty_ctx_equal_weak l r =
  match (l, r) with
  | Ok l, Ok r -> TyCtx.equal ( = ) l r
  | Error _, Error _ -> true
  | _, _ -> false

module Tester (S : Solver.S) = struct
  let solve_module str ctx =
    let src = Source.of_string str in
    let* tks = Lexer.tokens src in
    let* m = Parser.(parse module_ tks) in
    let* ctx = S.solve_module m ctx in
    Ok ctx

  let test_solver label str ?(skip = false) ?(initial_ctx = TyCtx.empty)
      (expected_ctx : Type.poly TyCtx.t) =
    label >:: fun _ ->
    skip_if skip "Skipped test";
    assert_equal ~printer:string_of_result ~cmp:ty_ctx_equal (Ok expected_ctx)
      (solve_module str initial_ctx)

  let test_failure label str ?(skip = false) ?(initial_ctx = TyCtx.empty)
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
           test_solver "empty module" "module Hello = {}" TyCtx.empty;
           test_solver "one binding" "module Hello = { def hello = 1 }"
             (TyCtx.of_list [ ("hello", Type.(mono Int)) ]);
           test_solver "two bindings"
             "module Hello = { def hello = 1 def bye = hello }"
             (TyCtx.of_list
                [ ("hello", Type.(mono Int)); ("bye", Type.(mono Int)) ]);
           test_solver "boolean literals"
             "module Hello = { def hello = True def bye = False }"
             (TyCtx.of_list
                [ ("hello", Type.(mono Bool)); ("bye", Type.(mono Bool)) ]);
           test_solver "top-level define before use"
             "module Hello = { def hello = bye def bye = 1 }"
             (TyCtx.of_list
                [ ("hello", Type.(mono Int)); ("bye", Type.(mono Int)) ]);
           test_solver "let expression"
             "module Hello = { def hello = let x = 1 in x }"
             (TyCtx.of_list [ ("hello", Type.(mono Int)) ]);
           test_solver "if expression True"
             "module Hello = { def hello = if True then 1 else 2 }"
             (TyCtx.of_list [ ("hello", Type.(mono Int)) ]);
           test_solver "if expression False"
             "module Hello = { def hello = if False then False else True }"
             (TyCtx.of_list [ ("hello", Type.(mono Bool)) ]);
           test_solver "lambda expression"
             "module Hello = { def hello = \\x : Int. True }"
             (TyCtx.of_list [ ("hello", Type.(mono (Arrow (Int, Bool)))) ]);
           test_solver "lambda expression annotated as a whole"
             "module Hello = { def hello = (\\x x) : Int -> Int }"
             (TyCtx.of_list [ ("hello", Type.(mono (Arrow (Int, Int)))) ]);
           test_solver "top level function annotation"
             "module Hello = { def hello : Int -> Bool  = \\x False }"
             (TyCtx.of_list [ ("hello", Type.(mono (Arrow (Int, Bool)))) ]);
           test_solver "top level recursive value with annotation"
             "module Hello = { def hello : Int  = hello }"
             (TyCtx.of_list [ ("hello", Type.(mono Int)) ]);
           test_solver "annotatated expression"
             "module Hello = { def hello = 1 : Int }"
             (TyCtx.of_list [ ("hello", Type.(mono Int)) ]);
           test_solver "function application"
             "module Hello = {
                def identity = \\x : Int. x
                def hello = identity 1
              }"
             (TyCtx.of_list
                [
                  ("identity", Type.(mono (Arrow (Int, Int))));
                  ("hello", Type.(mono Int));
                ]);
           test_solver "infer function application"
             "module Hello = {
                def hello = (\\x \\y x) True 0
              }"
             (TyCtx.of_list [ ("hello", Type.(mono Bool)) ]);
           test_solver "nested function application"
             "module Hello = {
                def identity = \\x : Int. x
                def hello = identity (identity 1)
              }"
             (TyCtx.of_list
                [
                  ("identity", Type.(mono (Arrow (Int, Int))));
                  ("hello", Type.(mono Int));
                ]);
           test_solver "can solve annotated higher order function"
             "module Hello = {
                def hello : (Int -> Int) -> Int = \\f
                  f 1
              }"
             (TyCtx.of_list
                [ ("hello", Type.(mono (Arrow (Arrow (Int, Int), Type.Int)))) ]);
           test_solver "can solve un-annotated higher order function"
             "module Hello = {
                def hello = \\f
                  f 1
              }"
             (TyCtx.of_list
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
             TyCtx.(
               union BuiltIns.ty_ctx
                 (of_list
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
             TyCtx.(
               union BuiltIns.ty_ctx
                 (of_list [ ("main", Type.(mono (Arrow (Int, Int)))) ]));
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
             TyCtx.(
               union BuiltIns.ty_ctx
                 (of_list
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
             TyCtx.(
               union BuiltIns.ty_ctx
                 (of_list
                    [
                      ("is_even", Type.(mono (Arrow (Int, Bool))));
                      ("is_odd", Type.(mono (Arrow (Int, Bool))));
                    ]));
           test_solver "mutually recursive definitionns"
             "module Hello = {
                def ping = ping
                def pong = pong
              }"
             (TyCtx.of_list
                [
                  ("ping", Type.(Poly ([ "t0" ], Var "t0")));
                  ("pong", Type.(Poly ([ "t0" ], Var "t0")));
                ]);
           (* Failures *)
           test_failure "let expression out-of-scope"
             "module Hello = { def foo = let x = 1 in x def main = x }"
             [ [ Text "Unbound variable: x" ] ];
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
         ]
end

module Poly (S : Solver.S) = struct
  open Tester (S)

  let suite =
    "Poly Solver"
    >::: [
           (* Success *)
           (* TODO: polymorphic functions *)
           test_solver "infer top-level polymorphic function"
             "module Hello = {
              def identity = \\x x
            }"
             (TyCtx.of_list
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
             (TyCtx.of_list
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
             (TyCtx.of_list
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
             (TyCtx.of_list
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
