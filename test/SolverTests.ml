open OUnit2
open Ml_lang
open Result.Syntax
module TyCtx = Solver.TyCtx

let solve_module str ctx =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  let* ctx = Solver.solve_module m ctx in
  Ok ctx

let string_of_result r =
  match r with
  | Ok ctx ->
      "Ok ["
      ^ String.concat "; "
          (List.map
             (fun (name, ty) -> "( \"" ^ name ^ "\", " ^ Type.show ty ^ " )")
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
             (fun (name, ty) -> "( \"" ^ name ^ "\", " ^ Type.show ty ^ " )")
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

let test_solver label str ?(initial_ctx = TyCtx.empty) expected_ctx =
  label >:: fun _ ->
  assert_equal ~printer:string_of_result ~cmp:ty_ctx_equal (Ok expected_ctx)
    (solve_module str initial_ctx)

let test_failure label str ?(initial_ctx = TyCtx.empty) expected_lines =
  label >:: fun _ ->
  let result = solve_module str initial_ctx in
  assert_equal ~printer:string_of_result_lines ~cmp:ty_ctx_equal
    (Error expected_lines)
    (Result.map_error errors_to_lines result)

let suite =
  "Solver"
  >::: [
         (* Successes *)
         test_solver "empty module" "module Hello = {}" TyCtx.empty;
         test_solver "one binding" "module Hello = { def hello = 1 }"
           (TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "two bindings"
           "module Hello = { def hello = 1 def bye = hello }"
           (TyCtx.of_list [ ("hello", Type.Int); ("bye", Type.Int) ]);
         test_solver "boolean literals"
           "module Hello = { def hello = True def bye = False }"
           (TyCtx.of_list [ ("hello", Type.Bool); ("bye", Type.Bool) ]);
         test_solver "top-level define before use"
           "module Hello = { def hello = bye def bye = 1 }"
           (TyCtx.of_list [ ("hello", Type.Int); ("bye", Type.Int) ]);
         test_solver "let expression"
           "module Hello = { def hello = let x = 1 in x }"
           (TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "if expression True"
           "module Hello = { def hello = if True then 1 else 2 }"
           (TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "if expression False"
           "module Hello = { def hello = if False then False else True }"
           (TyCtx.of_list [ ("hello", Type.Bool) ]);
         test_solver "lambda expression"
           "module Hello = { def hello = \\x : Int. True }"
           (TyCtx.of_list
              [ ("hello", Type.Arrow { from = Type.Int; to_ = Type.Bool }) ]);
         test_solver "lambda expression annotated as a whole"
           "module Hello = { def hello = (\\x x) : Int -> Int }"
           (TyCtx.of_list
              [ ("hello", Type.Arrow { from = Type.Int; to_ = Type.Int }) ]);
         test_solver "top level function annotation"
           "module Hello = { def hello : Int -> Bool  = \\x False }"
           (TyCtx.of_list
              [ ("hello", Type.Arrow { from = Type.Int; to_ = Type.Bool }) ]);
         test_solver "top level recursive value with annotation"
           "module Hello = { def hello : Int  = hello }"
           (TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "annotatated expression"
           "module Hello = { def hello = 1 : Int }"
           (TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "function application"
           "module Hello = {
              def identity = \\x : Int. x
              def hello = identity 1
            }"
           (TyCtx.of_list
              [
                ("identity", Type.Arrow { from = Type.Int; to_ = Type.Int });
                ("hello", Type.Int);
              ]);
         test_solver "nested function application"
           "module Hello = {
              def identity = \\x : Int. x
              def hello = identity (identity 1)
            }"
           (TyCtx.of_list
              [
                ("identity", Type.Arrow { from = Type.Int; to_ = Type.Int });
                ("hello", Type.Int);
              ]);
         test_solver "can solve higher order function"
           "module Hello = {
              def hello : (Int -> Int) -> Int = \\f
                f 1
            }"
           (TyCtx.of_list
              [
                ( "hello",
                  Type.Arrow
                    {
                      from = Type.Arrow { from = Type.Int; to_ = Type.Int };
                      to_ = Type.Int;
                    } );
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
                    ( "my_add",
                      Type.Arrow
                        {
                          from = Type.Int;
                          to_ = Type.Arrow { from = Type.Int; to_ = Type.Int };
                        } );
                    ("main", Type.Int);
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
               (of_list
                  [ ("main", Type.Arrow { from = Type.Int; to_ = Type.Int }) ]));
         (* Failures *)
         test_failure "let expression out-of-scope"
           "module Hello = { def foo = let x = 1 in x def main = x }"
           [ [ Text "Unbound variable: x" ] ];
         test_failure "if expression condition-not-bool"
           "module Hello = { def main = if 1 then 1 else 2 }"
           [
             [
               Text ("Expected if-condition to be type: " ^ Type.show Bool);
               Text ("But got type: " ^ Type.show Int);
             ];
           ];
         test_failure "if expression branch-mismatch"
           "module Hello = { def main = if True then 1 else False }"
           [
             [
               Text "If branches must have the same type";
               Text ("then-branch has type: " ^ Type.show Int);
               Text ("but else-branch has type: " ^ Type.show Bool);
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
               Text ("Expected: " ^ Type.show Int);
               Text ("But got: " ^ Type.show Bool);
             ];
           ];
         test_failure "wrong annotatated expression"
           "module Hello = { def hello = 1 : Bool }"
           [
             [
               Text "Type mismatch";
               Text ("Expected: " ^ Type.show Bool);
               Text ("But got: " ^ Type.show Int);
             ];
           ];
       ]
