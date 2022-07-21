open OUnit2
open Ml_lang
open Result.Syntax
module TyCtx = Solver.TyCtx
module TmCtx = Interpreter.TmCtx

let interpret_module ?(entrypoint = "main") ?(tm_ctx = TmCtx.empty)
    ?(ty_ctx = TmCtx.empty) str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  let* m = Solver.module_ m ty_ctx in
  let* value = Interpreter.run ~entrypoint ~context:tm_ctx m in
  Ok value

let string_of_result r =
  match r with
  | Ok value -> "Ok " ^ Value.show value
  | Error e -> "Error [" ^ String.concat "\n" (List.map Error.to_string e) ^ "]"

let error_to_lines (e : Error.t) : Error.Line.t list = e.lines
let errors_to_lines es = List.(map error_to_lines es)

let string_of_result_lines (r : (Value.t, Error.Line.t list list) result) =
  match r with
  | Ok value -> "Ok " ^ Value.show value
  | Error e ->
      let lines = List.map (List.map Error.Line.show) e in
      let lines = List.map (String.concat "; ") lines in
      "Error [" ^ String.concat "\n" lines ^ "]"

let test_interpreter label str ?(entrypoint = "main") ?(tm_ctx = TmCtx.empty)
    ?(ty_ctx = TmCtx.empty) expected =
  label >:: fun _ ->
  assert_equal ~printer:string_of_result (Ok expected)
    (interpret_module ~entrypoint ~tm_ctx ~ty_ctx str)

let test_failure label str ?(entrypoint = "main") ?(tm_ctx = TmCtx.empty)
    ?(ty_ctx = TmCtx.empty) (expected : Error.Line.t list list) =
  label >:: fun _ ->
  let result = interpret_module ~entrypoint ~tm_ctx ~ty_ctx str in
  assert_equal ~printer:string_of_result_lines (Error expected)
    (Result.map_error errors_to_lines result)

let suite =
  "Interpreter"
  >::: [
         (* Successes *)
         test_interpreter "one binding" "module Hello = { def main = 1 }"
           (Int 1);
         test_interpreter "boolean literal True"
           "module Hello = { def main = True }" (Bool true);
         test_interpreter "boolean literal False"
           "module Hello = { def main = False }" (Bool false);
         test_interpreter "two bindings"
           "module Hello = { def hello = 1 def main = hello }" (Int 1);
         test_interpreter "top-level define before use"
           "module Hello = { def main = bye def bye = 1 }" (Int 1);
         test_interpreter "let expression"
           "module Hello = { def main = let x = 2 in x }" (Int 2);
         test_interpreter "if expression True"
           "module Hello = { def main = if True then let x = 1 in x else 2 }"
           (Int 1);
         test_interpreter "if expression False"
           "module Hello = { def main = if False then 1 else 2 }" (Int 2);
         test_interpreter "function application"
           "module Hello = {
              def identity = \\x : Int. x
              def main = identity 1
            }"
           (Int 1);
         test_interpreter "annotated expression"
           "module Hello = {
              def main = True : Bool
            }"
           (Bool true);
         test_interpreter "nested lambda"
           "module Hello = {
              def const = \\x : Int. \\y : Int.
                x
              def main = const 1 2
            }"
           (Int 1);
         test_interpreter "nested function application"
           "module Hello = {
              def identity = \\x : Int. x
              def main = identity (identity 1)
            }"
           (Int 1);
         test_interpreter "lambda expression annotated as a whole"
           "module Hello = {
              def identity = (\\x x) : Int -> Int
              def main = (identity 2)
            }"
           (Int 2);
         test_interpreter "different nested function application"
           "module Hello = {
              def id1 = \\x : Int. x
              def id2 = \\y : Int. y
              def main = id1 (id2 1)
            }"
           (Int 1);
         test_interpreter ~tm_ctx:BuiltIns.tm_ctx ~ty_ctx:BuiltIns.ty_ctx
           "higher order function"
           "module Hello = {
              def hello : (Int -> Int) -> Int = \\f
                f 1
              def identity : Int -> Int = \\x add x 1
              def main = hello identity
            }"
           (Int 2);
         test_interpreter ~tm_ctx:BuiltIns.tm_ctx ~ty_ctx:BuiltIns.ty_ctx
           "built-in functions"
           "module Hello = {
              def my_add = \\x : Int. \\y : Int.
                add x y

              def main = add 1 2
            }"
           (Int 3);
         test_interpreter ~tm_ctx:BuiltIns.tm_ctx ~ty_ctx:BuiltIns.ty_ctx
           "recursive function"
           "module Hello = {
               def fact : Int -> Int = \\x
                 if eq 0 x then
                   1
                 else
                   mul x (fact (sub x 1))

               def main = fact 5
             }"
           (Int 120);
         (* Failure *)
         test_failure "local scope"
           "module Hello = { def foo = let x = 1 in x def main = x }"
           [ [ Text "Unbound variable: x" ] ];
       ]