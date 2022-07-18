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
  let* m = Solver.(solve (module_ m) ty_ctx) in
  let* value = Interpreter.run ~entrypoint ~context:tm_ctx m in
  Ok value

let string_of_result r =
  match r with
  | Ok value -> "Ok " ^ Value.show value
  | Error e -> "Error [" ^ String.concat "\n" (List.map Error.to_string e) ^ "]"

let test_interpreter label str ?(entrypoint = "main") ?(tm_ctx = TmCtx.empty)
    ?(ty_ctx = TmCtx.empty) expected =
  label >:: fun _ ->
  assert_equal ~printer:string_of_result (Ok expected)
    (interpret_module ~entrypoint ~tm_ctx ~ty_ctx str)

let suite =
  "Interpreter"
  >::: [
         test_interpreter "one binding" "module Hello = { def main = 1 }"
           (Value.Int 1);
         test_interpreter "two bindings"
           "module Hello = { def hello = 1 def main = hello }" (Value.Int 1);
         test_interpreter "top-level define before use"
           "module Hello = { def main = bye def bye = 1 }" (Value.Int 1);
       ]