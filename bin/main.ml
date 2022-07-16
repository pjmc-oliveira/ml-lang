open Ml_lang
open Result.Syntax

let run str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m, _tks' = Parser.parse Parser.module_ tks in
  Ok m

let report res =
  match res with
  | Error errs ->
      let lines = List.map Error.to_string errs in
      let lines = String.concat "\n\n" lines in
      "Error:\n" ^ lines
  | Ok m -> "Ok:\n" ^ Ast.show_module_ m

let source = "module Hello = { def hello = 1 def bye = 2 }"
let () = print_string (report (run source))
(* let () = prerr_string "Hello, World!" *)