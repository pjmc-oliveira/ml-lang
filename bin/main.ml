open Ml_lang
open Result.Syntax

let run str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  Ok m

let report res =
  match res with
  | Error errs ->
      let lines = List.map Error.to_string errs in
      let lines = String.concat "\n\n" lines in
      "Failure:\n" ^ lines
  | Ok m -> "Success:\n" ^ Cst.Module.show m

let source = "\nmodule Hello = {\n  def hello = 1\n  def bye = 2\n}\n"

(* TODO *)
(* let source = "\nmodule Hello = {\n  def = 1 def = 1\n  def bye = 2\n}\n" *)

(* let source = "\nmodule Hello = {}\n" *)
let () = print_string (report (run source))