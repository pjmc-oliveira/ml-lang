open Ml_lang

(* open Result.Syntax *)

let run str =
  let src = Source.of_string str in
  Lexer.tokens src

let report res =
  match res with
  | Error errs ->
      let lines = List.map Error.to_string errs in
      let lines = String.concat "\n\n" lines in
      "Error:\n" ^ lines
  | Ok tks ->
      let lines = List.map Token.to_string tks in
      let lines = String.concat "\n" lines in
      "Ok:\n" ^ lines

let () = print_string (report (run "1 module def defi hello {}"))
(* let () = prerr_string "Hello, World!" *)