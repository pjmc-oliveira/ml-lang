open Ml_lang
open Result.Syntax

(* TODO: make this lazy? *)
let read_lines path : string =
  let ic = open_in (String.concat Filename.dir_sep path) in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop lines =
    match try_read () with
    | Some line -> loop (line :: lines)
    | None ->
        close_in ic;
        List.rev lines
  in
  let lines = loop [] in
  String.concat "\n" lines

let run str =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  let* m = Solver.(solve (module_ m) TyCtx.empty) in
  let* value = Interpreter.run m in
  Ok value

let report res =
  match res with
  | Error errs ->
      let lines = List.map Error.to_string errs in
      let lines = String.concat "\n\n" lines in
      "Failure:\n" ^ lines
  | Ok value -> "Success:\n" ^ Value.show value

let source = read_lines [ "examples"; "hello.luz" ]

(* TODO *)
(* let source = "\nmodule Hello = {\n  def = 1 def = 1\n  def bye = 2\n}\n" *)

(* let source = "\nmodule Hello = {}\n" *)
let () = print_string (report (run source))
(* let () = Sys.argv |> Array.to_list |> String.concat ", " |> print_string *)
