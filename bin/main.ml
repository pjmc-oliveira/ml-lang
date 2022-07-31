open Ml_lang
open Extensions
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

let string_of_errors errs =
  let lines = List.map Error.to_string errs in
  let lines = String.concat "\n\n" lines in
  lines

let lex str =
  let src = Source.of_string str in
  Lexer.tokens src

let parse tks = Parser.(parse module_ tks)
let solve cst = Solver.(module_ cst BuiltIns.ty_ctx)
let interpret tast = Interpreter.run ~context:BuiltIns.tm_ctx tast

let run str =
  let* tks = lex str in
  let* cst = parse tks in
  let* tast = solve cst in
  let* value = interpret tast in
  Ok value

let report res =
  match res with
  | Error errs -> "Failure:\n" ^ errs
  | Ok value -> "Success:\n" ^ Value.show value

let source = read_lines [ "examples"; "hello.luz" ]
let () = print_string (report (Result.map_error string_of_errors (run source)))
