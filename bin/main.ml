open Ml_lang
open Extensions

module W = Writer_option.Make (struct
  type t = Error.t list

  let empty = []
  let concat = ( @ )
end)

open W.Syntax

(* TODO: make this lazy? *)
let read_lines path : string =
  let ic = open_in path in
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

let string_of_errors src errs =
  let lines = List.map (Error.to_string src) errs in
  let lines = String.concat "\n\n" lines in
  lines

let lex str =
  let src = Source.of_string str in
  Lexer.tokens src

let parse tks = Parser.(parse module_ tks)
let solve cst = Solver.(module_ cst Built_ins.ty_ctx)
let interpret tast = Interpreter.run ~context:Built_ins.tm_ctx tast

let run str =
  let tast =
    let* tks = lex str in
    let* cst = parse tks in
    let* tcst = solve cst in
    let tast = Desugar.module_ tcst in
    W.pure tast
  in
  match tast with
  | Some tast, [] -> interpret tast
  | _, errs -> Error errs

let report res =
  match res with
  | Error errs -> Ansi.pretty ~color:Red ~font:Bold "Failure:\n" ^ errs
  | Ok value ->
      Ansi.pretty ~color:Green ~font:Bold "Success:\n" ^ Value.show value

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [ _script; path ] ->
      let src = read_lines path in
      let res =
        Result.map_error (string_of_errors (Source.of_string src)) (run src)
      in
      print_endline (report res)
  | _ -> print_endline "Usage: dune exec ml_lang <path>"
