open Extensions

let fail ?location lines : Error.t = { kind = Lexer; lines; location }

module StrMap = Map.Make (String)

let keywords =
  [
    ("module", Token.Module);
    ("def", Def);
    ("let", Let);
    ("in", In);
    ("if", If);
    ("then", Then);
    ("else", Else);
    ("forall", Forall);
    ("match", Match);
    ("with", With);
    ("end", End);
    ("type", Type);
    ("True", Bool true);
    ("False", Bool false);
  ]
  |> List.to_seq
  |> StrMap.of_seq

let unexpected_char char loc =
  fail
    [ Text ("Unexpected character: '" ^ String.of_char char ^ "'"); Quote loc ]

(** Lexes a token *)
let token src : (Token.t * Source.span * Source.t, Error.t * Source.t) result =
  match Source.next src with
  | None -> Error (fail [ Text "Unexpected EOF" ], src)
  | Some (c, src') -> (
      let span = Source.between src src' in
      match c with
      | '=' -> Ok (Equal, span, src')
      | '{' -> Ok (LeftBrace, span, src')
      | '}' -> Ok (RightBrace, span, src')
      | '(' -> Ok (LeftParen, span, src')
      | ')' -> Ok (RightParen, span, src')
      | ':' -> Ok (Colon, span, src')
      | '\\' -> Ok (BackSlash, span, src')
      | '.' -> Ok (Dot, span, src')
      | '|' -> Ok (Pipe, span, src')
      (* TODO: Clean this up *)
      | '-' -> (
          match Source.next src' with
          | Some ('>', src'') ->
              let span = Source.between src src'' in
              Ok (Arrow, span, src'')
          | _ ->
              let location = Source.between src src' in
              Error (unexpected_char c location, src'))
      | _ when Char.is_alpha_upper c -> (
          let name, src'' =
            Source.take_while (fun c -> Char.is_alphanum c || c = '_') src
          in
          let span = Source.between src src'' in
          match StrMap.find_opt name keywords with
          | None -> Ok (UpperIdent name, span, src'')
          | Some kw -> Ok (kw, span, src''))
      | _ when Char.is_alpha_lower c -> (
          let name, src'' =
            Source.take_while (fun c -> Char.is_alphanum c || c = '_') src
          in
          let span = Source.between src src'' in
          match StrMap.find_opt name keywords with
          | None -> Ok (LowerIdent name, span, src'')
          | Some kw -> Ok (kw, span, src''))
      | _ when Char.is_digit c ->
          let digits, src'' = Source.take_while Char.is_alphanum src in
          if String.exists Char.is_alpha digits then
            let location = Source.between src src'' in
            Error
              ( fail
                  [
                    Text "Invalid number:";
                    Quote location;
                    Text "Numbers cannot contain letters";
                  ],
                src'' )
          else
            let number = int_of_string digits in
            let span = Source.between src src'' in
            Ok (Int number, span, src'')
      | _ ->
          let location = Source.between src src' in
          Error (unexpected_char c location, src'))

(** Comsume whitespace *)
let space src : Source.t = Source.drop_while Char.is_space src

(** Consume comments *)
let comment src =
  match Source.drop_prefix "--" src with
  | None -> None
  | Some src' -> Some (Source.drop_while (fun c -> not (c = '\n')) src')

(** Skips whitespace and comments *)
let rec skip src =
  let src' = space src in
  match comment src' with
  | None -> src'
  | Some src'' -> skip src''

(** Lexes all tokens *)
let tokens src : ((Token.t * Source.span) list, Error.t list) result =
  let rec loop tks errs s =
    if Source.is_done s then
      if errs = [] then
        Ok (List.rev tks)
      else
        Error (List.rev errs)
    else
      match token (skip s) with
      | Ok (tk, loc, s') -> loop ((tk, loc) :: tks) errs (skip s')
      | Error (err, s') -> loop tks (err :: errs) (Source.drop s')
  in
  loop [] [] (skip src)