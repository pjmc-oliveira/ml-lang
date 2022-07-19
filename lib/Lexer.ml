let fail ?location lines : ('a, Error.t) result =
  Error { kind = Lexer; lines; location }

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
    ("True", Bool true);
    ("False", Bool false);
  ]
  |> List.to_seq |> StrMap.of_seq

let token src : (Token.t * Source.span * Source.t, Error.t) result =
  match Source.next src with
  | None -> fail [ Text "Unexpected EOF" ]
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
      (* TODO: Clean this up *)
      | '-' -> (
          match Source.next src' with
          | Some ('>', src'') ->
              let span = Source.between src src'' in
              Ok (Arrow, span, src'')
          | _ ->
              let str = String.make 1 c in
              let location = Source.between src src' in
              fail [ Text "Unexpected character: "; Code (str, location) ])
      | _ when Char.is_alpha c -> (
          let name, src'' =
            Source.take_while (fun c -> Char.is_alphanum c || c = '_') src
          in
          let span = Source.between src src'' in
          match StrMap.find_opt name keywords with
          | None -> Ok (Ident name, span, src'')
          | Some kw -> Ok (kw, span, src''))
      | _ when Char.is_digit c ->
          let digits, src'' = Source.take_while Char.is_alphanum src in
          if String.exists Char.is_alpha digits then
            let location = Source.between src src'' in
            fail
              [
                Text "Invalid number:";
                Code (digits, location);
                Text "Numbers cannot contain letters";
              ]
          else
            let number = int_of_string digits in
            let span = Source.between src src'' in
            Ok (Int number, span, src'')
      | _ ->
          let str = String.make 1 c in
          let location = Source.between src src' in
          fail [ Text "Unexpected character: "; Code (str, location) ])

let space src : Source.t = Source.drop_while Char.is_space src

let tokens src : ((Token.t * Source.span) list, Error.t list) result =
  let rec loop tks errs s =
    if Source.is_done s then
      if errs = [] then
        Ok (List.rev tks)
      else
        Error (List.rev errs)
    else
      match token s with
      | Ok (tk, loc, s') -> loop ((tk, loc) :: tks) errs (space s')
      | Error err -> loop tks (err :: errs) (Source.drop s)
  in
  loop [] [] (space src)