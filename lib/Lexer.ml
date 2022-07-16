let fail location lines : ('a, Error.t) result =
  Error { kind = Lexer; lines; location }

module StrMap = Map.Make (String)

let keywords =
  [ ("module", Token.Module); ("def", Def) ] |> List.to_seq |> StrMap.of_seq

let token src : (Token.t * Source.t, Error.t) result =
  match Source.next src with
  | None -> Ok (Eof, src)
  | Some (c, src') -> (
      match c with
      | '=' -> Ok (Equal, src')
      | '{' -> Ok (LeftBrace, src')
      | '}' -> Ok (RightBrace, src')
      | _ when Char.is_alpha c -> (
          let name, src' = Source.take_while Char.is_alphanum src in
          match StrMap.find_opt name keywords with
          | None -> Ok (Ident name, src')
          | Some kw -> Ok (kw, src'))
      | _ when Char.is_digit c ->
          let digits, src'' = Source.take_while Char.is_alphanum src in
          if String.exists Char.is_alpha digits then
            let start = Source.position src in
            let stop = Source.position src'' in
            let location = Loc.between start stop in
            fail (Some start)
              [
                Text "Invalid number:";
                Code (digits, location);
                Text "Numbers cannot contain letters";
              ]
          else
            let number = int_of_string digits in
            Ok (Int number, src'')
      | _ ->
          let str = String.make 1 c in
          let start = Source.position src in
          let stop = Source.position src' in
          let location = Loc.between start stop in
          fail (Some start)
            [ Text "Unexpected character: "; Code (str, location) ])

let space src : Source.t = Source.drop_while Char.is_space src

let tokens src : (Token.t list, Error.t list) result =
  let rec loop tks errs s =
    if Source.is_done s then
      if errs = [] then
        Ok (List.rev (Token.Eof :: tks))
      else
        Error (List.rev errs)
    else
      match token s with
      | Ok (tk, s') -> loop (tk :: tks) errs (space s')
      | Error err -> loop tks (err :: errs) (Source.drop s)
  in
  loop [] [] (space src)