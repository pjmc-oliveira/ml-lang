let fail lines : ('a, Error.t) result = Error { kind = Lexer; lines }

let token src : (Token.t * Source.t, Error.t) result =
  match Source.next src with
  | None -> Ok (Eof, src)
  | Some (c, src') -> (
      match c with
      | '=' -> Ok (Equal, src')
      | '{' -> Ok (LeftBrace, src')
      | '}' -> Ok (RightBrace, src')
      | _ when Char.is_alpha c ->
          let name, src' = Source.take_while Char.is_alphanum src in
          Ok (Ident name, src')
      | _ when Char.is_digit c ->
          let digits, src'' = Source.take_while Char.is_alphanum src in
          if String.exists Char.is_alpha digits then
            fail
              [
                Text "Invalid number:";
                Code digits;
                Text "Numbers cannot contain letters";
              ]
          else
            let number = int_of_string digits in
            Ok (Int number, src'')
      | _ ->
          let str = String.make 1 c in
          fail [ Text "Unexpected character: "; Code str ])

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
  loop [] [] src