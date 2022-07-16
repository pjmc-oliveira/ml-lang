type t =
  | Module
  | Ident of string
  | Equal
  | LeftBrace
  | RightBrace
  | Def
  | Int of int
  | Eof
[@@deriving show]

let to_string = show