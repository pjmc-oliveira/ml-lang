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

let to_string = function
  | Module -> "module"
  | Ident s -> s
  | Equal -> "="
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Def -> "def"
  | Int n -> string_of_int n
  | Eof -> "<eof>"