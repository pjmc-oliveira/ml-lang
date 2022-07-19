type t =
  (* Keywords *)
  | Module
  | Def
  | Let
  | In
  | If
  | Then
  | Else
  (* Symbols *)
  | Equal
  | LeftBrace
  | RightBrace
  | LeftParen
  | RightParen
  | Colon
  | BackSlash
  | Arrow
  | Dot
  (* Literals *)
  | Ident of string
  | Int of int
  | Bool of bool
[@@deriving show]

let to_string = function
  (* Keywords *)
  | Module -> "module"
  | Def -> "def"
  | Let -> "let"
  | In -> "in"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  (* Symbols *)
  | Equal -> "="
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | LeftParen -> "("
  | RightParen -> ")"
  | Colon -> ":"
  | BackSlash -> "\\"
  | Arrow -> "->"
  | Dot -> "."
  (* Literals *)
  | Ident s -> s
  | Int n -> string_of_int n
  | Bool true -> "True"
  | Bool false -> "False"