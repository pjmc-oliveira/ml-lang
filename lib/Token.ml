type t =
  (* Keywords *)
  | Module
  | Def
  | Let
  | In
  | If
  | Then
  | Else
  | Match
  | With
  | End
  | Forall
  | Type
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
  | Pipe
  (* Literals *)
  | LowerIdent of string
  | UpperIdent of string
  | Int of int
  | Bool of bool
[@@deriving show { with_path = false }]

let to_string = function
  (* Keywords *)
  | Module -> "module"
  | Def -> "def"
  | Let -> "let"
  | In -> "in"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Match -> "match"
  | With -> "with"
  | End -> "end"
  | Forall -> "forall"
  | Type -> "type"
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
  | Pipe -> "|"
  (* Literals *)
  | LowerIdent s -> s
  | UpperIdent s -> s
  | Int n -> string_of_int n
  | Bool true -> "True"
  | Bool false -> "False"