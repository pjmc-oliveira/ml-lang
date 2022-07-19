type t =
  (* Keywords *)
  | Module
  | Def
  | Let
  | In
  (* Symbols *)
  | Equal
  | LeftBrace
  | RightBrace
  (* Literals *)
  | Ident of string
  | Int of int
  | Bool of bool
[@@deriving show]

let to_string = function
  | Module -> "module"
  | Def -> "def"
  | Let -> "let"
  | In -> "in"
  | Equal -> "="
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Ident s -> s
  | Int n -> string_of_int n
  | Bool true -> "True"
  | Bool false -> "False"