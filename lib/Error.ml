module Line = struct
  type t = Text of string | Code of string

  let to_string = function Text str -> str | Code code -> "|   " ^ code
end

module Kind = struct
  type t = Lexer | Parser [@@deriving show]

  let to_string = show
end

type line = Line.t
type kind = Kind.t
type t = { kind : kind; lines : line list }

let to_string { kind; lines } =
  let lines = List.map Line.to_string lines in
  let lines = String.concat "\n" lines in
  let kind = Kind.to_string kind ^ " error" in
  kind ^ "\n" ^ lines