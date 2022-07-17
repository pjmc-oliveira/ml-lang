module Line = struct
  type t = Text of string | Code of string * Loc.t

  let to_string = function
    | Text str -> str
    | Code (code, loc) -> string_of_int (Loc.start loc) ^ " |   " ^ code
end

module Kind = struct
  type t = Lexer | Parser [@@deriving show]

  let to_string = function Lexer -> "Lexer" | Parser -> "Parser"
end

type line = Line.t
type kind = Kind.t
type t = { kind : kind; lines : line list; location : Loc.t option }

let to_string { kind; lines; location } =
  let lines = List.map (fun ln -> "  " ^ Line.to_string ln) lines in
  let lines = String.concat "\n" lines in
  let kind = Kind.to_string kind ^ " error" in
  let location =
    match location with
    | Some location -> "at " ^ string_of_int (Loc.start location)
    | None -> ""
  in
  String.concat "\n" [ kind; lines; location ]
