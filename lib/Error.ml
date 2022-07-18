module Line = struct
  type t = Text of string | Code of string * Source.Span.t [@@deriving show]

  let to_string = function
    | Text str -> str
    | Code (code, loc) -> Source.Span.to_string loc ^ " |   " ^ code
end

module Kind = struct
  type t = Lexer | Parser | Solver | Interpreter [@@deriving show]

  let to_string = function
    | Lexer -> "Lexer"
    | Parser -> "Parser"
    | Solver -> "Solver"
    | Interpreter -> "Interpreter"
end

type t = { kind : Kind.t; lines : Line.t list; location : Source.Span.t option }
[@@deriving show]

let to_string { kind; lines; location } =
  let lines = List.map (fun ln -> "  " ^ Line.to_string ln) lines in
  let lines = String.concat "\n" lines in
  let kind = Kind.to_string kind ^ " error" in
  let location =
    match location with
    | Some location -> "at " ^ Source.Span.to_string location
    | None -> ""
  in
  String.concat "\n" [ kind; lines; location ]
