open Extensions

module Line = struct
  type t =
    | Text of string
    | Quote of Span.t
  [@@deriving show { with_path = false }]

  (** Gets source lines of a span *)
  let quote_lines (src : Source.t) (span : Span.t) : string list =
    let lines = String.split_on_char '\n' src.text in

    let number_of_lines =
      List.length
        String.(split_on_char '\n' (sub src.text span.index span.length))
    in
    (* Span lines start at 1 *)
    List.slice (span.line - 1) (span.line + number_of_lines - 1) lines

  (** Highlights a span, returns highlighted lines + an optional underlined terminator *)
  let highlight lines (span : Span.t) pad_len =
    (* Span columns start at 1 and lines are padded by 'pad_len' spaces
       and an extra character *)
    if List.length lines = 1 then
      ( lines,
        "\n"
        ^ String.(
            make (span.column + (pad_len + 1) - 1) ' '
            ^ Ansi.pretty ~color:Red (make span.length '^')) )
    else
      (List.map (fun ln -> Ansi.pretty ~color:Red ">" ^ ln) lines, "")

  let to_string (src : Source.t) = function
    | Text str -> str
    | Quote loc ->
        let pad_len = 4 in
        let lines = quote_lines src loc in
        let lines, highlight = highlight lines loc pad_len in
        let line_numbers = List.range loc.line (loc.line + List.length lines) in
        let format_line (ln, str) =
          Ansi.pretty ~color:Cyan
            (String.left_pad pad_len (string_of_int ln) ^ "|")
          ^ str
        in
        let lines = List.(map format_line (zip line_numbers lines)) in
        String.concat "\n" lines ^ highlight
end

module Kind = struct
  type t =
    | Lexer
    | Parser
    | Solver
    | Interpreter
  [@@deriving show { with_path = false }]

  let to_string = function
    | Lexer -> "Lexer"
    | Parser -> "Parser"
    | Solver -> "Solver"
    | Interpreter -> "Interpreter"
end

type t = {
  kind : Kind.t;
  lines : Line.t list;
  location : Span.t option;
}
[@@deriving show { with_path = false }]

let to_string src { kind; lines; location } =
  let lines = List.map (Line.to_string src) lines in
  let lines = String.concat "\n" lines in
  let kind = Kind.to_string kind ^ " error" in
  let location =
    match location with
    | Some location -> "\nat " ^ Span.to_string location
    | None -> ""
  in
  String.concat "\n" [ kind; lines ] ^ location
