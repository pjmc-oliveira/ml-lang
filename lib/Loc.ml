type t = Pos of int | Span of { start : int; length : int } [@@deriving show]

let between start stop =
  let start = match start with Pos start | Span { start; _ } -> start in
  let stop =
    match stop with
    | Pos stop -> stop
    | Span { start; length } -> start + length
  in
  Span { start; length = stop - start }

let start = function Pos start | Span { start; _ } -> start
