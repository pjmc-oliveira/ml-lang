module Pos = struct
  type t = {
    index : int;
    line : int;
    column : int;
  }
  [@@deriving show { with_path = false }]

  let empty = { index = 0; line = 1; column = 1 }
  let inc_line p = { index = p.index + 1; line = p.line + 1; column = 1 }

  let inc_column p =
    { index = p.index + 1; line = p.line; column = p.column + 1 }

  let step c p = if c = '\n' then inc_line p else inc_column p
  let step_many s p = String.fold_left (fun p c -> step c p) p s
end

module Span = struct
  type t = {
    index : int;
    line : int;
    column : int;
    length : int;
  }
  [@@deriving show { with_path = false }]

  let from ({ index; line; column } : Pos.t) ~(to_ : Pos.t) =
    { index; line; column; length = to_.index - index }

  let between s1 s2 =
    let length = s2.index + s2.length - s1.index in
    { s1 with length }

  let to_string s =
    "ln " ^ string_of_int s.line ^ ", col " ^ string_of_int s.column
end

type t = {
  text : string;
  position : Pos.t;
}
[@@deriving show { with_path = false }]

type pos = Pos.t
type span = Span.t

let of_string text = { text; position = Pos.empty }
let is_done src = String.length src.text <= src.position.index
let position src : Pos.t = src.position

let between src1 src2 =
  let p1 = position src1 in
  let p2 = position src2 in
  Span.from p1 ~to_:p2

let next src =
  if is_done src then
    None
  else
    let ch = src.text.[src.position.index] in
    let position = Pos.step ch src.position in
    Some (ch, { src with position })

let drop_prefix prefix src : t option =
  let index = src.position.index in
  let length = String.length src.text - index in
  let text = String.sub src.text index length in
  if String.starts_with ~prefix text then
    let position = Pos.step_many prefix src.position in
    Some { src with position }
  else
    None

let drop_while predicate src : t =
  let start = src.position.index in
  let size = String.length src.text in
  let rec loop i =
    if i < size && predicate src.text.[i] then
      loop (i + 1)
    else
      i
  in
  let stop = loop start in
  let str = String.sub src.text start (stop - start) in
  let position = Pos.step_many str src.position in
  { src with position }

let take_while predicate src : string * t =
  let start = src.position.index in
  let src' = drop_while predicate src in
  let stop = src'.position.index in
  let str = String.sub src.text start (stop - start) in
  (str, src')

let drop src : t =
  match next src with
  | None -> src
  | Some (_, src') -> src'
