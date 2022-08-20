type t = {
  text : string;
  position : Span.Pos.t;
}
[@@deriving show { with_path = false }]

let of_string text = { text; position = Span.Pos.empty }
let is_done src = String.length src.text <= src.position.index
let position src : Span.Pos.t = src.position

let between src1 src2 =
  let p1 = position src1 in
  let p2 = position src2 in
  Span.from p1 ~to_:p2

let next src =
  if is_done src then
    None
  else
    let ch = src.text.[src.position.index] in
    let position = Span.Pos.step ch src.position in
    Some (ch, { src with position })

let drop_prefix prefix src : t option =
  let index = src.position.index in
  let length = String.length src.text - index in
  let text = String.sub src.text index length in
  if String.starts_with ~prefix text then
    let position = Span.Pos.step_many prefix src.position in
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
  let position = Span.Pos.step_many str src.position in
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
