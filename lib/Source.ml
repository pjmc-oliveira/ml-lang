module Pos = struct
  type t = int [@@deriving show]
end

module Span = struct
  type t = { index : int; length : int } [@@deriving show]

  let from start ~to_:stop = { index = start; length = stop - start }

  let between s1 s2 =
    let index = s1.index in
    let length = s2.index + s2.length - index in
    { index; length }

  let start { index; _ } = index
end

type t = { text : string; index : Pos.t } [@@deriving show]
type pos = Pos.t
type span = Span.t

let of_string text = { text; index = 0 }
let is_done src = String.length src.text <= src.index
let position src : Pos.t = src.index

let between src1 src2 =
  let p1 = position src1 in
  let p2 = position src2 in
  Span.from p1 ~to_:p2

let next src =
  if src.index >= String.length src.text then
    None
  else
    let char = String.get src.text src.index in
    let index = src.index + 1 in
    Some (char, { src with index })

let drop_while predicate src : t =
  let start = src.index in
  let size = String.length src.text in
  let rec loop i =
    if i < size && predicate src.text.[i] then
      loop (i + 1)
    else
      i
  in
  let stop = loop start in
  { src with index = stop }

let take_while predicate src : string * t =
  let start = src.index in
  let src' = drop_while predicate src in
  let stop = src'.index in
  let str = String.sub src.text start (stop - start) in
  (str, src')

let drop src : t = { src with index = src.index + 1 }
