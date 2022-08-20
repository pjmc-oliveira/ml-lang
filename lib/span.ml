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
