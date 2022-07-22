type t = Int | Bool | Var of string | Arrow of { from : t; to_ : t }
[@@deriving show]
