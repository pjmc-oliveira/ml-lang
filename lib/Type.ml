type t =
  | Int
  | Bool
  | Var of string
  | Arrow of { from : t; to_ : t }
  | Forall of { ty_vars : string list; type_ : t }
[@@deriving show]
