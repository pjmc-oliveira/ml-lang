type mono = Int | Bool | Var of string | Arrow of mono * mono
[@@deriving show]

type poly = Mono of mono | Poly of string list * mono [@@deriving show]

let get_mono_type = function
  | Mono ty -> ty
  | Poly (_, ty) -> ty
