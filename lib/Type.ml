type kind = KType [@@deriving show]

type mono = Int | Bool | Con of string | Var of string | Arrow of mono * mono
[@@deriving show]

type poly = Poly of string list * mono [@@deriving show]

let get_mono_type = function
  | Poly (_, ty) -> ty

let mono ty = Poly ([], ty)
