type kind = KType [@@deriving show]

type mono = Int | Bool | Con of string | Var of string | Arrow of mono * mono
[@@deriving show]

type poly = Poly of string list * mono [@@deriving show]

let get_mono_type = function
  | Poly (_, ty) -> ty

let mono ty = Poly ([], ty)

(** Gets the arity of a type *)
let rec get_arity = function
  | Int | Bool | Con _ | Var _ -> 0
  | Arrow (_, to_) -> 1 + get_arity to_

(* Splits the compoenets of a type-arrow *)
let rec split_arrow = function
  | Int | Bool | Con _ | Var _ -> []
  | Arrow (from, to_) -> from :: split_arrow to_

(** Gets the last type of a type arrow *)
let rec final_type ty =
  match ty with
  | Int | Bool | Con _ | Var _ -> ty
  | Arrow (_, to_) -> final_type to_