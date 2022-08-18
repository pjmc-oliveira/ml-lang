type kind =
  | KType
  | KVar of string
  | KArrow of kind * kind
[@@deriving show { with_path = false }]

type mono =
  | Int
  | Bool
  | Con of string
  | App of mono * mono
  | Var of string
  | Arrow of mono * mono
[@@deriving show { with_path = false }]

type poly = Poly of string list * mono [@@deriving show { with_path = false }]

let rec pretty_kind = function
  | KType -> "Type"
  | KVar name -> name
  | KArrow ((KArrow _ as param), body) ->
      "(" ^ pretty_kind param ^ ") -> " ^ pretty_kind body
  | KArrow (param, body) -> pretty_kind param ^ " -> " ^ pretty_kind body

let rec pretty_mono = function
  | Int -> "Int"
  | Bool -> "Bool"
  | Con name | Var name -> name
  | App (func, (App _ as arg)) ->
      pretty_mono func ^ " (" ^ pretty_mono arg ^ ")"
  | App (func, arg) -> pretty_mono func ^ " " ^ pretty_mono arg
  | Arrow ((Arrow _ as inp), out) ->
      "(" ^ pretty_mono inp ^ ") -> " ^ pretty_mono out
  | Arrow (inp, out) -> pretty_mono inp ^ " -> " ^ pretty_mono out

let pretty_poly (Poly (tvars, ty)) =
  let quantifier =
    if tvars = [] then "" else "forall " ^ String.concat " " tvars ^ ". "
  in
  quantifier ^ pretty_mono ty

let get_mono_type = function
  | Poly (_, ty) -> ty

let mono ty = Poly ([], ty)

let rec get_con = function
  | Int -> Some "Int"
  | Bool -> Some "Bool"
  | Con name -> Some name
  | Var _ -> None
  | App (func, _) -> get_con func
  | Arrow (_, _) -> None

(** Gets the arity of a type *)
let rec get_arity = function
  | Int | Bool | Con _ | Var _ | App _ -> 0
  | Arrow (_, to_) -> 1 + get_arity to_

(* Splits the components of a type-arrow *)
let rec split_arrow = function
  | Int | Bool | Con _ | Var _ | App _ -> []
  | Arrow (from, to_) -> from :: split_arrow to_

(** Gets the last type of a type arrow *)
let rec final_type ty =
  match ty with
  | Int | Bool | Con _ | Var _ | App _ -> ty
  | Arrow (_, to_) -> final_type to_