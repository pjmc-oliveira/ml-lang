type ty = TCon of string | TVar of string | TArr of ty * ty [@@deriving show]
type scheme = TMono of ty | TForall of string list * ty [@@deriving show]
type lit = Int of int | Bool of bool [@@deriving show]

type expr =
  | ELit of lit
  | EVar of string
  | ELet of ty option * string * expr * expr
  | EIf of expr * expr * expr
  | ELam of ty option * string * expr
  | EApp of expr * expr
  | EAnn of expr * ty
[@@deriving show]

type bind = Def of scheme option * string * expr [@@deriving show]
type modu = Module of string * bind list [@@deriving show]
