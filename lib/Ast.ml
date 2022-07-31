type ty = TCon of string | TVar of string | TArr of ty * ty [@@deriving show]
type scheme = TMono of ty | TForall of string list * ty [@@deriving show]
type lit = Int of int | Bool of bool [@@deriving show]
type pat = PCon of string * string list [@@deriving show]

type expr =
  | ELit of lit
  | EVar of string
  | ELet of string * ty option * expr * expr
  | EIf of expr * expr * expr
  | ELam of string * ty option * expr
  | EApp of expr * expr
  | EAnn of expr * ty
  | EMatch of expr * (pat * expr) list
[@@deriving show]

type alt = string * ty list [@@deriving show]

type bind = Def of scheme option * string * expr | Type of string * alt list
[@@deriving show]

type modu = Module of string * bind list [@@deriving show]
