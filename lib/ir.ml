type lit =
  | Int of int
  | Bool of bool
[@@deriving show { with_path = false }]

type pat = PCon of string * string list
[@@deriving show { with_path = false }]

type expr =
  | Lit of Type.mono * lit
  | Var of Type.mono * string
  | Let of Type.mono * string * expr * expr
  | LetRec of Type.mono * string * expr * expr
  | If of Type.mono * expr * expr * expr
  (* Type of [param] to [body] *)
  | Lam of Type.mono * Type.mono * string * expr
  | App of Type.mono * expr * expr
  | Match of Type.mono * expr * (pat * expr) list
[@@deriving show { with_path = false }]

type ty_def =
  | TyDef of {
      name : string;
      kind : Type.kind;
      alts : (string * Type.poly) list;
    }
[@@deriving show { with_path = false }]

type tm_def =
  | TmDef of {
      name : string;
      scheme : Type.poly;
      expr : expr;
    }
[@@deriving show { with_path = false }]

type modu =
  | Module of {
      name : string;
      types : ty_def list;
      terms : tm_def list;
    }
[@@deriving show { with_path = false }]
