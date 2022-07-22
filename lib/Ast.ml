type ty = Const of { name : string } | Arrow of { from : ty; to_ : ty }
[@@deriving show]

type expr =
  | Int of { value : int }
  | Bool of { value : bool }
  | Var of { name : string }
  | Let of { name : string; def_t : ty option; def : expr; body : expr }
  | If of { cond : expr; con : expr; alt : expr }
  | Lam of { param : string; param_t : ty option; body : expr }
  | App of { func : expr; arg : expr }
  | Ann of { expr : expr; ann : ty }
[@@deriving show]

type binding = Def of { name : string; ann : ty option; expr : expr }
[@@deriving show]

type module_ = Module of { name : string; bindings : binding list }
[@@deriving show]
