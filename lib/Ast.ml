type expr = Const of int | Var of string [@@deriving show]
type binding = Def of { name : string; expr : expr } [@@deriving show]

type module_ = Module of { name : string; bindings : binding list }
[@@deriving show]
