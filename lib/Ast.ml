module Expr = struct
  type t = Const of { value : int } | Var of { name : string }
  [@@deriving show]
end

module Binding = struct
  type t = Def of { name : string; expr : Expr.t } [@@deriving show]
end

module Module = struct
  type t = Module of { name : string; bindings : Binding.t list }
  [@@deriving show]
end

type expr = Expr.t
type binding = Binding.t
type module_ = Module.t
