module Type = struct
  type t = Const of { name : string } | Arrow of { from : t; to_ : t }
  [@@deriving show]
end

module Expr = struct
  type t =
    | Int of { value : int }
    | Bool of { value : bool }
    | Var of { name : string }
    | Let of { name : string; def : t; body : t }
    | If of { cond : t; con : t; alt : t }
    | Lam of { param : string; param_t : Type.t option; body : t }
    | App of { func : t; arg : t }
    | Ann of { expr : t; ann : Type.t }
  [@@deriving show]
end

module Binding = struct
  type t = Def of { name : string; ann : Type.t option; expr : Expr.t }
  [@@deriving show]
end

module Module = struct
  type t = Module of { name : string; bindings : Binding.t list }
  [@@deriving show]
end

type type_ = Type.t
type expr = Expr.t
type binding = Binding.t
type module_ = Module.t
