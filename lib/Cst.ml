module Expr = struct
  type t =
    | Const of { value : int; span : Source.Span.t }
    | Var of { value : string; span : Source.Span.t }
  [@@deriving show]
end

module Binding = struct
  type t = Def of { name : string; expr : Expr.t; span : Source.Span.t }
  [@@deriving show]
end

module Module = struct
  type t =
    | Module of {
        name : string;
        bindings : Binding.t list;
        span : Source.Span.t;
      }
  [@@deriving show]
end

type expr = Expr.t
type binding = Binding.t
type module_ = Module.t
