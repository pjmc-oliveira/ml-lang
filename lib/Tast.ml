module Expr = struct
  type t =
    | Int of { value : int; span : Source.Span.t; type_ : Type.t }
    | Bool of { value : bool; span : Source.Span.t; type_ : Type.t }
    | Var of { name : string; span : Source.Span.t; type_ : Type.t }
    | Let of {
        name : string;
        ann : Type.t;
        def : t;
        body : t;
        span : Source.Span.t;
        type_ : Type.t;
      }
    | If of { cond : t; con : t; alt : t; span : Source.Span.t; type_ : Type.t }
  [@@deriving show]
end

module Binding = struct
  type t =
    | Def of {
        name : string;
        expr : Expr.t;
        span : Source.Span.t;
        type_ : Type.t;
      }
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