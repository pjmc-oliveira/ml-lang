module Expr = struct
  type t =
    | Const of { value : int; span : Source.Span.t }
    | Var of { name : string; span : Source.Span.t }
    | Let of { name : string; def : t; body : t; span : Source.Span.t }
  [@@deriving show]

  let rec to_ast e : Ast.Expr.t =
    match e with
    | Const { value; _ } -> Ast.Expr.Const { value }
    | Var { name; _ } -> Ast.Expr.Var { name }
    | Let { name; def; body; _ } ->
        let def = to_ast def in
        let body = to_ast body in
        Ast.Expr.Let { name; def; body }
end

module Binding = struct
  type t = Def of { name : string; expr : Expr.t; span : Source.Span.t }
  [@@deriving show]

  let to_ast b : Ast.Binding.t =
    match b with
    | Def { name; expr; _ } ->
        let expr = Expr.to_ast expr in
        Ast.Binding.Def { name; expr }
end

module Module = struct
  type t =
    | Module of {
        name : string;
        bindings : Binding.t list;
        span : Source.Span.t;
      }
  [@@deriving show]

  let to_ast m : Ast.Module.t =
    match m with
    | Module { name; bindings; _ } ->
        let bindings = List.map Binding.to_ast bindings in
        Ast.Module.Module { name; bindings }
end

type expr = Expr.t
type binding = Binding.t
type module_ = Module.t
