module Type = struct
  type t =
    | Const of { name : string; span : Source.Span.t }
    | Arrow of { from : t; to_ : t; span : Source.Span.t }
  [@@deriving show]

  let rec to_ast e : Ast.Type.t =
    match e with
    | Const { name; _ } -> Const { name }
    | Arrow { from; to_; _ } ->
        let from = to_ast from in
        let to_ = to_ast to_ in
        Arrow { from; to_ }
end

module Expr = struct
  type t =
    | Int of { value : int; span : Source.Span.t }
    | Bool of { value : bool; span : Source.Span.t }
    | Var of { name : string; span : Source.Span.t }
    | Let of { name : string; def : t; body : t; span : Source.Span.t }
    | If of { cond : t; con : t; alt : t; span : Source.Span.t }
    | Lam of {
        param : string;
        param_t : Type.t option;
        body : t;
        span : Source.Span.t;
      }
    | App of { func : t; arg : t; span : Source.Span.t }
    | Ann of { expr : t; ann : Type.t; span : Source.Span.t }
  [@@deriving show]

  let rec to_ast e : Ast.Expr.t =
    match e with
    | Int { value; _ } -> Int { value }
    | Bool { value; _ } -> Bool { value }
    | Var { name; _ } -> Var { name }
    | Let { name; def; body; _ } ->
        let def = to_ast def in
        let body = to_ast body in
        Let { name; def; body }
    | If { cond; con; alt; _ } ->
        let cond = to_ast cond in
        let con = to_ast con in
        let alt = to_ast alt in
        If { cond; con; alt }
    | Lam { param; param_t; body; _ } ->
        let param_t = Option.map Type.to_ast param_t in
        let body = to_ast body in
        Lam { param; param_t; body }
    | App { func; arg; _ } ->
        let func = to_ast func in
        let arg = to_ast arg in
        App { func; arg }
    | Ann { expr; ann; _ } ->
        let ann = Type.to_ast ann in
        let expr = to_ast expr in
        Ann { expr; ann }

  let map_span f = function
    | Int { value; span } -> Int { value; span = f span }
    | Bool { value; span } -> Bool { value; span = f span }
    | Var { name; span } -> Var { name; span = f span }
    | Let { name; def; body; span } -> Let { name; def; body; span = f span }
    | If { cond; con; alt; span } -> If { cond; con; alt; span = f span }
    | Lam { param; param_t; body; span } ->
        Lam { param; param_t; body; span = f span }
    | App { func; arg; span } -> App { func; arg; span = f span }
    | Ann { expr; ann; span } -> Ann { expr; ann; span = f span }
end

module Binding = struct
  type t = Def of { name : string; expr : Expr.t; span : Source.Span.t }
  [@@deriving show]

  let to_ast b : Ast.Binding.t =
    match b with
    | Def { name; expr; _ } ->
        let expr = Expr.to_ast expr in
        Def { name; expr }
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
        Module { name; bindings }
end

type type_ = Type.t
type expr = Expr.t
type binding = Binding.t
type module_ = Module.t
