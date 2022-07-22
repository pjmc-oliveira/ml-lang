module Type = struct
  type 'a t =
    | Const of { name : string; span : 'a }
    | Var of { name : string; span : 'a }
    | Arrow of { from : 'a t; to_ : 'a t; span : 'a }
    | Forall of { ty_vars : string list; type_ : 'a t; span : 'a }
  [@@deriving show]

  let rec to_ast e : Ast.ty =
    match e with
    | Const { name; _ } -> Const { name }
    | Var { name; _ } -> Var { name }
    | Arrow { from; to_; _ } ->
        let from = to_ast from in
        let to_ = to_ast to_ in
        Arrow { from; to_ }
    | Forall { ty_vars; type_; _ } ->
        let type_ = to_ast type_ in
        Forall { ty_vars; type_ }

  let map f = function
    | Const { name; span } -> Const { name; span = f span }
    | Var { name; span } -> Var { name; span = f span }
    | Arrow { from; to_; span } -> Arrow { from; to_; span = f span }
    | Forall { ty_vars; type_; span } ->
        Forall { ty_vars; type_; span = f span }
end

module Expr = struct
  type 'a t =
    | Int of { value : int; span : 'a }
    | Bool of { value : bool; span : 'a }
    | Var of { name : string; span : 'a }
    | Let of {
        name : string;
        def_t : 'a Type.t option;
        def : 'a t;
        body : 'a t;
        span : 'a;
      }
    | If of { cond : 'a t; con : 'a t; alt : 'a t; span : 'a }
    | Lam of {
        param : string;
        param_t : 'a Type.t option;
        body : 'a t;
        span : 'a;
      }
    | App of { func : 'a t; arg : 'a t; span : 'a }
    | Ann of { expr : 'a t; ann : 'a Type.t; span : 'a }
  [@@deriving show]

  let rec to_ast e : Ast.expr =
    match e with
    | Int { value; _ } -> Int { value }
    | Bool { value; _ } -> Bool { value }
    | Var { name; _ } -> Var { name }
    | Let { name; def_t; def; body; _ } ->
        let def_t = Option.map Type.to_ast def_t in
        let def = to_ast def in
        let body = to_ast body in
        Let { name; def_t; def; body }
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

  let map f = function
    | Int { value; span } -> Int { value; span = f span }
    | Bool { value; span } -> Bool { value; span = f span }
    | Var { name; span } -> Var { name; span = f span }
    | Let { name; def_t; def; body; span } ->
        Let { name; def_t; def; body; span = f span }
    | If { cond; con; alt; span } -> If { cond; con; alt; span = f span }
    | Lam { param; param_t; body; span } ->
        Lam { param; param_t; body; span = f span }
    | App { func; arg; span } -> App { func; arg; span = f span }
    | Ann { expr; ann; span } -> Ann { expr; ann; span = f span }
end

module Binding = struct
  type 'a t =
    | Def of {
        name : string;
        ann : 'a Type.t option;
        expr : 'a Expr.t;
        span : 'a;
      }
  [@@deriving show]

  let to_ast b : Ast.binding =
    match b with
    | Def { name; ann; expr; _ } ->
        let ann = Option.map Type.to_ast ann in
        let expr = Expr.to_ast expr in
        Def { name; ann; expr }
end

module Module = struct
  type 'a t =
    | Module of { name : string; bindings : 'a Binding.t list; span : 'a }
  [@@deriving show]

  let to_ast m : Ast.module_ =
    match m with
    | Module { name; bindings; _ } ->
        let bindings = List.map Binding.to_ast bindings in
        Module { name; bindings }
end

type 'a ty = 'a Type.t
type 'a expr = 'a Expr.t
type 'a binding = 'a Binding.t
type 'a module_ = 'a Module.t
