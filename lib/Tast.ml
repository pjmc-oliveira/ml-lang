module Expr = struct
  type t =
    | Int of { value : int; span : Source.Span.t; type_ : Type.t }
    | Bool of { value : bool; span : Source.Span.t; type_ : Type.t }
    | Var of { name : string; span : Source.Span.t; type_ : Type.t }
    | Let of {
        name : string;
        def : t;
        def_t : Type.t;
        body : t;
        span : Source.Span.t;
        type_ : Type.t;
      }
    | If of { cond : t; con : t; alt : t; span : Source.Span.t; type_ : Type.t }
    | Lam of {
        param : string;
        param_t : Type.t;
        body : t;
        span : Source.Span.t;
        type_ : Type.t;
      }
    | App of { func : t; arg : t; span : Source.Span.t; type_ : Type.t }
  [@@deriving show]

  let rec to_string = function
    | Int { value; _ } -> string_of_int value
    | Bool { value; _ } -> if value then "True" else "False"
    | Var { name; _ } -> name
    | Let { name; def; body; _ } ->
        "let " ^ name ^ " = " ^ to_string def ^ " in " ^ to_string body
    | If { cond; con; alt; _ } ->
        "if " ^ to_string cond ^ " then " ^ to_string con ^ " else "
        ^ to_string alt
    | Lam { param; body; _ } -> "(\\" ^ param ^ ". " ^ to_string body ^ ")"
    | App { func; arg; _ } -> "(" ^ to_string func ^ " " ^ to_string arg ^ ")"

  let span = function
    | Int { span; _ }
    | Bool { span; _ }
    | Var { span; _ }
    | Let { span; _ }
    | If { span; _ }
    | Lam { span; _ }
    | App { span; _ } -> span
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