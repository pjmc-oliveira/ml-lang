module Expr = struct
  type 'a t =
    | Int of { value : int; span : 'a; type_ : Type.t }
    | Bool of { value : bool; span : 'a; type_ : Type.t }
    | Var of { name : string; span : 'a; type_ : Type.t }
    | Let of {
        name : string;
        def : 'a t;
        def_t : Type.t;
        body : 'a t;
        span : 'a;
        type_ : Type.t;
      }
    | If of { cond : 'a t; con : 'a t; alt : 'a t; span : 'a; type_ : Type.t }
    | Lam of {
        param : string;
        param_t : Type.t;
        body : 'a t;
        span : 'a;
        type_ : Type.t;
      }
    | App of { func : 'a t; arg : 'a t; span : 'a; type_ : Type.t }
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
  type 'a t =
    | Def of { name : string; expr : 'a Expr.t; span : 'a; type_ : Type.t }
  [@@deriving show]
end

module Module = struct
  type 'a t =
    | Module of { name : string; bindings : 'a Binding.t list; span : 'a }
  [@@deriving show]
end

type 'a expr = 'a Expr.t
type 'a binding = 'a Binding.t
type 'a module_ = 'a Module.t