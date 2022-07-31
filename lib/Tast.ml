type lit = Int of int | Bool of bool [@@deriving show]

type expr =
  | ELit of Type.mono * Source.Span.t * lit
  | EVar of Type.mono * Source.Span.t * string
  | ELet of Type.mono * Source.Span.t * string * expr * expr
  | EIf of Type.mono * Source.Span.t * expr * expr * expr
  | ELam of Type.mono * Type.mono * Source.Span.t * string * expr
  | EApp of Type.mono * Source.Span.t * expr * expr
[@@deriving show]

type bind = Def of Type.poly * Source.Span.t * string * expr [@@deriving show]
type modu = Module of Source.Span.t * string * bind list [@@deriving show]

let type_of_expr (e : expr) : Type.mono =
  match e with
  | ELit (ty, _, _) -> ty
  | EVar (ty, _, _) -> ty
  | ELet (ty, _, _, _, _) -> ty
  | EIf (ty, _, _, _, _) -> ty
  | ELam (param_t, body_t, _, _, _) -> Type.Arrow (param_t, body_t)
  | EApp (ty, _, _, _) -> ty

let rec map_type (f : Type.mono -> Type.mono) : expr -> expr = function
  | ELit (ty, span, lit) -> ELit (f ty, span, lit)
  | EVar (ty, span, name) -> EVar (f ty, span, name)
  | ELet (ty, span, name, def, body) ->
      ELet (f ty, span, name, map_type f def, map_type f body)
  | EIf (ty, span, cond, con, alt) ->
      EIf (f ty, span, map_type f cond, map_type f con, map_type f alt)
  | ELam (param_t, body_t, span, param, body) ->
      ELam (f param_t, f body_t, span, param, map_type f body)
  | EApp (ty, span, func, arg) ->
      EApp (f ty, span, map_type f func, map_type f arg)
