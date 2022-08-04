type lit = Int of int | Bool of bool [@@deriving show]
type pat = PCon of string * string list [@@deriving show]

type expr =
  | ELit of Type.mono * Source.Span.t * lit
  | EVar of Type.mono * Source.Span.t * string
  | ELet of Type.mono * Source.Span.t * string * expr * expr
  | EIf of Type.mono * Source.Span.t * expr * expr * expr
  (* Type of [param] to [body] *)
  | ELam of Type.mono * Type.mono * Source.Span.t * string * expr
  | EApp of Type.mono * Source.Span.t * expr * expr
  | EMatch of Type.mono * Source.Span.t * expr * (pat * expr) list
[@@deriving show]

type ty_def =
  | TyDef of {
      span : Source.Span.t;
      name : string;
      kind : Type.kind;
      alts : (string * Type.poly) list;
    }
[@@deriving show]

type tm_def =
  | TmDef of {
      span : Source.Span.t;
      name : string;
      scheme : Type.poly;
      expr : expr;
    }
[@@deriving show]

type modu =
  | Module of {
      span : Source.Span.t;
      name : string;
      types : ty_def list;
      terms : tm_def list;
    }
[@@deriving show]

let type_of_expr (e : expr) : Type.mono =
  match e with
  | ELit (ty, _, _) -> ty
  | EVar (ty, _, _) -> ty
  | ELet (ty, _, _, _, _) -> ty
  | EIf (ty, _, _, _, _) -> ty
  | ELam (param_t, body_t, _, _, _) -> Type.Arrow (param_t, body_t)
  | EApp (ty, _, _, _) -> ty
  | EMatch (ty, _, _, _) -> ty

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
  | EMatch (ty, span, expr, alts) ->
      EMatch
        ( f ty,
          span,
          map_type f expr,
          List.map (fun (pat, case) -> (pat, map_type f case)) alts )

let get_span = function
  | ELit (_, sp, _) -> sp
  | EVar (_, sp, _) -> sp
  | ELet (_, sp, _, _, _) -> sp
  | EIf (_, sp, _, _, _) -> sp
  | ELam (_, _, sp, _, _) -> sp
  | EApp (_, sp, _, _) -> sp
  | EMatch (_, sp, _, _) -> sp