open Extensions

type lit =
  | Int of int
  | Bool of bool
[@@deriving show]

type pat = PCon of string * string list [@@deriving show]

type expr =
  | ELit of Type.mono * Source.Span.t * lit
  | EVar of Type.mono * Source.Span.t * string
  | ELet of Type.mono * Source.Span.t * string * expr * expr
  | ELetRec of Type.mono * Source.Span.t * string * expr * expr
  | EIf of Type.mono * Source.Span.t * expr * expr * expr
  (* Type of [param] to [body] *)
  | ELam of Type.mono * Type.mono * Source.Span.t * string * expr
  | EApp of Type.mono * Source.Span.t * expr * expr
  | EMatch of Type.mono * Source.Span.t * expr * (pat * expr) list
[@@deriving show]

type 'a expr_f =
  | ELitF of Type.mono * Source.Span.t * lit
  | EVarF of Type.mono * Source.Span.t * string
  | ELetF of Type.mono * Source.Span.t * string * 'a * 'a
  | ELetRecF of Type.mono * Source.Span.t * string * 'a * 'a
  | EIfF of Type.mono * Source.Span.t * 'a * 'a * 'a
  | ELamF of Type.mono * Type.mono * Source.Span.t * string * 'a
  | EAppF of Type.mono * Source.Span.t * 'a * 'a
  | EMatchF of Type.mono * Source.Span.t * 'a * (pat * 'a) list
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
  | ELetRec (ty, _, _, _, _) -> ty
  | EIf (ty, _, _, _, _) -> ty
  | ELam (param_t, body_t, _, _, _) -> Type.Arrow (param_t, body_t)
  | EApp (ty, _, _, _) -> ty
  | EMatch (ty, _, _, _) -> ty

let rec map_type (f : Type.mono -> Type.mono) : expr -> expr = function
  | ELit (ty, span, lit) -> ELit (f ty, span, lit)
  | EVar (ty, span, name) -> EVar (f ty, span, name)
  | ELet (ty, span, name, def, body) ->
      ELet (f ty, span, name, map_type f def, map_type f body)
  | ELetRec (ty, span, name, def, body) ->
      ELetRec (f ty, span, name, map_type f def, map_type f body)
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
  | ELetRec (_, sp, _, _, _) -> sp
  | EIf (_, sp, _, _, _) -> sp
  | ELam (_, _, sp, _, _) -> sp
  | EApp (_, sp, _, _) -> sp
  | EMatch (_, sp, _, _) -> sp

let expr_f_to_expr : expr expr_f -> expr = function
  | ELitF (ty, sp, lit) -> ELit (ty, sp, lit)
  | EVarF (ty, sp, name) -> EVar (ty, sp, name)
  | ELetF (ty, sp, name, def, body) -> ELet (ty, sp, name, def, body)
  | ELetRecF (ty, sp, name, def, body) -> ELetRec (ty, sp, name, def, body)
  | EIfF (ty, sp, cond, con, alt) -> EIf (ty, sp, cond, con, alt)
  | ELamF (param_t, body_t, sp, param, body) ->
      ELam (param_t, body_t, sp, param, body)
  | EAppF (ty, sp, func, arg) -> EApp (ty, sp, func, arg)
  | EMatchF (ty, sp, expr, cases) -> EMatch (ty, sp, expr, cases)

let rec fold_expr_result (f : 'a expr_f -> ('a, 'e) result) :
    expr -> ('a, 'e) result =
  let open Result.Syntax in
  function
  | ELit (ty, sp, lit) -> f (ELitF (ty, sp, lit))
  | EVar (ty, sp, name) -> f (EVarF (ty, sp, name))
  | ELet (ty, sp, name, def, body) ->
      let* def = fold_expr_result f def in
      let* body = fold_expr_result f body in
      f (ELetF (ty, sp, name, def, body))
  | ELetRec (ty, sp, name, def, body) ->
      let* def = fold_expr_result f def in
      let* body = fold_expr_result f body in
      f (ELetRecF (ty, sp, name, def, body))
  | EIf (ty, sp, cond, con, alt) ->
      let* cond = fold_expr_result f cond in
      let* con = fold_expr_result f con in
      let* alt = fold_expr_result f alt in
      f (EIfF (ty, sp, cond, con, alt))
  | ELam (in_t, out_t, sp, param, body) ->
      let* body = fold_expr_result f body in
      f (ELamF (in_t, out_t, sp, param, body))
  | EApp (ty, sp, func, arg) ->
      let* func = fold_expr_result f func in
      let* arg = fold_expr_result f arg in
      f (EAppF (ty, sp, func, arg))
  | EMatch (ty, sp, expr, alts) ->
      let* expr = fold_expr_result f expr in
      let* alts =
        Result.traverse_list
          (fun (p, e) ->
            let* e = fold_expr_result f e in
            Ok (p, e))
          alts
      in
      f (EMatchF (ty, sp, expr, alts))
