open Extensions

type lit =
  | Int of int
  | Bool of bool
[@@deriving show]

type pat = PCon of string * string list [@@deriving show]

type expr =
  | ELit of Type.mono * lit
  | EVar of Type.mono * string
  | ELet of Type.mono * string * expr * expr
  | ELetRec of Type.mono * string * expr * expr
  | EIf of Type.mono * expr * expr * expr
  (* Type of [param] to [body] *)
  | ELam of Type.mono * Type.mono * string * expr
  | EApp of Type.mono * expr * expr
  | EMatch of Type.mono * expr * (pat * expr) list
[@@deriving show]

type 'a expr_f =
  | ELitF of Type.mono * lit
  | EVarF of Type.mono * string
  | ELetF of Type.mono * string * 'a * 'a
  | ELetRecF of Type.mono * string * 'a * 'a
  | EIfF of Type.mono * 'a * 'a * 'a
  | ELamF of Type.mono * Type.mono * string * 'a
  | EAppF of Type.mono * 'a * 'a
  | EMatchF of Type.mono * 'a * (pat * 'a) list
[@@deriving show]

type ty_def =
  | TyDef of {
      name : string;
      kind : Type.kind;
      alts : (string * Type.poly) list;
    }
[@@deriving show]

type tm_def =
  | TmDef of {
      name : string;
      scheme : Type.poly;
      expr : expr;
    }
[@@deriving show]

type modu =
  | Module of {
      name : string;
      types : ty_def list;
      terms : tm_def list;
    }
[@@deriving show]

let type_of_expr (e : expr) : Type.mono =
  match e with
  | ELit (ty, _) -> ty
  | EVar (ty, _) -> ty
  | ELet (ty, _, _, _) -> ty
  | ELetRec (ty, _, _, _) -> ty
  | EIf (ty, _, _, _) -> ty
  | ELam (param_t, body_t, _, _) -> Type.Arrow (param_t, body_t)
  | EApp (ty, _, _) -> ty
  | EMatch (ty, _, _) -> ty

let rec map_type (f : Type.mono -> Type.mono) : expr -> expr = function
  | ELit (ty, lit) -> ELit (f ty, lit)
  | EVar (ty, name) -> EVar (f ty, name)
  | ELet (ty, name, def, body) ->
      ELet (f ty, name, map_type f def, map_type f body)
  | ELetRec (ty, name, def, body) ->
      ELetRec (f ty, name, map_type f def, map_type f body)
  | EIf (ty, cond, con, alt) ->
      EIf (f ty, map_type f cond, map_type f con, map_type f alt)
  | ELam (param_t, body_t, param, body) ->
      ELam (f param_t, f body_t, param, map_type f body)
  | EApp (ty, func, arg) -> EApp (f ty, map_type f func, map_type f arg)
  | EMatch (ty, expr, alts) ->
      EMatch
        ( f ty,
          map_type f expr,
          List.map (fun (pat, case) -> (pat, map_type f case)) alts )

let expr_f_to_expr : expr expr_f -> expr = function
  | ELitF (ty, lit) -> ELit (ty, lit)
  | EVarF (ty, name) -> EVar (ty, name)
  | ELetF (ty, name, def, body) -> ELet (ty, name, def, body)
  | ELetRecF (ty, name, def, body) -> ELetRec (ty, name, def, body)
  | EIfF (ty, cond, con, alt) -> EIf (ty, cond, con, alt)
  | ELamF (param_t, body_t, param, body) -> ELam (param_t, body_t, param, body)
  | EAppF (ty, func, arg) -> EApp (ty, func, arg)
  | EMatchF (ty, expr, cases) -> EMatch (ty, expr, cases)

let rec fold_expr_result (f : 'a expr_f -> ('a, 'e) result) :
    expr -> ('a, 'e) result =
  let open Result.Syntax in
  function
  | ELit (ty, lit) -> f (ELitF (ty, lit))
  | EVar (ty, name) -> f (EVarF (ty, name))
  | ELet (ty, name, def, body) ->
      let* def = fold_expr_result f def in
      let* body = fold_expr_result f body in
      f (ELetF (ty, name, def, body))
  | ELetRec (ty, name, def, body) ->
      let* def = fold_expr_result f def in
      let* body = fold_expr_result f body in
      f (ELetRecF (ty, name, def, body))
  | EIf (ty, cond, con, alt) ->
      let* cond = fold_expr_result f cond in
      let* con = fold_expr_result f con in
      let* alt = fold_expr_result f alt in
      f (EIfF (ty, cond, con, alt))
  | ELam (in_t, out_t, param, body) ->
      let* body = fold_expr_result f body in
      f (ELamF (in_t, out_t, param, body))
  | EApp (ty, func, arg) ->
      let* func = fold_expr_result f func in
      let* arg = fold_expr_result f arg in
      f (EAppF (ty, func, arg))
  | EMatch (ty, expr, alts) ->
      let* expr = fold_expr_result f expr in
      let* alts =
        Result.traverse_list
          (fun (p, e) ->
            let* e = fold_expr_result f e in
            Ok (p, e))
          alts
      in
      f (EMatchF (ty, expr, alts))
