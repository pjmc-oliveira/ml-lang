type ty =
  | TCon of Source.Span.t * string
  | TVar of Source.Span.t * string
  | TApp of Source.Span.t * ty * ty
  | TArr of Source.Span.t * ty * ty
[@@deriving show]

type scheme = TMono of ty | TForall of Source.Span.t * string list * ty
[@@deriving show]

let map_ty f = function
  | TCon (x, name) -> TCon (f x, name)
  | TVar (x, name) -> TVar (f x, name)
  | TApp (x, func, arg) -> TApp (f x, func, arg)
  | TArr (x, from, to_) -> TArr (f x, from, to_)

let map_scheme f = function
  | TMono ty -> TMono (map_ty f ty)
  | TForall (x, tvars, ty) -> TForall (f x, tvars, ty)

type lit = Int of int | Bool of bool [@@deriving show]
type pat = PCon of string * string list [@@deriving show]

type expr =
  | ELit of Source.Span.t * lit
  | EVar of Source.Span.t * string
  | ELet of Source.Span.t * string * ty option * expr * expr
  | EIf of Source.Span.t * expr * expr * expr
  | ELam of Source.Span.t * string * ty option * expr
  | EApp of Source.Span.t * expr * expr
  | EAnn of Source.Span.t * expr * ty
  | EMatch of Source.Span.t * expr * (pat * expr) list
[@@deriving show]

type alt = string * ty list [@@deriving show]
type tm_def = Source.Span.t * string * scheme option * expr [@@deriving show]
type ty_def = Source.Span.t * string * string list * alt list [@@deriving show]
type bind = Def of tm_def | Type of ty_def [@@deriving show]
type modu = Module of Source.Span.t * string * bind list [@@deriving show]

(* Helpers *)

let map_expr (f : Source.span -> Source.span) (e : expr) =
  match e with
  | ELit (x, lit) -> ELit (f x, lit)
  | EVar (x, name) -> EVar (f x, name)
  | ELet (x, name, def_t, def, body) -> ELet (f x, name, def_t, def, body)
  | EIf (x, cond, con, alt) -> EIf (f x, cond, con, alt)
  | ELam (x, param, param_t, body) -> ELam (f x, param, param_t, body)
  | EApp (x, func, arg) -> EApp (f x, func, arg)
  | EAnn (x, expr, ty) -> EAnn (f x, expr, ty)
  | EMatch (x, expr, alts) -> EMatch (f x, expr, alts)

let rec ty_to_ast (t : ty) : Ast.ty =
  match t with
  | TCon (_, name) -> TCon name
  | TVar (_, name) -> TVar name
  | TApp (_, func, arg) ->
      let func = ty_to_ast func in
      let arg = ty_to_ast arg in
      TApp (func, arg)
  | TArr (_, from, to_) ->
      let from = ty_to_ast from in
      let to_ = ty_to_ast to_ in
      TArr (from, to_)

let scheme_to_ast (s : scheme) : Ast.scheme =
  match s with
  | TMono ty -> TMono (ty_to_ast ty)
  | TForall (_, tvars, ty) ->
      let ty = ty_to_ast ty in
      TForall (tvars, ty)

let lit_to_ast (l : lit) : Ast.lit =
  match l with
  | Int v -> Int v
  | Bool v -> Bool v

let rec expr_to_ast (e : expr) : Ast.expr =
  match (e : expr) with
  | ELit (_, lit) -> ELit (lit_to_ast lit)
  | EVar (_, name) -> EVar name
  | ELet (_, name, def_t, def, body) ->
      let def_t = Option.map ty_to_ast def_t in
      let def = expr_to_ast def in
      let body = expr_to_ast body in
      ELet (name, def_t, def, body)
  | EIf (_, cond, con, alt) ->
      let cond = expr_to_ast cond in
      let con = expr_to_ast con in
      let alt = expr_to_ast alt in
      EIf (cond, con, alt)
  | ELam (_, param, param_t, body) ->
      let param_t = Option.map ty_to_ast param_t in
      let body = expr_to_ast body in
      ELam (param, param_t, body)
  | EApp (_, func, arg) ->
      let func = expr_to_ast func in
      let arg = expr_to_ast arg in
      EApp (func, arg)
  | EAnn (_, expr, ty) ->
      let expr = expr_to_ast expr in
      let ty = ty_to_ast ty in
      EAnn (expr, ty)
  | EMatch (_, expr, alts) ->
      let expr = expr_to_ast expr in
      EMatch (expr, List.map pat_to_ast alts)

and pat_to_ast (pat, expr) =
  match pat with
  | PCon (con, vars) -> (Ast.PCon (con, vars), expr_to_ast expr)

let alt_to_ast (head, tys) = (head, List.map ty_to_ast tys)

let bind_to_ast (b : bind) : Ast.bind =
  match b with
  | Def (_, name, scheme, expr) ->
      Def (Option.map scheme_to_ast scheme, name, expr_to_ast expr)
  | Type (_, name, vars, alts) -> Type (name, vars, List.map alt_to_ast alts)

let to_ast (m : modu) : Ast.modu =
  match m with
  | Module (_, name, bindings) -> Module (name, List.map bind_to_ast bindings)
