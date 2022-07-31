include Syn.Make (struct
  (* Type extensions *)
  type xty = Source.Span.t [@@deriving show]

  (* Expr extensions *)
  type ('e, 't) lit = Source.Span.t [@@deriving show]
  type ('e, 't) var = Source.Span.t [@@deriving show]
  type ('e, 't) let_ = Source.Span.t * 't option [@@deriving show]
  type ('e, 't) if_ = Source.Span.t [@@deriving show]
  type ('e, 't) lam = Source.Span.t * 't option [@@deriving show]
  type ('e, 't) app = Source.Span.t [@@deriving show]
  type ('e, 't) ext = [ `Ann of Source.Span.t * 'e * 't ] [@@deriving show]

  (* Bindings and Module *)
  type 't def = Source.Span.t * 't option [@@deriving show]
  type xmodu = Source.Span.t [@@deriving show]
end)

let map_expr (f : Source.span -> Source.span) (e : expr) =
  match e with
  | ELit (x, lit) -> ELit (f x, lit)
  | EVar (x, name) -> EVar (f x, name)
  | ELet ((x, def_t), name, def, body) -> ELet ((f x, def_t), name, def, body)
  | EIf (x, cond, con, alt) -> EIf (f x, cond, con, alt)
  | ELam ((x, param_t), param, body) -> ELam ((f x, param_t), param, body)
  | EApp (x, func, arg) -> EApp (f x, func, arg)
  | EExt (`Ann (x, expr, ty)) -> EExt (`Ann (f x, expr, ty))

let rec ty_to_ast (t : ty) : Ast.ty =
  match t with
  | TCon (_, name) -> TCon name
  | TVar (_, name) -> TVar name
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
  | ELet ((_, def_t), name, def, body) ->
      let def_t = Option.map ty_to_ast def_t in
      let def = expr_to_ast def in
      let body = expr_to_ast body in
      ELet (def_t, name, def, body)
  | EIf (_, cond, con, alt) ->
      let cond = expr_to_ast cond in
      let con = expr_to_ast con in
      let alt = expr_to_ast alt in
      EIf (cond, con, alt)
  | ELam ((_, param_t), param, body) ->
      let param_t = Option.map ty_to_ast param_t in
      let body = expr_to_ast body in
      ELam (param_t, param, body)
  | EApp (_, func, arg) ->
      let func = expr_to_ast func in
      let arg = expr_to_ast arg in
      EApp (func, arg)
  | EExt (`Ann (_, expr, ty)) ->
      let expr = expr_to_ast expr in
      let ty = ty_to_ast ty in
      EAnn (expr, ty)

let bind_to_ast (b : bind) : Ast.bind =
  match b with
  | Def ((_, scheme), name, expr) ->
      Def (Option.map scheme_to_ast scheme, name, expr_to_ast expr)

let to_ast (m : modu) : Ast.modu =
  match m with
  | Module (_, name, bindings) -> Module (name, List.map bind_to_ast bindings)
