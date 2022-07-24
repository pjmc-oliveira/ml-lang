type void = []

module type Ext = sig
  (* Type extensions *)
  type xty

  (* Expr extensions *)

  type ('e, 't) lit
  type ('e, 't) var
  type ('e, 't) let_
  type ('e, 't) if_
  type ('e, 't) lam
  type ('e, 't) app
  type ('e, 't) ext

  (* Bindings and Module *)
  type 't def
  type xmodu

  (* Pretty printers *)
  val pp_xty : Format.formatter -> xty -> unit

  val pp_lit :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) lit ->
    unit

  val pp_var :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) var ->
    unit

  val pp_let_ :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) let_ ->
    unit

  val pp_if_ :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) if_ ->
    unit

  val pp_lam :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) lam ->
    unit

  val pp_app :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) app ->
    unit

  val pp_ext :
    (Format.formatter -> 'e -> unit) ->
    (Format.formatter -> 't -> unit) ->
    Format.formatter ->
    ('e, 't) ext ->
    unit

  val pp_def :
    (Format.formatter -> 't -> unit) -> Format.formatter -> 't def -> unit

  val pp_xmodu : Format.formatter -> xmodu -> unit
end

module Make (X : Ext) = struct
  include X

  type ty =
    | TCon of X.xty * string
    | TVar of X.xty * string
    | TArr of X.xty * ty * ty
    | TForall of X.xty * string list * ty
  [@@deriving show]

  let map_ty f = function
    | TCon (x, name) -> TCon (f x, name)
    | TVar (x, name) -> TVar (f x, name)
    | TArr (x, from, to_) -> TArr (f x, from, to_)
    | TForall (x, tvars, ty) -> TForall (f x, tvars, ty)

  type lit = Int of int | Bool of bool [@@deriving show]

  type expr =
    | ELit of (expr, ty) X.lit * lit
    | EVar of (expr, ty) X.var * string
    | ELet of (expr, ty) X.let_ * string * expr * expr
    | EIf of (expr, ty) X.if_ * expr * expr * expr
    | ELam of (expr, ty) X.lam * string * expr
    | EApp of (expr, ty) X.app * expr * expr
    | EExt of (expr, ty) X.ext
  [@@deriving show]

  type bind = Def of ty X.def * string * expr [@@deriving show]
  type modu = Module of X.xmodu * string * bind list [@@deriving show]
end

module Ast = Make (struct
  (* Type extensions *)
  type xty = unit [@@deriving show]

  (* Expr extensions *)
  type ('e, 't) lit = unit [@@deriving show]
  type ('e, 't) var = unit [@@deriving show]
  type ('e, 't) let_ = 't option [@@deriving show]
  type ('e, 't) if_ = unit [@@deriving show]
  type ('e, 't) lam = 't option [@@deriving show]
  type ('e, 't) app = unit [@@deriving show]
  type ('e, 't) ext = [ `Ann of 'e * 't ] [@@deriving show]

  (* Bindings and Module *)
  type 't def = 't option [@@deriving show]
  type xmodu = unit [@@deriving show]
end)

module Cst = struct
  include Make (struct
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
    | TCon (_, name) -> TCon ((), name)
    | TVar (_, name) -> TVar ((), name)
    | TArr (_, from, to_) ->
        let from = ty_to_ast from in
        let to_ = ty_to_ast to_ in
        TArr ((), from, to_)
    | TForall (_, tvars, ty) ->
        let ty = ty_to_ast ty in
        TForall ((), tvars, ty)

  let lit_to_ast (l : lit) : Ast.lit =
    match l with
    | Int v -> Int v
    | Bool v -> Bool v

  let rec expr_to_ast (e : expr) : Ast.expr =
    match (e : expr) with
    | ELit (_, lit) -> ELit ((), lit_to_ast lit)
    | EVar (_, name) -> EVar ((), name)
    | ELet ((_, def_t), name, def, body) ->
        let def_t = Option.map ty_to_ast def_t in
        let def = expr_to_ast def in
        let body = expr_to_ast body in
        ELet (def_t, name, def, body)
    | EIf (_, cond, con, alt) ->
        let cond = expr_to_ast cond in
        let con = expr_to_ast con in
        let alt = expr_to_ast alt in
        EIf ((), cond, con, alt)
    | ELam ((_, param_t), param, body) ->
        let param_t = Option.map ty_to_ast param_t in
        let body = expr_to_ast body in
        ELam (param_t, param, body)
    | EApp (_, func, arg) ->
        let func = expr_to_ast func in
        let arg = expr_to_ast arg in
        EApp ((), func, arg)
    | EExt (`Ann (_, expr, ty)) ->
        let expr = expr_to_ast expr in
        let ty = ty_to_ast ty in
        EExt (`Ann (expr, ty))

  let bind_to_ast (b : bind) : Ast.bind =
    match b with
    | Def ((_, ty), name, expr) ->
        Def (Option.map ty_to_ast ty, name, expr_to_ast expr)

  let to_ast (m : modu) : Ast.modu =
    match m with
    | Module (_, name, bindings) ->
        Module ((), name, List.map bind_to_ast bindings)
end

module Tast = Make (struct
  (* Type extensions *)
  type xty = Type.t * Source.Span.t [@@deriving show]

  (* Expr extensions *)
  type ('e, 't) lit = Type.t * Source.Span.t [@@deriving show]
  type ('e, 't) var = Type.t * Source.Span.t [@@deriving show]
  type ('e, 't) let_ = Type.t * Source.Span.t [@@deriving show]
  type ('e, 't) if_ = Type.t * Source.Span.t [@@deriving show]

  (* type information  from [param_t] to [body_t]*)
  type ('e, 't) lam = Type.t * Type.t * Source.Span.t [@@deriving show]
  type ('e, 't) app = Type.t * Source.Span.t [@@deriving show]
  type ('e, 't) ext = void

  let pp_ext :
      (Format.formatter -> 'e -> unit) ->
      (Format.formatter -> 't -> unit) ->
      Format.formatter ->
      ('e, 't) ext ->
      unit =
   fun _pp_e _pp_t _fmt _expr -> ()

  (* Bindings and Module *)
  type 't def = Type.t * Source.Span.t [@@deriving show]
  type xmodu = Source.Span.t [@@deriving show]
end)
