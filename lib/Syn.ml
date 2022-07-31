type void = []

let pp_void (_fmt : Format.formatter) (_void : void) = ()

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
  [@@deriving show]

  type scheme = TMono of ty | TForall of X.xty * string list * ty
  [@@deriving show]

  let map_ty f = function
    | TCon (x, name) -> TCon (f x, name)
    | TVar (x, name) -> TVar (f x, name)
    | TArr (x, from, to_) -> TArr (f x, from, to_)

  let map_scheme f = function
    | TMono ty -> TMono (map_ty f ty)
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

  type bind = Def of scheme X.def * string * expr [@@deriving show]
  type modu = Module of X.xmodu * string * bind list [@@deriving show]
end
