include Syn.Make (struct
  (* Type extensions *)
  type xty = Syn.void [@@deriving show]

  (* Expr extensions *)
  type ('e, 't) lit = Type.mono * Source.Span.t [@@deriving show]
  type ('e, 't) var = Type.mono * Source.Span.t [@@deriving show]
  type ('e, 't) let_ = Type.mono * Source.Span.t [@@deriving show]
  type ('e, 't) if_ = Type.mono * Source.Span.t [@@deriving show]

  (* type information  from [param_t] to [body_t]*)
  type ('e, 't) lam = Type.mono * Type.mono * Source.Span.t [@@deriving show]
  type ('e, 't) app = Type.mono * Source.Span.t [@@deriving show]
  type ('e, 't) ext = Syn.void [@@deriving show]

  (* Bindings and Module *)
  type 't def = Type.poly * Source.Span.t [@@deriving show]
  type xmodu = Source.Span.t [@@deriving show]
end)

let type_of_expr (e : expr) : Type.mono =
  match e with
  | ELit ((ty, _), _) -> ty
  | EVar ((ty, _), _) -> ty
  | ELet ((ty, _), _, _, _) -> ty
  | EIf ((ty, _), _, _, _) -> ty
  | ELam ((param_t, body_t, _), _, _) -> Type.Arrow (param_t, body_t)
  | EApp ((ty, _), _, _) -> ty
  | EExt _ -> failwith "Tast.type_of_expr void"

let rec map_type (f : Type.mono -> Type.mono) : expr -> expr = function
  | ELit ((ty, span), lit) -> ELit ((f ty, span), lit)
  | EVar ((ty, span), name) -> EVar ((f ty, span), name)
  | ELet ((ty, span), name, def, body) ->
      ELet ((f ty, span), name, map_type f def, map_type f body)
  | EIf ((ty, span), cond, con, alt) ->
      EIf ((f ty, span), map_type f cond, map_type f con, map_type f alt)
  | ELam ((param_t, body_t, span), param, body) ->
      ELam ((f param_t, f body_t, span), param, map_type f body)
  | EApp ((ty, span), func, arg) ->
      EApp ((f ty, span), map_type f func, map_type f arg)
  | EExt _ -> failwith "Tast.type_of_expr void"
