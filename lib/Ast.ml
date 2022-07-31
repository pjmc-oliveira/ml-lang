include Syn.Make (struct
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