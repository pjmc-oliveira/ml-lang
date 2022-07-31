module TyCtx = Ctx.Make (String)

type ty_ctx = Type.poly TyCtx.t

module type S = sig
  val module_ : Cst.modu -> ty_ctx -> (Tast.modu, Error.t list) result
  val solve_module : Cst.modu -> ty_ctx -> (ty_ctx, Error.t list) result
end