module TmCtx = Ctx.Make (String)

type t = Int of int | Thunk of { ctx : t TmCtx.t; expr : Tast.expr }

let show = function
  | Int n -> string_of_int n
  | Thunk _ -> "<thunk>"
