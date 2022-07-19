module TmCtx = Ctx.Make (String)

type t =
  | Int of int
  | Bool of bool
  | Thunk of { ctx : t TmCtx.t; expr : Tast.expr }

let show = function
  | Int n -> string_of_int n
  | Bool true -> "True"
  | Bool false -> "False"
  | Thunk _ -> "<thunk>"
