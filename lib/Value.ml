module TmCtx = Ctx.Make (String)

type t =
  | Int of int
  | Bool of bool
  | Closure of { ctx : t TmCtx.t; param : string; body : Tast.expr }
  | Thunk of { ctx : t TmCtx.t; expr : Tast.expr }
  | Native of (t -> t)
  | Fix of { ctx : t TmCtx.t; name : string; expr : Tast.expr }

let show = function
  | Int n -> string_of_int n
  | Bool true -> "True"
  | Bool false -> "False"
  | Closure _ -> "<closure>"
  | Thunk _ -> "<thunk>"
  | Native _ -> "<native>"
  | Fix _ -> "<fixpoint>"

let lift (f : t -> t) : t = Native f
let lift2 (f : t -> t -> t) : t = Native (fun x -> lift (f x))

let get_int v =
  match v with
  | Int n -> n
  | _ -> failwith ("cannot get Int from: " ^ show v)

let get_bool v =
  match v with
  | Bool b -> b
  | _ -> failwith ("cannot get Bool from: " ^ show v)
