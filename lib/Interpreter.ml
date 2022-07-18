module TmCtx = Ctx.Make (String)

type tm_ctx = Value.t TmCtx.t
type 'a t = tm_ctx -> ('a * tm_ctx, Error.t list) result

let error ?location lines : Error.t = { kind = Interpreter; lines; location }
let pure (x : 'a) : 'a t = fun ctx -> Ok (x, ctx)

let bind (s : 'a t) (f : 'a -> 'b t) : 'b t =
 fun ctx ->
  match s ctx with
  | Ok (x, ctx') -> (f x) ctx'
  | Error es -> Error es

let prod (tx : 'a t) (ty : 'b t) =
  bind tx (fun x -> bind ty (fun y -> pure (x, y)))

let fail (e : Error.t list) : 'a t = fun _ -> Error e
let get : tm_ctx t = fun ctx -> Ok (ctx, ctx)
let set ctx : unit t = fun _ -> Ok ((), ctx)
let mut f : unit t = fun ctx -> Ok ((), f ctx)

module Syntax = struct
  let ( let* ) = bind
  let ( and* ) = prod
end

open Syntax

let define name value = mut (TmCtx.insert name value)

let lookup location name =
  let* ctx = get in
  match TmCtx.lookup name ctx with
  | Some value -> pure value
  | None -> fail [ error ~location [ Text ("Unbound variable: " ^ name) ] ]

let eval (e : Tast.expr) : Value.t t =
  match e with
  | Const { value; _ } -> pure (Value.Int value)
  | Var { name; span; _ } ->
      let* value = lookup span name in
      pure value

let binding (b : Tast.binding) : Value.t t =
  match b with
  | Def { name; expr; _ } ->
      let* value = eval expr in
      let* _ = define name value in
      pure value

let module_ entrypoint (m : Tast.module_) : Value.t t =
  match m with
  | Module { bindings; span; _ } ->
      let* b =
        match
          List.find_opt
            (fun b ->
              match b with
              | Tast.Binding.Def { name; _ } -> name = entrypoint)
            bindings
        with
        | Some b -> pure b
        | None ->
            fail
              [
                error ~location:span
                  [ Text ("Unbound entrypoint: " ^ entrypoint) ];
              ]
      in
      binding b

let run ?(entrypoint = "main") ?(context = TmCtx.empty) (m : Tast.module_) :
    (Value.t, Error.t list) result =
  match (module_ entrypoint) m context with
  | Ok (value, _) -> Ok value
  | Error e -> Error e
