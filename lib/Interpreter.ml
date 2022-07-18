module TmCtx = Ctx.Make (String)

type tm_ctx = Value.t TmCtx.t

module S = StateResult

type 'a t = ('a, tm_ctx, Error.t list) StateResult.t

let error ?location lines : Error.t = { kind = Interpreter; lines; location }

open StateResult.Syntax

let define name value = S.mut (TmCtx.insert name value)

let lookup location name =
  let* ctx = S.get in
  match TmCtx.lookup name ctx with
  | Some value -> S.pure value
  | None -> S.fail [ error ~location [ Text ("Unbound variable: " ^ name) ] ]

let eval (e : Tast.expr) : Value.t t =
  match e with
  | Const { value; _ } -> S.pure (Value.Int value)
  | Var { name; span; _ } ->
      let* value = lookup span name in
      S.pure value

let binding (b : Tast.binding) : Value.t t =
  match b with
  | Def { name; expr; _ } ->
      let* value = eval expr in
      let* _ = define name value in
      S.pure value

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
        | Some b -> S.pure b
        | None ->
            S.fail
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
