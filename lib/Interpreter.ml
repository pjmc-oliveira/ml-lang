module TmCtx = Ctx.Make (String)

type tm_ctx = Value.t TmCtx.t

module S = StateResult
open StateResult.Syntax

type 'a t = ('a, tm_ctx, Error.t list) StateResult.t

let error ?location lines : Error.t = { kind = Interpreter; lines; location }

let traverse_list (f : 'a -> 'b t) (ls : 'a list) : 'b list t =
  List.fold_right
    (fun x ys_t ->
      let* y = f x in
      let* ys = ys_t in
      S.pure (y :: ys))
    ls (S.pure [])

let define name value = S.mut (TmCtx.insert name value)

let lookup location name =
  let* ctx = S.get in
  match TmCtx.lookup name ctx with
  | Some value -> S.pure value
  | None -> S.fail [ error ~location [ Text ("Unbound variable: " ^ name) ] ]

let defer (expr : Tast.expr) : Value.t t =
  let* ctx = S.get in
  S.pure (Value.Thunk { ctx; expr })

let rec eval (e : Tast.expr) : Value.t t =
  match e with
  | Int { value; _ } -> S.pure (Value.Int value)
  | Bool { value; _ } -> S.pure (Value.Bool value)
  | Var { name; span; _ } ->
      let* value = lookup span name in
      let* value = force value in
      S.pure value
  | Let { name; def; body; _ } ->
      let* thunk = defer def in
      let* _ = define name thunk in
      eval body

and force (v : Value.t) : Value.t t =
  match v with
  | Int n -> S.pure (Value.Int n)
  | Bool b -> S.pure (Value.Bool b)
  | Thunk { ctx; expr } -> S.scope ctx (eval expr)

let binding (b : Tast.binding) : Value.t t =
  match b with
  | Def { name; expr; _ } ->
      let* value = eval expr in
      let* _ = define name value in
      S.pure value

let defer_binding (b : Tast.binding) : Value.t t =
  match b with
  | Def { name; expr; _ } ->
      let* ctx = S.get in
      let thunk = Value.Thunk { ctx; expr } in
      let* _ = define name thunk in
      S.pure thunk

let find_entrypoint entrypoint bindings : Tast.binding option =
  List.find_opt
    (fun b ->
      match b with
      | Tast.Binding.Def { name; _ } -> name = entrypoint)
    bindings

let module_ entrypoint (m : Tast.module_) : Value.t t =
  match m with
  | Module { bindings; span; _ } ->
      let* _ = traverse_list defer_binding bindings in
      let* b =
        match find_entrypoint entrypoint bindings with
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
