open Extensions
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

let lookup location name ctx =
  match TmCtx.lookup name ctx with
  | Some value -> Ok value
  | None -> Error (error ~location [ Text ("Unbound variable: " ^ name) ])

let define name value ctx = TmCtx.insert name value ctx

let defer (expr : Source.span Tast.expr) (ctx : tm_ctx) : Value.t =
  Value.Thunk { ctx; expr }

let fix name expr ctx = Value.Fix { ctx; name; expr }

let rec eval (e : Source.span Tast.expr) (ctx : tm_ctx) :
    (Value.t, Error.t) result =
  let open Result.Syntax in
  match e with
  | Int { value; _ } -> Ok (Value.Int value)
  | Bool { value; _ } -> Ok (Value.Bool value)
  | Var { name; span; _ } -> lookup span name ctx
  | Let { name; def; body; _ } ->
      let fixpoint = fix name def ctx in
      let ctx' = define name fixpoint ctx in
      eval body ctx'
  | If { cond; con; alt; _ } -> (
      let* cond = eval cond ctx in
      let* cond = force cond in
      match cond with
      | Bool true -> eval con ctx
      | Bool false -> eval alt ctx
      | _ -> failwith ("Impossible if-cond not bool: " ^ Value.show cond))
  | Lam { param; body; _ } -> Ok (Value.Closure { ctx; param; body })
  | App { func; arg; _ } -> (
      let arg' = defer arg ctx in
      let* func = eval func ctx in
      let* func = force func in
      match func with
      | Closure { ctx = closure_ctx; param; body } ->
          let closure_ctx' = define param arg' closure_ctx in
          eval body closure_ctx'
      | Native func ->
          (* Defer to create a thunk value from the ast
             then force to pass it into the native function *)
          let arg = defer arg ctx in
          let* arg = force arg in
          Ok (func arg)
      | _ ->
          failwith ("Imposible cannot apply to non-function: " ^ Value.show func)
      )

and force (v : Value.t) : (Value.t, Error.t) result =
  let open Result.Syntax in
  match v with
  | Int n -> Ok (Value.Int n)
  | Bool b -> Ok (Value.Bool b)
  | Closure f -> Ok (Value.Closure f)
  | Thunk { ctx; expr } ->
      let* expr = eval expr ctx in
      force expr
  | Native _ -> Ok v
  | Fix { ctx; name; expr } ->
      let ctx' = define name (Value.Fix { ctx; name; expr }) ctx in
      let* expr = eval expr ctx' in
      force expr

let binding (b : Source.span Tast.binding) (ctx : tm_ctx) :
    (Value.t * tm_ctx, Error.t list) result =
  let open Result.Syntax in
  Result.map_error
    (fun e -> [ e ])
    (match b with
    | Def { expr; _ } ->
        let* value = eval expr ctx in
        Ok (value, ctx))

let defer_binding (b : Source.span Tast.binding) (ctx : tm_ctx) :
    (Value.t * tm_ctx, Error.t list) result =
  match b with
  | Def { name; expr; _ } ->
      let fixpoint = fix name expr ctx in
      let ctx' = define name fixpoint ctx in
      Ok (fixpoint, ctx')

let find_entrypoint entrypoint bindings : Source.span Tast.binding option =
  List.find_opt
    (fun b ->
      match b with
      | Tast.Binding.Def { name; _ } -> name = entrypoint)
    bindings

let module_ entrypoint (m : Source.span Tast.module_) : Value.t t =
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

let run ?(entrypoint = "main") ?(context = TmCtx.empty)
    (m : Source.span Tast.module_) : (Value.t, Error.t list) result =
  let open Result.Syntax in
  match (module_ entrypoint) m context with
  | Ok (value, _) ->
      let* value = Result.map_error (fun e -> [ e ]) (force value) in
      Ok value
  | Error e -> Error e
