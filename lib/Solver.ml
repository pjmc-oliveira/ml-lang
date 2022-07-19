module TyCtx = Ctx.Make (String)

type ty_ctx = Type.t TyCtx.t

let error ?span lines : Error.t =
  { kind = Error.Kind.Solver; location = span; lines }

let unbound_var name span : Error.t =
  error ~span [ Text ("Unbound variable: " ^ name) ]

let rec expression (e : Cst.expr) (ctx : ty_ctx) :
    (Tast.expr * Type.t, Error.t) result =
  match e with
  | Int { value; span } ->
      let type_ = Type.Int in
      Ok (Tast.Expr.Int { value; span; type_ }, type_)
  | Var { name; span } -> (
      match TyCtx.lookup name ctx with
      | Some type_ -> Ok (Tast.Expr.Var { name; span; type_ }, type_)
      | None -> Error (unbound_var name span))
  | Let { name; def; body; span } ->
      let open Result.Syntax in
      let* def, ann = expression def ctx in
      let ctx' = TyCtx.insert name ann ctx in
      let* body, type_ = expression body ctx' in
      Ok (Tast.Expr.Let { name; ann; def; body; span; type_ }, type_)

let binding (ctx : ty_ctx) (b : Cst.binding) :
    (Tast.binding * Type.t, Error.t) result =
  match b with
  | Def { name; expr; span } ->
      let open Result.Syntax in
      let* expr, type_ = expression expr ctx in
      Ok (Tast.Binding.Def { name; expr; span; type_ }, type_)

let rec multiple_passes (previous : int) (bindings : Cst.binding list)
    (ctx : ty_ctx) : (Tast.binding list, Error.t list) result =
  let rec loop errs oks bs ctx =
    match bs with
    | [] ->
        if errs = [] then
          Ok (List.rev oks)
        else
          let current = List.length errs in
          if current < previous then
            (* TODO: should this skip the successful bindings on the next pass? *)
            multiple_passes current bindings ctx
          else
            let errors = List.map (fun (_, e) -> e) errs in
            Error errors
    | b :: bs' -> (
        match binding ctx b with
        | Ok ((Def { name; _ } as tast), ty) ->
            let ctx' = TyCtx.insert name ty ctx in
            loop errs (tast :: oks) bs' ctx'
        | Error e -> loop ((b, e) :: errs) oks bs' ctx)
  in
  loop [] [] bindings ctx

let module_ (m : Cst.module_) (ctx : ty_ctx) :
    (Tast.module_, Error.t list) result =
  match m with
  | Module { name; bindings; span } ->
      let open Result.Syntax in
      let* bindings = multiple_passes (List.length bindings) bindings ctx in
      Ok (Tast.Module.Module { name; bindings; span })

let solve_module (m : Cst.module_) (ctx : ty_ctx) :
    (ty_ctx, Error.t list) result =
  let open Result.Syntax in
  let insert_to_ctx ctx (name, ty) = TyCtx.insert name ty ctx in
  let type_of_binding b =
    match b with
    | Tast.Binding.Def { name; type_; _ } -> (name, type_)
  in
  let* m = module_ m ctx in
  match m with
  | Module { bindings; _ } ->
      let bs = List.map type_of_binding bindings in
      let ctx = List.fold_left insert_to_ctx ctx bs in
      Ok ctx
