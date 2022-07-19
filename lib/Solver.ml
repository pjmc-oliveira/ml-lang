module TyCtx = Ctx.Make (String)

type ty_ctx = Type.t TyCtx.t

let error ?span lines : Error.t =
  { kind = Error.Kind.Solver; location = span; lines }

let unbound_var name span : Error.t =
  error ~span [ Text ("Unbound variable: " ^ name) ]

let wrong_arg_type expected_t actual_t span : Error.t =
  error ~span
    [
      Text "Wrong argument type";
      Text ("Expected: " ^ Type.show expected_t);
      Text ("But got: " ^ Type.show actual_t);
    ]

let expr_not_a_function _expr span : Error.t =
  error ~span [ Text "Cannot a apply to non-function values" ]

let if_branch_mismatch con_t alt_t span : Error.t =
  error ~span
    [
      Text "If branches must have the same type";
      Text ("then-branch has type: " ^ Type.show con_t);
      Text ("but else-branch has type: " ^ Type.show alt_t);
    ]

let if_condition_not_bool cond_t span : Error.t =
  error ~span
    [
      Text ("Expected if-condition to be type: " ^ Type.show Bool);
      Text ("But got type: " ^ Type.show cond_t);
    ]

let rec solve_type (ty : Cst.Type.t) : (Type.t, Error.t) result =
  let open Result.Syntax in
  match ty with
  | Const { name; _ } -> (
      match name with
      | "Int" -> Ok Int
      | "Bool" -> Ok Bool
      | _ -> Error (error [ Text ("Unbound type: " ^ name) ]))
  | Arrow { from; to_; _ } ->
      let* from = solve_type from in
      let* to_ = solve_type to_ in
      Ok (Type.Arrow { from; to_ })

let rec expression (e : Cst.expr) (ctx : ty_ctx) :
    (Tast.expr * Type.t, Error.t) result =
  let open Result.Syntax in
  match e with
  | Int { value; span } ->
      let type_ = Type.Int in
      Ok (Tast.Expr.Int { value; span; type_ }, type_)
  | Bool { value; span } ->
      let type_ = Type.Bool in
      Ok (Tast.Expr.Bool { value; span; type_ }, type_)
  | Var { name; span } -> (
      match TyCtx.lookup name ctx with
      | Some type_ -> Ok (Tast.Expr.Var { name; span; type_ }, type_)
      | None -> Error (unbound_var name span))
  | Let { name; def; body; span } ->
      let* def, def_t = expression def ctx in
      let ctx' = TyCtx.insert name def_t ctx in
      let* body, type_ = expression body ctx' in
      Ok (Tast.Expr.Let { name; def_t; def; body; span; type_ }, type_)
  | If { cond; con; alt; span } -> (
      let* cond, cond_t = expression cond ctx in
      match cond_t with
      | Bool ->
          let* con, con_t = expression con ctx in
          let* alt, alt_t = expression alt ctx in
          if con_t = alt_t then
            Ok (Tast.Expr.If { cond; con; alt; span; type_ = con_t }, con_t)
          else
            Error (if_branch_mismatch con_t alt_t span)
      | _ -> Error (if_condition_not_bool cond_t span))
  | Lam { param; param_t; body; span } -> (
      match param_t with
      | None -> failwith "TODO"
      | Some param_t ->
          let* param_t = solve_type param_t in
          let ctx' = TyCtx.insert param param_t ctx in
          let* body, type_ = expression body ctx' in
          Ok
            ( Tast.Expr.Lam { param; param_t; body; span; type_ },
              Type.Arrow { from = param_t; to_ = type_ } ))
  | App { func; arg; span } -> (
      let* func, func_t = expression func ctx in
      match func_t with
      | Arrow { from = param_t; to_ = type_ } ->
          let* arg, arg_t = expression arg ctx in
          if param_t = arg_t then
            Ok (Tast.Expr.App { func; arg; span; type_ }, type_)
          else
            Error (wrong_arg_type param_t arg_t span)
      | _ -> Error (expr_not_a_function func span))

let binding (ctx : ty_ctx) (b : Cst.binding) :
    (Tast.binding * Type.t, Error.t) result =
  let open Result.Syntax in
  match b with
  | Def { name; expr; span } ->
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
  let open Result.Syntax in
  match m with
  | Module { name; bindings; span } ->
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
