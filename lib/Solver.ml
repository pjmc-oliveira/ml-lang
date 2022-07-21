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

let assert_equal ?span exprected_t actual_t : (unit, Error.t) result =
  if exprected_t = actual_t then
    Ok ()
  else
    Error
      (error ?span
         [
           Text "Type mismatch";
           Text ("Expected: " ^ Type.show exprected_t);
           Text ("But got: " ^ Type.show actual_t);
         ])

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

let rec infer (e : Cst.expr) (ctx : ty_ctx) :
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
      let* def, def_t = infer def ctx in
      let ctx' = TyCtx.insert name def_t ctx in
      let* body, type_ = infer body ctx' in
      Ok (Tast.Expr.Let { name; def_t; def; body; span; type_ }, type_)
  | If { cond; con; alt; span } ->
      let* cond, cond_t = infer cond ctx in
      let* _ =
        Result.map_error
          (fun _ -> if_condition_not_bool cond_t span)
          (assert_equal Bool cond_t)
      in
      let* con, con_t = infer con ctx in
      let* alt, alt_t = infer alt ctx in
      if con_t = alt_t then
        Ok (Tast.Expr.If { cond; con; alt; span; type_ = con_t }, con_t)
      else
        Error (if_branch_mismatch con_t alt_t span)
  | Lam { param; param_t; body; span } -> (
      match param_t with
      | None -> failwith "TODO"
      | Some param_t ->
          let* param_t = solve_type param_t in
          let ctx' = TyCtx.insert param param_t ctx in
          let* body, type_ = infer body ctx' in
          Ok
            ( Tast.Expr.Lam { param; param_t; body; span; type_ },
              Type.Arrow { from = param_t; to_ = type_ } ))
  | App { func; arg; span } -> (
      let* func, func_t = infer func ctx in
      match func_t with
      | Arrow { from = param_t; to_ = type_ } ->
          let* arg, arg_t = infer arg ctx in
          if param_t = arg_t then
            Ok (Tast.Expr.App { func; arg; span; type_ }, type_)
          else
            Error (wrong_arg_type param_t arg_t span)
      | _ -> Error (expr_not_a_function func span))
  | Ann { expr; ann; _ } ->
      let* type_ = solve_type ann in
      let* expr = check expr type_ ctx in
      Ok (expr, type_)

and check (e : Cst.expr) (expected_t : Type.t) (ctx : ty_ctx) :
    (Tast.expr, Error.t) result =
  let open Result.Syntax in
  match e with
  | Int { value; span } ->
      let* _ = assert_equal expected_t Int in
      Ok (Tast.Expr.Int { value; span; type_ = Int })
  | Bool { value; span } ->
      let* _ = assert_equal expected_t Bool in
      Ok (Tast.Expr.Bool { value; span; type_ = Bool })
  | Var { name; span } -> (
      match TyCtx.lookup name ctx with
      | Some type_ ->
          let* _ = assert_equal expected_t type_ in
          Ok (Tast.Expr.Var { name; span; type_ })
      | None -> Error (unbound_var name span))
  | Let { name; def; body; span } ->
      let* def, def_t = infer def ctx in
      let ctx' = TyCtx.insert name def_t ctx in
      let* body = check body expected_t ctx' in
      Ok (Tast.Expr.Let { name; def_t; def; body; span; type_ = expected_t })
  | If { cond; con; alt; span } ->
      (* TODO: Should we check this instead of inferring? *)
      let* cond, cond_t = infer cond ctx in
      let* _ =
        Result.map_error
          (fun _ -> if_condition_not_bool cond_t span)
          (assert_equal Bool cond_t)
      in
      let* con = check con expected_t ctx in
      let* alt = check alt expected_t ctx in
      Ok (Tast.Expr.If { cond; con; alt; span; type_ = expected_t })
  | Lam { param; param_t; body; span } -> (
      match expected_t with
      | Arrow { from; to_ } -> (
          match param_t with
          | Some param_t ->
              let* param_t = solve_type param_t in
              let* _ = assert_equal from param_t in
              let ctx' = TyCtx.insert param param_t ctx in
              let* body = check body to_ ctx' in
              Ok
                (Tast.Expr.Lam
                   { param; param_t; body; span; type_ = expected_t })
          | None ->
              let ctx' = TyCtx.insert param from ctx in
              let* body = check body to_ ctx' in
              Ok
                (Tast.Expr.Lam
                   { param; param_t = from; body; span; type_ = expected_t }))
      | _ ->
          (* TODO: Better error message? This will always fail... *)
          let* expr, actual_t = infer e ctx in
          let* _ = assert_equal expected_t actual_t in
          Ok expr)
  | App { func; arg; span } ->
      let* arg, arg_t = infer arg ctx in
      let* func = check func (Arrow { from = arg_t; to_ = expected_t }) ctx in
      Ok (Tast.Expr.App { func; arg; span; type_ = expected_t })
  | Ann { expr; ann; _ } ->
      let* type_ = solve_type ann in
      let* expr = check expr type_ ctx in
      Ok expr

and infer_int value span =
  let type_ = Type.Int in
  Ok (Tast.Expr.Int { value; span; type_ }, type_)

let binding (ctx : ty_ctx) (b : Cst.binding) :
    (Tast.binding * Type.t, Error.t) result =
  let open Result.Syntax in
  match b with
  | Def { name; expr; span; ann } -> (
      match ann with
      | Some ann ->
          let* type_ = solve_type ann in
          let ctx' = TyCtx.insert name type_ ctx in
          let* expr = check expr type_ ctx' in
          Ok (Tast.Binding.Def { name; expr; span; type_ }, type_)
      | None ->
          let* expr, type_ = infer expr ctx in
          Ok (Tast.Binding.Def { name; expr; span; type_ }, type_))

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
