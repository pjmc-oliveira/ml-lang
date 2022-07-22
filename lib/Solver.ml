open Extensions
module TyCtx = Ctx.Make (String)
module StrSet = Set.Make (String)

type ty_ctx = Type.t TyCtx.t
type ty = Source.span Cst.ty
type expr = Source.span Cst.expr
type binding = Source.span Cst.binding
type module_ = Source.span Cst.module_
type constraints = (Type.t * Type.t * Error.t option) list
type subst = (string * Type.t) list

module S = StateResult
open S.Syntax

type ('a, 'e) t = ('a, int, 'e) S.t

let pure x : ('a, 'e) t = S.pure x
let fail e : ('a, 'e) t = S.fail e
let fresh : (string, 'e) t = fun n -> Ok ("t" ^ string_of_int n, n + 1)

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

let rec apply_subst (old : string) (new_ : Type.t) (ty : Type.t) : Type.t =
  match ty with
  | Var name when name = old -> new_
  | Int | Bool | Var _ -> ty
  | Arrow { from; to_ } ->
      let from = apply_subst old new_ from in
      let to_ = apply_subst old new_ to_ in
      Arrow { from; to_ }
  | Forall _ -> failwith "TODO"

let rec compare_type (ty : Type.t) (ty' : Type.t) =
  match (ty, ty') with
  | _, _ when ty = ty' -> true
  | Arrow { from; to_ }, Arrow { from = from'; to_ = to' } ->
      compare_type from from' && compare_type to_ to'
  | Forall { ty_vars; type_ }, Forall { ty_vars = ty_vars'; type_ = type' } ->
      let zipped = List.zip ty_vars ty_vars' in
      if List.length ty_vars = List.length ty_vars' then
        let s : subst = List.map (fun (v, v') -> (v', Type.Var v)) zipped in
        let type_ =
          List.fold_right (fun (name, new_) -> apply_subst name new_) s type_
        in
        compare_type type_ type'
      else
        false
  | _, _ -> false

let assert_equal ?span exprected_t actual_t : (constraints, Error.t) t =
  let err =
    error ?span
      [
        Text "Type mismatch";
        Text ("Expected: " ^ Type.show exprected_t);
        Text ("But got: " ^ Type.show actual_t);
      ]
  in
  match (exprected_t, actual_t) with
  | _, _ when compare_type exprected_t actual_t -> pure []
  (* Type variables may be equal to each once they are instantiated *)
  | Type.Var _, _ | _, Type.Var _ -> pure [ (exprected_t, actual_t, Some err) ]
  | _, _ -> fail err

let rec solve_type (ty : ty) : (Type.t, Error.t) t =
  match ty with
  | Const { name; _ } -> (
      match name with
      | "Int" -> pure Type.Int
      | "Bool" -> pure Type.Bool
      | _ -> fail (error [ Text ("Unbound type: " ^ name) ]))
  | Var { name; _ } -> pure (Type.Var name)
  | Arrow { from; to_; _ } ->
      let* from = solve_type from in
      let* to_ = solve_type to_ in
      pure (Type.Arrow { from; to_ })
  | Forall { ty_vars; type_; _ } ->
      let* type_ = solve_type type_ in
      pure (Type.Forall { ty_vars; type_ })

let rec occurs_check (name : string) (ty : Type.t) : (unit, Error.t) t =
  match ty with
  | Var name' when name = name' ->
      fail (error [ Text ("Failed occurs check: " ^ name) ])
  | Int | Bool | Var _ -> pure ()
  | Arrow { from; to_ } ->
      let* _ = occurs_check name from in
      let* _ = occurs_check name to_ in
      pure ()
  | Forall _ -> failwith "TODO occurs_check Forall"

let apply_subst_to_constraints (name : string) (ty : Type.t) (cs : constraints)
    : constraints =
  let rewrite = apply_subst name ty in
  List.map (fun (l, r, e) -> (rewrite l, rewrite r, e)) cs

let rec apply_subst (ss : subst) (ty : Type.t) : Type.t =
  match ss with
  | [] -> ty
  | (name, new_) :: ss' -> (
      match ty with
      | Var name' when name = name' -> apply_subst ss' new_
      | Int | Bool | Var _ -> apply_subst ss' ty
      | Arrow { from; to_ } ->
          let from = apply_subst ss from in
          let to_ = apply_subst ss to_ in
          apply_subst ss' (Arrow { from; to_ })
      | Forall _ -> failwith "TODO apply_subst")

let rec free_ty_vars (ty : Type.t) : StrSet.t =
  match ty with
  | Type.Int | Type.Bool -> StrSet.empty
  | Type.Var name -> StrSet.singleton name
  | Type.Arrow { from; to_ } ->
      let from = free_ty_vars from in
      let to_ = free_ty_vars to_ in
      StrSet.union from to_
  | Type.Forall { ty_vars; type_ } ->
      let type_ = free_ty_vars type_ in
      List.fold_right StrSet.remove ty_vars type_

let generalize (type_ : Type.t) : Type.t =
  let ty_vars = List.of_seq (StrSet.to_seq (free_ty_vars type_)) in
  match ty_vars with
  | [] -> type_
  | _ -> Type.Forall { ty_vars; type_ }

let rec solve_constraints (cs : constraints) : (subst, Error.t) t =
  match cs with
  | [] -> pure []
  | (ty, ty', err) :: cs' -> (
      match (ty, ty') with
      | Int, Int | Bool, Bool -> solve_constraints cs'
      | Var name, Var name' when name = name' -> solve_constraints cs'
      | Var name, ty | ty, Var name ->
          let* _ = occurs_check name ty in
          let cs' = apply_subst_to_constraints name ty cs' in
          let* subs = solve_constraints cs' in
          pure ((name, ty) :: subs)
      | Arrow { from = t1; to_ = t2 }, Arrow { from = t1'; to_ = t2' } ->
          solve_constraints ((t1, t1', err) :: (t2, t2', err) :: cs')
      | ty, ty' -> (
          match err with
          | None ->
              fail
                (error
                   [
                     Text
                       ("Cannot solve constraint: " ^ Type.show ty ^ " = "
                      ^ Type.show ty');
                   ])
          | Some err -> fail err))

let rec infer (e : expr) (ctx : ty_ctx) :
    (Source.span Tast.expr * Type.t * constraints, Error.t) t =
  match e with
  | Int { value; span } ->
      let type_ = Type.Int in
      pure (Tast.Expr.Int { value; span; type_ }, type_, [])
  | Bool { value; span } ->
      let type_ = Type.Bool in
      pure (Tast.Expr.Bool { value; span; type_ }, type_, [])
  | Var { name; span } -> (
      match TyCtx.lookup name ctx with
      | Some type_ -> pure (Tast.Expr.Var { name; span; type_ }, type_, [])
      | None -> fail (unbound_var name span))
  | Let { name; def_t; def; body; span } ->
      let* def, def_t, c1 = infer_check_let name def def_t ctx in
      let ctx' = TyCtx.insert name def_t ctx in
      let* body, type_, c2 = infer body ctx' in
      pure
        (Tast.Expr.Let { name; def_t; def; body; span; type_ }, type_, c1 @ c2)
  | If { cond; con; alt; span } ->
      let* cond, cond_t, c1 = infer cond ctx in
      let* _ =
        S.map_error
          (fun _ -> if_condition_not_bool cond_t span)
          (assert_equal Bool cond_t)
      in
      let* con, con_t, c2 = infer con ctx in
      let* alt, alt_t, c3 = infer alt ctx in
      if con_t = alt_t then
        pure
          ( Tast.Expr.If { cond; con; alt; span; type_ = con_t },
            con_t,
            ((cond_t, Type.Bool, None) :: (con_t, alt_t, None) :: c1) @ c2 @ c3
          )
      else
        fail (if_branch_mismatch con_t alt_t span)
  | Lam { param; param_t; body; span } -> (
      match param_t with
      | None ->
          let* name = fresh in
          let param_t = Type.Var name in
          let ctx' = TyCtx.insert param param_t ctx in
          let* body, type_, c1 = infer body ctx' in
          (* TODO: should this be type_? *)
          pure
            ( Tast.Expr.Lam { param; param_t; body; span; type_ },
              Type.Arrow { from = param_t; to_ = type_ },
              c1 )
      | Some param_t ->
          let* param_t = solve_type param_t in
          let ctx' = TyCtx.insert param param_t ctx in
          let* body, type_, c1 = infer body ctx' in
          pure
            ( Tast.Expr.Lam { param; param_t; body; span; type_ },
              Type.Arrow { from = param_t; to_ = type_ },
              c1 ))
  | App { func; arg; span } -> (
      let* func, func_t, c1 = infer func ctx in
      match func_t with
      | Arrow { from = param_t; to_ = type_ } -> (
          let* arg, arg_t, c2 = infer arg ctx in
          match param_t with
          | _ when param_t = arg_t ->
              pure (Tast.Expr.App { func; arg; span; type_ }, type_, c1 @ c2)
          | _ ->
              let err = wrong_arg_type param_t arg_t span in
              pure
                ( Tast.Expr.App { func; arg; span; type_ },
                  type_,
                  ((arg_t, param_t, Some err) :: c1) @ c2 ))
      | Forall
          { ty_vars = _todo; type_ = Arrow { from = param_t; to_ = type_ } } ->
          let* arg, arg_t, c2 = infer arg ctx in
          let err = wrong_arg_type param_t arg_t span in
          pure
            ( Tast.Expr.App { func; arg; span; type_ },
              type_,
              ((param_t, arg_t, Some err) :: c1) @ c2 )
      | _ -> fail (expr_not_a_function func span))
  | Ann { expr; ann; _ } ->
      let* type_ = solve_type ann in
      let* expr, c1 = check expr type_ ctx in
      pure (expr, type_, c1)

and check (e : expr) (expected_t : Type.t) (ctx : ty_ctx) :
    (Source.span Tast.expr * constraints, Error.t) t =
  match e with
  | Int { value; span } ->
      let* _ = assert_equal expected_t Int in
      pure (Tast.Expr.Int { value; span; type_ = Int }, [])
  | Bool { value; span } ->
      let* _ = assert_equal expected_t Bool in
      pure (Tast.Expr.Bool { value; span; type_ = Bool }, [])
  | Var { name; span } -> (
      match TyCtx.lookup name ctx with
      | Some type_ ->
          let* _ = assert_equal expected_t type_ in
          pure (Tast.Expr.Var { name; span; type_ }, [])
      | None -> fail (unbound_var name span))
  | Let { name; def_t; def; body; span } ->
      let* def, def_t, c1 = infer_check_let name def def_t ctx in
      let ctx' = TyCtx.insert name def_t ctx in
      let* body, c2 = check body expected_t ctx' in
      pure
        ( Tast.Expr.Let { name; def_t; def; body; span; type_ = expected_t },
          c1 @ c2 )
  | If { cond; con; alt; span } ->
      (* TODO: Should we check this instead of inferring? *)
      let* cond, cond_t, c1 = infer cond ctx in
      let* _ =
        S.map_error
          (fun _ -> if_condition_not_bool cond_t span)
          (assert_equal Bool cond_t)
      in
      let* con, c2 = check con expected_t ctx in
      let* alt, c3 = check alt expected_t ctx in
      pure
        ( Tast.Expr.If { cond; con; alt; span; type_ = expected_t },
          ((cond_t, Type.Bool, None) :: c1) @ c2 @ c3 )
  | Lam { param; param_t; body; span } -> (
      match expected_t with
      | Arrow { from; to_ } -> (
          match param_t with
          | Some param_t ->
              let* param_t = solve_type param_t in
              let* c1 = assert_equal from param_t in
              let ctx' = TyCtx.insert param param_t ctx in
              let* body, c2 = check body to_ ctx' in
              pure
                ( Tast.Expr.Lam
                    { param; param_t; body; span; type_ = expected_t },
                  c1 @ c2 )
          | None ->
              let ctx' = TyCtx.insert param from ctx in
              let* body, c1 = check body to_ ctx' in
              pure
                ( Tast.Expr.Lam
                    { param; param_t = from; body; span; type_ = expected_t },
                  c1 ))
      | Forall { type_ = Arrow { from; to_ }; _ } -> (
          match param_t with
          | Some param_t ->
              let* param_t = solve_type param_t in
              let* c1 = assert_equal from param_t in
              let ctx' = TyCtx.insert param param_t ctx in
              let* body, c2 = check body to_ ctx' in
              pure
                ( Tast.Expr.Lam
                    { param; param_t = from; body; span; type_ = expected_t },
                  ((from, param_t, None) :: (to_, expected_t, None) :: c1) @ c2
                )
          | None ->
              (* TODO: is this right? *)
              let ctx' = TyCtx.insert param from ctx in
              let* body, c1 = check body to_ ctx' in
              pure
                ( Tast.Expr.Lam
                    { param; param_t = from; body; span; type_ = expected_t },
                  c1 ))
      | _ ->
          (* TODO: Better error message? This will always fail... *)
          let* expr, actual_t, c1 = infer e ctx in
          let* c2 =
            print_endline "check lam not arrow";
            assert_equal expected_t actual_t
          in
          pure (expr, c1 @ c2))
  | App { func; arg; span } ->
      let* arg, arg_t, c1 = infer arg ctx in
      let* func, c2 =
        check func (Arrow { from = arg_t; to_ = expected_t }) ctx
      in
      pure (Tast.Expr.App { func; arg; span; type_ = expected_t }, c1 @ c2)
  | Ann { expr; ann; _ } ->
      let* type_ = solve_type ann in
      let* expr, c1 = check expr type_ ctx in
      pure (expr, c1)

and infer_check_let (name : string) (expr : expr) (ty : ty option)
    (ctx : ty_ctx) : (Source.span Tast.expr * Type.t * constraints, Error.t) t =
  match ty with
  | None -> infer expr ctx
  | Some ty ->
      let* ty = solve_type ty in
      let ctx' = TyCtx.insert name ty ctx in
      let* expr, c1 = check expr ty ctx' in
      pure (expr, ty, c1)

let binding (ctx : ty_ctx) (b : binding) :
    (Source.span Tast.binding * Type.t, Error.t) t =
  match b with
  | Def { name; expr; span; ann } -> (
      match ann with
      | Some ann ->
          let* type_ = solve_type ann in
          let ctx' = TyCtx.insert name type_ ctx in
          let* expr, _c1 = check expr type_ ctx' in
          (* TODO: Solve constraints *)
          pure (Tast.Binding.Def { name; expr; span; type_ }, type_)
      | None ->
          let* expr, type_, c1 = infer expr ctx in
          let* s = solve_constraints c1 in
          let type_ = apply_subst s type_ in
          let type_ = generalize type_ in
          pure (Tast.Binding.Def { name; expr; span; type_ }, type_))

let rec multiple_passes (previous : int) (bindings : binding list)
    (ctx : ty_ctx) : (Source.span Tast.binding list, Error.t list) t =
  let rec loop errs oks bs ctx =
    match bs with
    | [] ->
        if errs = [] then
          pure (List.rev oks)
        else
          let current = List.length errs in
          if current < previous then
            (* TODO: should this skip the successful bindings on the next pass? *)
            multiple_passes current bindings ctx
          else
            let errors = List.map (fun (_, e) -> e) errs in
            fail errors
    | b :: bs' -> (
        let* s = S.get in
        match binding ctx b s with
        | Ok (((Def { name; _ } as tast), ty), _s) ->
            let ctx' = TyCtx.insert name ty ctx in
            loop errs (tast :: oks) bs' ctx'
        | Error e -> loop ((b, e) :: errs) oks bs' ctx)
  in
  loop [] [] bindings ctx

let module_ (m : module_) (ctx : ty_ctx) :
    (Source.span Tast.module_, Error.t list) result =
  match m with
  | Module { name; bindings; span } -> (
      match multiple_passes (List.length bindings) bindings ctx 0 with
      | Ok (bindings, _s) -> Ok (Tast.Module.Module { name; bindings; span })
      | Error es -> Error es)

let solve_module (m : module_) (ctx : ty_ctx) : (ty_ctx, Error.t list) result =
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
