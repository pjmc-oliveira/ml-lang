open Extensions
module TyCtx = Ctx.Make (String)
module StrSet = Set.Make (String)
module Tast = Syn.Tast
module Cst = Syn.Cst

type ty_ctx = Type.poly TyCtx.t
type constraints = (Type.mono * Type.mono * Error.t option) list
type subst = (string * Type.mono) list

module S = StateResult

type ('a, 'e) t = ('a, int, 'e) S.t

let pure x : ('a, 'e) t = S.pure x
let fail e : ('a, 'e) t = S.fail e
let fresh : (string, 'e) t = fun n -> Ok ("t" ^ string_of_int n, n + 1)

(* Error reportinng helpers *)
let error ?span lines : Error.t =
  { kind = Error.Kind.Solver; location = span; lines }

let unbound_var name span : Error.t =
  error ~span [ Text ("Unbound variable: " ^ name) ]

let expr_not_a_function _expr span : Error.t =
  error ~span [ Text "Cannot a apply to non-function values" ]

let type_mismatch exprected_t actual_t =
  error
    [
      Text "Type mismatch";
      Text ("Expected: " ^ Type.show_mono exprected_t);
      Text ("But got: " ^ Type.show_mono actual_t);
    ]

let type_mismatch_poly exprected_t actual_t =
  error
    [
      Text "Type mismatch";
      Text ("Expected: " ^ Type.show_poly exprected_t);
      Text ("But got: " ^ Type.show_poly actual_t);
    ]

let if_branch_mismatch con_t alt_t span : Error.t =
  error ~span
    [
      Text "If branches must have the same type";
      Text ("then-branch has type: " ^ Type.show_mono con_t);
      Text ("but else-branch has type: " ^ Type.show_mono alt_t);
    ]

let if_condition_not_bool cond_t span : Error.t =
  error ~span
    [
      Text ("Expected if-condition to be type: " ^ Type.show_mono Bool);
      Text ("But got type: " ^ Type.show_mono cond_t);
    ]

let assert_equal exprected_t actual_t : (unit, Error.t) t =
  match (exprected_t, actual_t) with
  | _, _ when exprected_t = actual_t -> pure ()
  | _, _ -> fail (type_mismatch exprected_t actual_t)

(* Resolve syntax types *)
let rec solve_type (ty : Cst.ty) : (Type.mono, Error.t) t =
  let open S.Syntax in
  match ty with
  | TCon (_, name) -> (
      match name with
      | "Int" -> pure Type.Int
      | "Bool" -> pure Type.Bool
      | _ -> fail (error [ Text ("Unbound type: " ^ name) ]))
  | TVar (_, name) -> pure (Type.Var name)
  | TArr (_, from, to_) ->
      let* from = solve_type from in
      let* to_ = solve_type to_ in
      pure (Type.Arrow (from, to_))

let solve_scheme (ty : Cst.scheme) : (Type.poly, Error.t) t =
  let open S.Syntax in
  match ty with
  | TForall (_, ty_vars, ty) ->
      let* ty = solve_type ty in
      pure (Type.Poly (ty_vars, ty))
  | TMono ty ->
      let* ty = solve_type ty in
      pure (Type.Mono ty)

(* Constraint solvers *)
let rec occurs_check (name : string) (ty : Type.mono) : (unit, Error.t) t =
  let open S.Syntax in
  match ty with
  | Var name' when name = name' ->
      fail (error [ Text ("Failed occurs check: " ^ name) ])
  | Int | Bool | Var _ -> pure ()
  | Arrow (from, to_) ->
      let* _ = occurs_check name from in
      let* _ = occurs_check name to_ in
      pure ()

let rec apply_subst (old : string) (new_ : Type.mono) (ty : Type.mono) :
    Type.mono =
  match ty with
  | Var name when name = old -> new_
  | Int | Bool | Var _ -> ty
  | Arrow (from, to_) ->
      let from = apply_subst old new_ from in
      let to_ = apply_subst old new_ to_ in
      Arrow (from, to_)

let apply_subst_to_constraints (name : string) (ty : Type.mono)
    (cs : constraints) : constraints =
  let rewrite = apply_subst name ty in
  List.map (fun (l, r, e) -> (rewrite l, rewrite r, e)) cs

let rec solve_constraints (cs : constraints) : (subst, Error.t) t =
  let open S.Syntax in
  match cs with
  | [] -> pure []
  | (ty, ty', e) :: cs' -> (
      match (ty, ty') with
      | Int, Int | Bool, Bool -> solve_constraints cs'
      | Var name, Var name' when name = name' -> solve_constraints cs'
      | Var name, ty | ty, Var name ->
          let* _ = occurs_check name ty in
          let cs' = apply_subst_to_constraints name ty cs' in
          let* subs = solve_constraints cs' in
          pure ((name, ty) :: subs)
      | Arrow (t1, t2), Arrow (t1', t2') ->
          solve_constraints ((t1, t1', None) :: (t2, t2', None) :: cs')
      | ty, ty' -> (
          match e with
          | None ->
              fail
                (error
                   [
                     Text
                       ("Cannot solve constraint: " ^ Type.show_mono ty ^ " = "
                      ^ Type.show_mono ty');
                   ])
          | Some e -> fail e))

let rec apply (ss : subst) (ty : Type.mono) : Type.mono =
  match ss with
  | [] -> ty
  | (name, new_) :: ss' -> (
      match ty with
      | Var name' when name = name' -> apply ss' new_
      | Int | Bool | Var _ -> apply ss' ty
      | Arrow (from, to_) ->
          let from = apply ss from in
          let to_ = apply ss to_ in
          apply ss' (Arrow (from, to_)))

let rec free_ty_vars (ty : Type.mono) : StrSet.t =
  match ty with
  | Type.Int | Type.Bool -> StrSet.empty
  | Type.Var name -> StrSet.singleton name
  | Type.Arrow (from, to_) ->
      let from = free_ty_vars from in
      let to_ = free_ty_vars to_ in
      StrSet.union from to_

let generalize (type_ : Type.mono) : Type.poly =
  let ty_vars = List.of_seq (StrSet.to_seq (free_ty_vars type_)) in
  match ty_vars with
  | [] -> Type.Mono type_
  | _ ->
      let count = ref 0 in
      let subst =
        List.map
          (fun old_name ->
            let new_name = "t" ^ string_of_int !count in
            count := !count + 1;
            (old_name, new_name, Type.Var new_name))
          ty_vars
      in
      let new_ty_vars = List.map (fun (_, name, _) -> name) subst in
      let type_ =
        apply (List.map (fun (name, _, ty) -> (name, ty)) subst) type_
      in
      Type.Poly (new_ty_vars, type_)

let rec instantiate (ty : Type.poly) : (Type.mono, Error.t) t =
  let open S.Syntax in
  match ty with
  | Mono ty -> pure ty
  | Poly (ty_vars, ty) -> (
      match ty_vars with
      | [] -> pure ty
      | ty_var :: ty_vars' ->
          let* name = fresh in
          let new_var = Type.Var name in
          instantiate (Type.Poly (ty_vars', apply_subst ty_var new_var ty)))

let unify_scheme (original : Type.poly) (inferred : Type.poly) :
    (Type.poly, Error.t) t =
  let open S.Syntax in
  match (original, inferred) with
  | Mono ty, Mono ty' ->
      let* _ = assert_equal ty ty' in
      pure (Type.Mono ty)
  | Poly (ty_vars, ty), Poly (ty_vars', ty') ->
      let* _ =
        S.guard
          (List.length ty_vars = List.length ty_vars')
          (type_mismatch_poly original inferred)
      in
      let subst =
        List.zip ty_vars' ty_vars
        |> List.map (fun (inf, og) -> (inf, Type.Var og))
      in
      let ty'' = apply subst ty' in
      print_endline (Type.show_poly original);
      print_endline (Type.show_mono ty'');
      let* _ = S.guard (ty = ty'') (type_mismatch_poly original inferred) in
      pure original
  | Poly (_, _), Mono _ -> fail (type_mismatch_poly original inferred)
  | Mono _, Poly (_, _) -> failwith "TODO unify_scheme Mono Poly"

(* Inference *)

let rec infer (e : Cst.expr) (ctx : ty_ctx) :
    (Tast.expr * Type.mono * constraints, Error.t) t =
  let open S.Syntax in
  match e with
  | ELit (span, Int value) ->
      pure (Tast.ELit ((Int, span), Int value), Type.Int, [])
  | ELit (span, Bool value) ->
      pure (Tast.ELit ((Bool, span), Bool value), Type.Bool, [])
  | EVar (span, name) -> (
      match TyCtx.lookup name ctx with
      | None -> fail (unbound_var name span)
      | Some ty ->
          let* ty = instantiate ty in
          pure (Tast.EVar ((ty, span), name), ty, []))
  | ELet ((span, ann), name, def, body) ->
      let* def, ctx', c1 =
        match ann with
        | None ->
            let* tvar = fresh in
            let ann = Type.Var tvar in
            let ctx' = TyCtx.insert name (Type.Mono ann) ctx in
            let* def, def_t, c1 = infer def ctx' in
            let ctx'' = TyCtx.insert name (Type.Mono def_t) ctx' in
            pure (def, ctx'', (ann, def_t, None) :: c1)
        | Some ann ->
            let* ann = solve_type ann in
            let ctx' = TyCtx.insert name (Type.Mono ann) ctx in
            let* def, def_t, c1 = infer def ctx' in
            let ctx'' = TyCtx.insert name (Type.Mono def_t) ctx' in
            pure (def, ctx'', (ann, def_t, None) :: c1)
      in
      let* body, body_t, c2 = infer body ctx' in
      pure (Tast.ELet ((body_t, span), name, def, body), body_t, c1 @ c2)
  | EIf (span, cond, con, alt) ->
      let* cond, cond_t, c1 = infer cond ctx in
      let* con, con_t, c2 = infer con ctx in
      let* alt, alt_t, c3 = infer alt ctx in
      pure
        ( Tast.EIf ((con_t, span), cond, con, alt),
          con_t,
          (cond_t, Type.Bool, Some (if_condition_not_bool cond_t span))
          :: (con_t, alt_t, Some (if_branch_mismatch con_t alt_t span))
          :: c1
          @ c2 @ c3 )
  | ELam ((span, None), param, body) ->
      let* tvar = fresh in
      let param_t = Type.Var tvar in
      let ctx' = TyCtx.insert param (Type.Mono param_t) ctx in
      let* body, body_t, c1 = infer body ctx' in
      pure
        ( Tast.ELam ((param_t, body_t, span), param, body),
          Type.Arrow (param_t, body_t),
          c1 )
  | ELam ((span, Some ann), param, body) ->
      let* param_t = solve_type ann in
      let ctx' = TyCtx.insert param (Type.Mono param_t) ctx in
      let* body, body_t, c1 = infer body ctx' in
      pure
        ( Tast.ELam ((param_t, body_t, span), param, body),
          Type.Arrow (param_t, body_t),
          c1 )
  | EApp (span, func, arg) ->
      let* func, func_t, c1 = infer func ctx in
      let* arg, arg_t, c2 = infer arg ctx in
      let* tvar = fresh in
      let out_t = Type.Var tvar in
      pure
        ( Tast.EApp ((out_t, span), func, arg),
          out_t,
          ( func_t,
            Type.Arrow (arg_t, out_t),
            Some (expr_not_a_function func span) )
          :: c1
          @ c2 )
  | EExt (`Ann (_span, expr, ann)) ->
      let* ann = solve_type ann in
      let* expr, ty, c1 = infer expr ctx in
      pure (expr, ty, (ann, ty, Some (type_mismatch ann ty)) :: c1)

let binding (ctx : ty_ctx) (b : Cst.bind) :
    (Tast.bind * Type.poly * constraints, Error.t) t =
  let open S.Syntax in
  match b with
  | Def ((span, ann), name, expr) ->
      let* expr, ty, ann, c =
        match ann with
        | None ->
            let* tvar = fresh in
            let ann = Type.Var tvar in
            let ctx' = TyCtx.insert name (Type.Mono ann) ctx in
            let* expr, ty, c = infer expr ctx' in
            pure (expr, ty, None, c)
        | Some ann ->
            let* ann = solve_scheme ann in
            let ctx' = TyCtx.insert name ann ctx in
            let* expr, ty, c = infer expr ctx' in
            pure (expr, ty, Some ann, (Type.get_mono_type ann, ty, None) :: c)
      in
      let* sub = solve_constraints c in
      let ty = generalize (apply sub ty) in
      let* ty =
        match ann with
        | None -> pure ty
        | Some ann -> unify_scheme ann ty
      in
      pure (Tast.Def ((ty, span), name, expr), ty, c)

let rec multiple_passes (ctx : ty_ctx) (constraints : constraints)
    (previous : int) (bindings : Cst.bind list) :
    (Tast.bind list * constraints, Error.t list) t =
  let open S.Syntax in
  let rec loop cs oks errs bs ctx =
    match bs with
    | [] ->
        if errs = [] then
          pure (List.rev oks, cs)
        else
          let current = List.length errs in
          if current < previous then
            multiple_passes ctx cs current bindings
          else
            fail errs
    | b :: bs' -> (
        let* st = S.get in
        match binding ctx b st with
        | Ok (((Def (_x, name, _expr) as ok), ty, c), st') ->
            let ctx' = TyCtx.insert name ty ctx in
            let* _ = S.set st' in
            loop (c @ cs) (ok :: oks) errs bs' ctx'
        | Error err -> loop cs oks (err :: errs) bs' ctx)
  in
  loop constraints [] [] bindings ctx

let module_ (m : Cst.modu) (ctx : ty_ctx) : (Tast.modu, Error.t list) result =
  match m with
  | Module (span, name, bindings) -> (
      match multiple_passes ctx [] (List.length bindings) bindings 0 with
      | Ok ((bindings, _constraints), _st) ->
          let generalize_binding (Tast.Def ((ty, span), name, expr)) =
            Tast.Def ((ty, span), name, expr)
          in
          let bindings = List.map generalize_binding bindings in
          Ok (Tast.Module (span, name, bindings))
      | Error errs -> Error errs)

let solve_module (m : Cst.modu) (ctx : ty_ctx) : (ty_ctx, Error.t list) result =
  let open Result.Syntax in
  let insert_to_ctx ctx (name, ty) = TyCtx.insert name ty ctx in
  let type_of_binding b =
    match b with
    | Tast.Def ((type_, _), name, _) -> (name, type_)
  in
  let* m = module_ m ctx in
  match m with
  | Module (_span, _name, bindings) ->
      let bs = List.map type_of_binding bindings in
      let ctx = List.fold_left insert_to_ctx ctx bs in
      Ok ctx
