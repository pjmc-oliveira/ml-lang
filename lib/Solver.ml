open Extensions
module TyCtx = Ctx.Make (String)
module StrSet = Set.Make (String)
module Cst = Syn.Cst
module Tast = Syn.Tast

type ty_ctx = Type.poly TyCtx.t

module type S = sig
  val module_ : Cst.modu -> ty_ctx -> (Tast.modu, Error.t list) result
  val solve_module : Cst.modu -> ty_ctx -> (ty_ctx, Error.t list) result
end

type constraints = (Type.mono * Type.mono * Error.t option) list
type subst = (string * Type.mono) list

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
      Text ("Expected: " ^ Type.show_mono expected_t);
      Text ("But got: " ^ Type.show_mono actual_t);
    ]

let expr_not_a_function _expr span : Error.t =
  error ~span [ Text "Cannot a apply to non-function values" ]

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

let rec apply_subst (old : string) (new_ : Type.mono) (ty : Type.mono) :
    Type.mono =
  match ty with
  | Var name when name = old -> new_
  | Int | Bool | Var _ -> ty
  | Arrow (from, to_) ->
      let from = apply_subst old new_ from in
      let to_ = apply_subst old new_ to_ in
      Arrow (from, to_)

let rec instantiate (ty : Type.poly) : (Type.mono, Error.t) t =
  let open S.Syntax in
  match ty with
  | Poly (ty_vars, ty) -> (
      match ty_vars with
      | [] -> pure ty
      | ty_var :: ty_vars' ->
          let* name = fresh in
          let new_var = Type.Var name in
          instantiate (Type.Poly (ty_vars', apply_subst ty_var new_var ty)))

let rec compare_type (ty : Type.mono) (ty' : Type.mono) =
  match (ty, ty') with
  | _, _ when ty = ty' -> true
  | Arrow (from, to_), Arrow (from', to') ->
      compare_type from from' && compare_type to_ to'
  | _, _ -> false

let assert_equal ?span exprected_t actual_t : (constraints, Error.t) t =
  let err =
    error ?span
      [
        Text "Type mismatch";
        Text ("Expected: " ^ Type.show_mono exprected_t);
        Text ("But got: " ^ Type.show_mono actual_t);
      ]
  in
  match (exprected_t, actual_t) with
  | _, _ when compare_type exprected_t actual_t -> pure []
  (* Type variables may be equal to each once they are instantiated *)
  | Type.Var _, _ | _, Type.Var _ -> pure [ (exprected_t, actual_t, Some err) ]
  | _, _ -> fail err

let rec solve_type (ty : Cst.ty) : (Type.mono, Error.t) t =
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
      pure (Type.mono ty)

let rec occurs_check (name : string) (ty : Type.mono) : (unit, Error.t) t =
  match ty with
  | Var name' when name = name' ->
      fail (error [ Text ("Failed occurs check: " ^ name) ])
  | Int | Bool | Var _ -> pure ()
  | Arrow (from, to_) ->
      let* _ = occurs_check name from in
      let* _ = occurs_check name to_ in
      pure ()

let apply_subst_to_constraints (name : string) (ty : Type.mono)
    (cs : constraints) : constraints =
  let rewrite = apply_subst name ty in
  List.map (fun (l, r, e) -> (rewrite l, rewrite r, e)) cs

let rec apply_subst (ss : subst) (ty : Type.mono) : Type.mono =
  match ss with
  | [] -> ty
  | (name, new_) :: ss' -> (
      match ty with
      | Var name' when name = name' -> apply_subst ss' new_
      | Int | Bool | Var _ -> apply_subst ss' ty
      | Arrow (from, to_) ->
          let from = apply_subst ss from in
          let to_ = apply_subst ss to_ in
          apply_subst ss' (Arrow (from, to_)))

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
  | [] -> Type.mono type_
  | _ -> Type.Poly (ty_vars, type_)

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
      | Arrow (t1, t2), Arrow (t1', t2') ->
          solve_constraints ((t1, t1', err) :: (t2, t2', err) :: cs')
      | ty, ty' -> (
          match err with
          | None ->
              fail
                (error
                   [
                     Text
                       ("Cannot solve constraint: " ^ Type.show_mono ty ^ " = "
                      ^ Type.show_mono ty');
                   ])
          | Some err -> fail err))

let rec infer (e : Cst.expr) (ctx : ty_ctx) :
    (Tast.expr * Type.mono * constraints, Error.t) t =
  let open Tast in
  match e with
  | ELit (span, Int value) ->
      let type_ = Type.Int in
      pure (ELit ((type_, span), Int value), type_, [])
  | ELit (span, Bool value) ->
      let type_ = Type.Bool in
      pure (ELit ((type_, span), Bool value), type_, [])
  | EVar (span, name) -> (
      match TyCtx.lookup name ctx with
      | Some type_ ->
          let* type_ = instantiate type_ in
          pure (EVar ((type_, span), name), type_, [])
      | None -> fail (unbound_var name span))
  | ELet ((span, def_t), name, def, body) ->
      let* def, def_t, c1 = infer_check_let name def def_t ctx in
      let ctx' = TyCtx.insert name (Type.mono def_t) ctx in
      let* body, type_, c2 = infer body ctx' in
      pure (ELet ((type_, span), name, def, body), type_, c1 @ c2)
  | EIf (span, cond, con, alt) ->
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
          ( EIf ((con_t, span), cond, con, alt),
            con_t,
            ((cond_t, Type.Bool, None) :: (con_t, alt_t, None) :: c1) @ c2 @ c3
          )
      else
        fail (if_branch_mismatch con_t alt_t span)
  | ELam ((span, param_t), param, body) -> (
      match param_t with
      | None ->
          let* name = fresh in
          let param_t = Type.Var name in
          let ctx' = TyCtx.insert param (Type.mono param_t) ctx in
          let* body, type_, c1 = infer body ctx' in
          (* TODO: should this be type_? type_ vs body_t *)
          pure
            ( ELam ((param_t, type_, span), param, body),
              Type.Arrow (param_t, type_),
              c1 )
      | Some param_t ->
          let* param_t = solve_type param_t in
          let ctx' = TyCtx.insert param (Type.mono param_t) ctx in
          let* body, type_, c1 = infer body ctx' in
          (* TODO type vs body_t *)
          pure
            ( ELam ((param_t, type_, span), param, body),
              Type.Arrow (param_t, type_),
              c1 ))
  | EApp (span, func, arg) -> (
      let* func, func_t, c1 = infer func ctx in
      match func_t with
      | Arrow (param_t, type_) -> (
          let* arg, arg_t, c2 = infer arg ctx in
          match param_t with
          | _ when param_t = arg_t ->
              pure (EApp ((type_, span), func, arg), type_, c1 @ c2)
          | _ ->
              let err = wrong_arg_type param_t arg_t span in
              pure
                ( EApp ((type_, span), func, arg),
                  type_,
                  ((arg_t, param_t, Some err) :: c1) @ c2 ))
      | _ -> fail (expr_not_a_function func span))
  | EExt (`Ann (_, expr, ann)) ->
      let* type_ = solve_type ann in
      let* expr, c1 = check expr type_ ctx in
      pure (expr, type_, c1)

and check (e : Cst.expr) (expected_t : Type.mono) (ctx : ty_ctx) :
    (Tast.expr * constraints, Error.t) t =
  let open Tast in
  match e with
  | ELit (span, Int value) ->
      let* _ = assert_equal expected_t Int in
      pure (ELit ((Int, span), Int value), [])
  | ELit (span, Bool value) ->
      let* _ = assert_equal expected_t Bool in
      pure (ELit ((Bool, span), Bool value), [])
  | EVar (span, name) -> (
      match TyCtx.lookup name ctx with
      | Some type_ ->
          let* type_ = instantiate type_ in
          let* _ = assert_equal expected_t type_ in
          pure (EVar ((type_, span), name), [])
      | None -> fail (unbound_var name span))
  | ELet ((span, def_t), name, def, body) ->
      let* def, def_t, c1 = infer_check_let name def def_t ctx in
      let ctx' = TyCtx.insert name (Type.mono def_t) ctx in
      let* body, c2 = check body expected_t ctx' in
      pure (ELet ((expected_t, span), name, def, body), c1 @ c2)
  | EIf (span, cond, con, alt) ->
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
        ( EIf ((expected_t, span), cond, con, alt),
          ((cond_t, Type.Bool, None) :: c1) @ c2 @ c3 )
  | ELam ((span, param_t), param, body) -> (
      match expected_t with
      | Arrow (from, to_) -> (
          match param_t with
          | Some param_t ->
              let* param_t = solve_type param_t in
              let* c1 = assert_equal from param_t in
              let ctx' = TyCtx.insert param (Type.mono param_t) ctx in
              let* body, c2 = check body to_ ctx' in
              pure (ELam ((param_t, expected_t, span), param, body), c1 @ c2)
          | None ->
              let ctx' = TyCtx.insert param (Type.mono from) ctx in
              let* body, c1 = check body to_ ctx' in
              pure (ELam ((from, expected_t, span), param, body), c1))
      | _ ->
          (* TODO: Better error message? This will always fail... *)
          let* expr, actual_t, c1 = infer e ctx in
          let* c2 =
            print_endline "check lam not arrow";
            assert_equal expected_t actual_t
          in
          pure (expr, c1 @ c2))
  | EApp (span, func, arg) ->
      let* arg, arg_t, c1 = infer arg ctx in
      let* func, c2 = check func (Arrow (arg_t, expected_t)) ctx in
      pure (EApp ((expected_t, span), func, arg), c1 @ c2)
  | EExt (`Ann (_, expr, ann)) ->
      let* type_ = solve_type ann in
      let* expr, c1 = check expr type_ ctx in
      pure (expr, c1)

and infer_check_let (name : string) (expr : Cst.expr) (ty : Cst.ty option)
    (ctx : ty_ctx) : (Tast.expr * Type.mono * constraints, Error.t) t =
  match ty with
  | None -> infer expr ctx
  | Some ty ->
      let* ty = solve_type ty in
      let ctx' = TyCtx.insert name (Type.mono ty) ctx in
      let* expr, c1 = check expr ty ctx' in
      pure (expr, ty, c1)

let binding (ctx : ty_ctx) (b : Cst.bind) : (Tast.bind * Type.poly, Error.t) t =
  let open Tast in
  match b with
  | Def ((span, ann), name, expr) -> (
      match ann with
      | Some ann ->
          let* type_ = solve_scheme ann in
          let ctx' = TyCtx.insert name type_ ctx in
          let* expr, _c1 = check expr (Type.get_mono_type type_) ctx' in
          (* TODO: Solve constraints *)
          pure (Def ((type_, span), name, expr), type_)
      | None ->
          let* expr, type_, c1 = infer expr ctx in
          let* s = solve_constraints c1 in
          let type_ = apply_subst s type_ in
          let type_ = generalize type_ in
          pure (Def ((type_, span), name, expr), type_))

let rec multiple_passes (previous : int) (bindings : Cst.bind list)
    (ctx : ty_ctx) : (Tast.bind list, Error.t list) t =
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
        | Ok (((Def (_x, name, _expr) as tast), ty), _s) ->
            let ctx' = TyCtx.insert name ty ctx in
            loop errs (tast :: oks) bs' ctx'
        | Error e -> loop ((b, e) :: errs) oks bs' ctx)
  in
  loop [] [] bindings ctx

let module_ (m : Cst.modu) (ctx : ty_ctx) : (Tast.modu, Error.t list) result =
  let open Tast in
  match m with
  | Module (span, name, bindings) -> (
      match multiple_passes (List.length bindings) bindings ctx 0 with
      | Ok (bindings, _s) -> Ok (Module (span, name, bindings))
      | Error es -> Error es)

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
