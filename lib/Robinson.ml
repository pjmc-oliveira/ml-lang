open Extensions
module TyCtx = Ctx.Make (String)
module StrSet = Set.Make (String)
module Tast = Syn.Tast
module Cst = Syn.Cst

type ty_ctx = Type.poly TyCtx.t
type constraints = (Type.mono * Type.mono) list
type subst = (string * Type.mono) list

module S = StateResult

type 'a t = ('a, int, Error.t list) S.t

let pure : 'a -> 'a t = S.pure
let fail e : 'a t = S.fail e
let fresh : string t = fun n -> Ok ("t" ^ string_of_int n, n + 1)

let fresh_var =
  S.Syntax.(
    let* name = fresh in
    pure (Type.Var name))

let error ?span lines : Error.t =
  { kind = Error.Kind.Solver; location = span; lines }

let unbound_var name span : Error.t =
  error ~span [ Text ("Unbound variable: " ^ name) ]

(*  *)
let rec solve_type (ty : Cst.ty) : Type.mono t =
  let open S.Syntax in
  match ty with
  | TCon (_, name) -> (
      match name with
      | "Int" -> pure Type.Int
      | "Bool" -> pure Type.Bool
      | _ -> fail [ error [ Text ("Unbound type: " ^ name) ] ])
  | TVar (_, name) -> pure (Type.Var name)
  | TArr (_, from, to_) ->
      let* from = solve_type from in
      let* to_ = solve_type to_ in
      pure (Type.Arrow (from, to_))

let solve_scheme (ty : Cst.scheme) : Type.poly t =
  let open S.Syntax in
  match ty with
  | TForall (_, ty_vars, ty) ->
      let* ty = solve_type ty in
      pure (Type.Poly (ty_vars, ty))
  | TMono ty ->
      let* ty = solve_type ty in
      pure (Type.Poly ([], ty))

(*  *)
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
  | _ ->
      (* TODO: Fix this *)
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

let instantiate (ty : Type.poly) : Type.mono t =
  let open S.Syntax in
  let ty_vars, ty =
    match ty with
    | Poly (ty_vars, ty) -> (ty_vars, ty)
  in
  let* fresh_vars = S.traverse_list (fun _ -> fresh_var) ty_vars in
  let s = List.zip ty_vars fresh_vars in
  pure (apply s ty)

(*  *)

let rec substitute (old : string) (new_ : Type.mono) (ty : Type.mono) :
    Type.mono =
  match ty with
  | Var name when name = old -> new_
  | Int | Bool | Var _ -> ty
  | Arrow (from, to_) ->
      let from = substitute old new_ from in
      let to_ = substitute old new_ to_ in
      Arrow (from, to_)

let substitute_constraints (name : string) (ty : Type.mono) (cs : constraints) :
    constraints =
  let rewrite = substitute name ty in
  List.map (fun (l, r) -> (rewrite l, rewrite r)) cs

let rec occurs_check (name : string) (ty : Type.mono) : unit t =
  let open S.Syntax in
  match ty with
  | Var name' when name = name' ->
      fail [ error [ Text ("Failed occurs check: " ^ name) ] ]
  | Int | Bool | Var _ -> pure ()
  | Arrow (from, to_) ->
      let* _ = occurs_check name from in
      let* _ = occurs_check name to_ in
      pure ()

let rec solve_constraints : constraints -> subst t = function
  | [] -> pure []
  | (ty, ty') :: cs -> (
      let open S.Syntax in
      match (ty, ty') with
      | Int, Int | Bool, Bool -> solve_constraints cs
      | Var tv, Var tv' when tv = tv' -> solve_constraints cs
      | Var tv, ty | ty, Var tv ->
          let* _ = occurs_check tv ty in
          let cs = substitute_constraints tv ty cs in
          let* subs = solve_constraints cs in
          pure ((tv, ty) :: subs)
      | Arrow (t1, t2), Arrow (t1', t2') ->
          solve_constraints ((t1, t1') :: (t2, t2') :: cs)
      | _, _ ->
          fail
            [
              error
                [
                  Text
                    ("Cannot solve constraint: " ^ Type.show_mono ty ^ " = "
                   ^ Type.show_mono ty');
                ];
            ])

(*  *)

let rec infer (ctx : ty_ctx) :
    Cst.expr -> (Type.mono * Tast.expr * constraints) t =
  let open S.Syntax in
  function
  | ELit (span, Int value) ->
      pure (Type.Int, Tast.ELit ((Int, span), Int value), [])
  | ELit (span, Bool value) ->
      pure (Type.Bool, Tast.ELit ((Bool, span), Bool value), [])
  | EVar (span, name) -> (
      match TyCtx.lookup name ctx with
      | None -> fail [ unbound_var name span ]
      | Some ty ->
          let* ty = instantiate ty in
          pure (ty, Tast.EVar ((ty, span), name), []))
  | ELet ((span, None), name, def, body) ->
      let* expr_t = fresh_var in
      let ctx' = TyCtx.insert name (Type.mono expr_t) ctx in
      let* def_t, def, c1 = infer ctx' def in
      let ctx'' = TyCtx.insert name (Type.mono expr_t) ctx' in
      let* body_t, body, c2 = infer ctx'' body in
      pure
        ( body_t,
          Tast.ELet ((def_t, span), name, def, body),
          ((expr_t, def_t) :: c1) @ c2 )
  | ELet ((span, Some ann), name, def, body) ->
      let* expr_t = solve_type ann in
      let ctx' = TyCtx.insert name (Type.mono expr_t) ctx in
      let* def_t, def, c1 = infer ctx' def in
      let ctx'' = TyCtx.insert name (Type.mono expr_t) ctx' in
      let* body_t, body, c2 = infer ctx'' body in
      pure
        ( body_t,
          Tast.ELet ((def_t, span), name, def, body),
          ((expr_t, def_t) :: c1) @ c2 )
  | EIf (span, cond, con, alt) ->
      let* cond_t, cond, c1 = infer ctx cond in
      let* con_t, con, c2 = infer ctx con in
      let* alt_t, alt, c3 = infer ctx alt in
      pure
        ( con_t,
          Tast.EIf ((con_t, span), cond, con, alt),
          ((cond_t, Type.Bool) :: (con_t, alt_t) :: c1) @ c2 @ c3 )
  | ELam ((span, None), param, body) ->
      let* param_t = fresh_var in
      let ctx' = TyCtx.insert param (Type.mono param_t) ctx in
      let* body_t, body, c1 = infer ctx' body in
      pure
        ( Type.Arrow (param_t, body_t),
          Tast.ELam ((param_t, body_t, span), param, body),
          c1 )
  | ELam ((span, Some param_t), param, body) ->
      let* param_t = solve_type param_t in
      let ctx' = TyCtx.insert param (Type.mono param_t) ctx in
      let* body_t, body, c1 = infer ctx' body in
      pure
        ( Type.Arrow (param_t, body_t),
          Tast.ELam ((param_t, body_t, span), param, body),
          c1 )
  | EApp (span, func, arg) ->
      let* out_t = fresh_var in
      let* func_t, func, c1 = infer ctx func in
      let* arg_t, arg, c2 = infer ctx arg in
      pure
        ( out_t,
          Tast.EApp ((out_t, span), func, arg),
          ((func_t, Type.Arrow (arg_t, out_t)) :: c1) @ c2 )
  | EExt (`Ann (_, expr, ty)) ->
      let* ty = solve_type ty in
      let* expr_t, expr, c1 = infer ctx expr in
      pure (expr_t, expr, (expr_t, ty) :: c1)

let infer_binding ctx :
    Cst.bind -> (Type.mono * Source.span * string * Tast.expr * constraints) t =
  let open S.Syntax in
  function
  | Cst.Def ((span, None), name, expr) ->
      let* expr_t, expr, c1 = infer ctx expr in
      pure (expr_t, span, name, expr, c1)
  | Cst.Def ((span, Some ann), name, expr) ->
      let* expr_t, expr, c1 = infer ctx expr in
      let* scheme = solve_scheme ann in
      let ann_t = Type.get_mono_type scheme in
      pure (expr_t, span, name, expr, (ann_t, expr_t) :: c1)

let infer_bindings (ctx : ty_ctx) (bindings : Cst.bind list) : Tast.bind list t
    =
  let open S.Syntax in
  let* top_level =
    bindings
    |> S.traverse_list (fun (Cst.Def ((_, ann), name, _)) ->
           match ann with
           | None ->
               let* ty = fresh_var in
               pure (name, Type.mono ty)
           | Some ann ->
               let* ty = solve_scheme ann in
               pure (name, ty))
  in
  let top_level = TyCtx.of_list top_level in
  let ctx = TyCtx.union top_level ctx in
  let rec infer_all oks errs cs = function
    | [] -> if errs = [] then pure (List.rev oks, cs) else fail (List.rev errs)
    | b :: bs -> (
        fun s ->
          match infer_binding ctx b s with
          | Error err -> infer_all oks (err @ errs) cs bs s
          | Ok ((ty, span, name, expr, c1), s') ->
              let make_constr =
                match TyCtx.lookup name top_level with
                | None -> pure []
                | Some ty' ->
                    let* ty' = instantiate ty' in
                    pure [ (ty, ty') ]
              in
              let c2, s'' =
                match make_constr s' with
                | Ok (c, s'') -> (c, s'')
                | Error _ -> ([], s')
              in
              infer_all
                ((ty, span, name, expr) :: oks)
                errs
                (c1 @ c2 @ cs)
                bs s'')
  in
  let* inferred, cs = infer_all [] [] [] bindings in
  let* subst = solve_constraints cs in
  let substituted =
    List.map
      (fun (ty, span, name, expr) ->
        let ty = apply subst ty in
        let expr = Tast.map_type (apply subst) expr in
        let ty = generalize ty in
        Tast.Def ((ty, span), name, expr))
      inferred
  in
  pure substituted

let module_ (m : Cst.modu) (ctx : ty_ctx) : (Tast.modu, Error.t list) result =
  let open Result.Syntax in
  match m with
  | Module (span, name, bindings) ->
      let* bindings, _s = infer_bindings ctx bindings 0 in
      Ok (Tast.Module (span, name, bindings))

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