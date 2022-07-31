open Extensions
module TyCtx = Ctx.Make (String)
module StrSet = Set.Make (String)

type ty_ctx = Type.poly TyCtx.t
type subst = (string * Type.mono) list

module S = StateResult

type ('a, 'e) t = ('a, int, 'e) S.t

let pure x : ('a, 'e) t = S.pure x
let fail e : ('a, 'e) t = S.fail e
let fresh : (string, 'e) t = fun n -> Ok ("t" ^ string_of_int n, n + 1)

let fresh_var =
  S.Syntax.(
    let* name = fresh in
    pure (Type.Var name))

(* Error reportinng helpers *)
let error ?span lines : Error.t =
  { kind = Error.Kind.Solver; location = span; lines }

let unbound_var name span : Error.t =
  error ~span [ Text ("Unbound variable: " ^ name) ]

module Ctx : sig
  type t

  val empty : t
  val of_list : (string * Type.poly) list -> t
  val to_list : t -> (string * Type.poly) list
  val insert : string -> Type.poly -> t -> t
  val lookup : string -> t -> Type.poly option
  val remove : string -> t -> t
  val union : t -> t -> t
  val map : (Type.poly -> Type.poly) -> t -> t
end = struct
  module String_map = Map.Make (String)

  type t = Type.poly String_map.t

  let empty = String_map.empty
  let of_list ls = String_map.of_seq (List.to_seq ls)
  let to_list ctx = List.of_seq (String_map.to_seq ctx)
  let insert = String_map.add
  let lookup = String_map.find_opt
  let remove = String_map.remove
  let union ls rs = String_map.union (fun _ l _ -> Some l) ls rs
  let map = String_map.map
end

module Subst : sig
  type t

  val empty : t
  val singleton : string -> Type.mono -> t
  val of_list : (string * Type.mono) list -> t
  val to_string : t -> string
  val compose : t -> t -> t
  val apply : t -> Type.mono -> Type.mono
  val apply_scheme : t -> Type.poly -> Type.poly
  val apply_ctx : t -> Ctx.t -> Ctx.t
  val ( <+> ) : t -> t -> t
end = struct
  module String_map = Map.Make (String)

  type t = Type.mono String_map.t

  let empty = String_map.empty
  let singleton = String_map.singleton
  let of_list ls = String_map.of_seq (List.to_seq ls)

  let to_string s =
    "{"
    ^ String.concat "; "
        List.(
          map
            (fun (var, ty) -> var ^ " -> " ^ Type.show_mono ty)
            (of_seq (String_map.to_seq s)))
    ^ "}"

  let rec apply s : Type.mono -> Type.mono = function
    | Var n -> (
        match String_map.find_opt n s with
        | None -> Var n
        | Some t -> t)
    | Arrow (t1, t2) -> Arrow (apply s t1, apply s t2)
    | t -> t

  let apply_scheme s : Type.poly -> Type.poly = function
    | Poly (vars, t) ->
        Poly (vars, apply (List.fold_right String_map.remove vars s) t)

  let apply_ctx s ctx = Ctx.map (apply_scheme s) ctx

  let compose s1 s2 =
    let union ls rs = String_map.(fold add ls rs) in
    union (String_map.map (apply s1) s2) s1

  let ( <+> ) = compose
end

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
      pure (Type.Poly ([], ty))

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

let instantiate (ty : Type.poly) : (Type.mono, Error.t) t =
  let open S.Syntax in
  let ty_vars, ty =
    match ty with
    | Poly (ty_vars, ty) -> (ty_vars, ty)
  in
  let* fresh_vars = S.traverse_list (fun _ -> fresh_var) ty_vars in
  let s = Subst.of_list (List.zip ty_vars fresh_vars) in
  pure (Subst.apply s ty)

(* Algorithm W *)
let bind_var name ty =
  if Type.Var name = ty then
    pure Subst.empty
  else
    let ty_vars = free_ty_vars ty in
    if StrSet.mem name ty_vars then
      fail (error [ Text ("Failed occurs check: " ^ name) ])
    else
      pure (Subst.singleton name ty)

let rec unify (ty : Type.mono) (ty' : Type.mono) =
  let open S.Syntax in
  match (ty, ty') with
  | Arrow (l, r), Arrow (l', r') ->
      let* s1 = unify l l' in
      let r, r' = (Subst.apply s1 r, Subst.apply s1 r') in
      let* s2 = unify (Subst.apply s1 r) (Subst.apply s1 r') in
      pure (Subst.compose s1 s2)
  | Var name, ty | ty, Var name -> bind_var name ty
  | Int, Int | Bool, Bool -> pure Subst.empty
  | _, _ ->
      fail
        (error
           [
             Text
               ("Cannot unify " ^ Type.show_mono ty ^ " with "
              ^ Type.show_mono ty');
           ])

let unify_scheme (original : Type.poly) (inferred : Type.poly) :
    (Subst.t, Error.t) t =
  let open S.Syntax in
  match (original, inferred) with
  | Poly ([], ty), Poly ([], ty') -> unify ty ty'
  | Poly (_, _), Poly (_, _) ->
      let* ty = instantiate original in
      let* ty' = instantiate inferred in
      unify ty ty'

(* Inference *)

let rec infer (e : Cst.expr) (ctx : Ctx.t) :
    (Tast.expr * Type.mono * Subst.t, Error.t) t =
  let open S.Syntax in
  match e with
  | ELit (span, Int value) ->
      pure (Tast.ELit (Int, span, Int value), Type.Int, Subst.empty)
  | ELit (span, Bool value) ->
      pure (Tast.ELit (Bool, span, Bool value), Type.Bool, Subst.empty)
  | EVar (span, name) -> (
      match Ctx.lookup name ctx with
      | None -> fail (unbound_var name span)
      | Some ty ->
          let* ty = instantiate ty in
          pure (Tast.EVar (ty, span, name), ty, Subst.empty))
  | ELet (span, name, None, def, body) ->
      (* TODO: is it necessary to remove itself from ctx? *)
      let ctx' = Ctx.remove name ctx in
      let* tvar = fresh in
      let ann_t = Type.Var tvar in
      let ctx'' = Ctx.insert name (Type.mono ann_t) ctx' in
      let* def, def_t, s1 = infer def ctx'' in
      (* TODO: is this needed? *)
      let def_t = Subst.(apply (singleton tvar def_t) def_t) in
      (* Don't generalize let-bindings *)
      let ctx''' = Ctx.insert name (Type.mono def_t) ctx'' in
      let* body, body_t, s2 = infer body (Subst.apply_ctx s1 ctx''') in
      pure (Tast.ELet (body_t, span, name, def, body), body_t, Subst.(s1 <+> s2))
  | ELet (span, name, Some ann_t, def, body) ->
      (* TODO: is it necessary to remove itself from ctx? *)
      let ctx' = Ctx.remove name ctx in
      let* ann_t = solve_type ann_t in
      let ctx'' = Ctx.insert name (Type.mono ann_t) ctx' in
      let* def, def_t, s1 = infer def ctx'' in
      let* s2 = unify ann_t def_t in
      (* Don't generalize let-bindings *)
      let ctx''' = Ctx.insert name (Type.mono def_t) ctx'' in
      let* body, body_t, s3 = infer body (Subst.apply_ctx s1 ctx''') in
      pure
        ( Tast.ELet (body_t, span, name, def, body),
          body_t,
          Subst.(s1 <+> s2 <+> s3) )
  | EIf (span, cond, con, alt) ->
      let* cond, cond_t, s1 = infer cond ctx in
      let* s2 = unify cond_t Type.Bool in
      let* con, con_t, s3 = infer con ctx in
      let* alt, alt_t, s4 = infer alt ctx in
      let* s5 = unify con_t alt_t in
      pure
        ( Tast.EIf (con_t, span, cond, con, alt),
          con_t,
          Subst.(s1 <+> s2 <+> s3 <+> s4 <+> s5) )
  | ELam (span, param, None, body) ->
      let* param_t = fresh_var in
      (* TODO: should remove param from ctx first? does it matter? *)
      let ctx' = Ctx.insert param (Type.mono param_t) ctx in
      let* body, body_t, s1 = infer body ctx' in
      let param_t = Subst.apply s1 param_t in
      pure
        ( Tast.ELam (param_t, body_t, span, param, body),
          Type.Arrow (param_t, body_t),
          s1 )
  | ELam (span, param, Some param_t, body) ->
      let* param_t = solve_type param_t in
      let ctx' = Ctx.insert param (Type.mono param_t) ctx in
      let* body, body_t, s1 = infer body ctx' in
      pure
        ( Tast.ELam (param_t, body_t, span, param, body),
          Type.Arrow (param_t, body_t),
          s1 )
  | EApp (span, func, arg) ->
      let* out_t = fresh_var in
      let* func, func_t, s1 = infer func ctx in
      let* arg, arg_t, s2 = infer arg (Subst.apply_ctx s1 ctx) in
      let* s3 = unify (Subst.apply s2 func_t) (Arrow (arg_t, out_t)) in
      let out_t = Subst.apply s3 out_t in
      pure (Tast.EApp (out_t, span, func, arg), out_t, Subst.(s1 <+> s2 <+> s3))
  | EAnn (_span, expr, ann_t) ->
      let* ann_t = solve_type ann_t in
      let* expr, ty, s1 = infer expr ctx in
      let* s2 = unify ann_t ty in
      pure (expr, ty, Subst.(s1 <+> s2))

let binding (ctx : Ctx.t) (b : Cst.bind) :
    (Tast.bind * Type.poly * Subst.t, Error.t) t =
  let open S.Syntax in
  match b with
  | Def (span, name, None, expr) ->
      let ctx' = Ctx.remove name ctx in
      let* ann_t = fresh_var in
      let ctx'' = Ctx.insert name (Type.mono ann_t) ctx' in
      let* expr, expr_t, s1 = infer expr ctx'' in
      let* s2 = unify (Subst.apply s1 ann_t) expr_t in
      let ty = generalize expr_t in
      pure (Tast.Def (ty, span, name, expr), ty, Subst.(s1 <+> s2))
  | Def (span, name, Some ann_t, expr) ->
      let* ann_t = solve_scheme ann_t in
      let ctx' = Ctx.remove name ctx in
      let ctx'' = Ctx.insert name ann_t ctx' in
      let* expr, expr_t, s1 = infer expr ctx'' in
      let ty = generalize expr_t in
      (* let* s2 = unify (ann_t) expr_t in *)
      pure (Tast.Def (ty, span, name, expr), ty, s1)

let infer_binding ctx (b : Cst.bind) =
  let open S.Syntax in
  match b with
  | Def (span, name, None, expr) ->
      let ctx' = Ctx.remove name ctx in
      let* ann_t = fresh_var in
      let ctx'' = Ctx.insert name (Type.mono ann_t) ctx' in
      let* expr, expr_t, s1 = infer expr ctx'' in
      let* s2 = unify (Subst.apply s1 ann_t) expr_t in
      let ty = Subst.apply s2 expr_t in
      (* let ty = generalize expr_t in *)
      pure (ty, span, name, expr, Subst.(s1 <+> s2))
  | Def (span, name, Some ann_t, expr) ->
      let* ann_t = solve_scheme ann_t in
      let ctx' = Ctx.remove name ctx in
      let ctx'' = Ctx.insert name ann_t ctx' in
      let* expr, expr_t, s1 = infer expr ctx'' in
      (* let ty = generalize expr_t in *)
      (* let* s2 = unify (ann_t) expr_t in *)
      pure (expr_t, span, name, expr, s1)

let infer_bindings (ctx : Ctx.t) (bindings : Cst.bind list) :
    (Tast.bind list * Ctx.t, Error.t list) t =
  let open S.Syntax in
  let* top_level =
    bindings
    |> S.traverse_list (fun (Cst.Def (_, name, ann, _)) ->
           match ann with
           | None ->
               let* ty = fresh_var in
               pure (name, Type.mono ty)
           | Some ann ->
               let* ty = solve_scheme ann in
               pure (name, ty))
    |> S.map_error List.singleton
  in
  let top_level = Ctx.of_list top_level in
  let rec infer_all ctx oks errs subs :
      Cst.bind list ->
      ( (Type.mono * Source.span * string * Tast.expr) list * Ctx.t * Subst.t,
        Error.t list )
      t = function
    | [] ->
        if errs = [] then
          pure (List.rev oks, ctx, subs)
        else
          fail (List.rev errs)
    | b :: bs -> (
        fun s ->
          match infer_binding ctx b s with
          | Error err -> infer_all ctx oks (err :: errs) subs bs s
          | Ok ((ty, span, name, expr, new_sub), s') ->
              let make_sub =
                match Ctx.lookup name top_level with
                | None -> pure Subst.empty
                | Some original ->
                    S.map_error List.singleton
                      (unify_scheme original (Type.mono ty))
              in
              let new_sub', s'' =
                match make_sub s' with
                | Ok (new_sub, s'') -> (new_sub, s'')
                | Error _ -> (Subst.empty, s')
              in
              let new_sub = Subst.(new_sub' <+> new_sub) in
              let ctx' = Subst.apply_ctx new_sub ctx in
              let ctx'' = Ctx.insert name (Type.mono ty) ctx' in
              infer_all ctx''
                ((ty, span, name, expr) :: oks)
                errs
                Subst.(new_sub <+> subs)
                bs s'')
  in
  let rec generalize_all bs ctx sub =
    match bs with
    | [] -> pure ([], ctx)
    | (ty, span, name, expr) :: bs ->
        let ty = Subst.apply sub ty in
        let scheme = generalize ty in
        let ctx = Ctx.insert name scheme ctx in
        let* bs, ctx' = generalize_all bs ctx sub in
        pure (Tast.Def (scheme, span, name, expr) :: bs, ctx')
  in
  let* inferred, ctx', subst =
    infer_all (Ctx.union top_level ctx) [] [] Subst.empty bindings
  in
  let substitions =
    inferred
    |> List.filter_map (fun (ty, _, name, _) ->
           match Ctx.lookup name top_level with
           | Some Type.(Poly ([], Var tvar)) -> Some (tvar, ty)
           | _ -> None)
    |> fun l ->
    List.fold_right
      (fun (tvar, ty) s -> Subst.(singleton tvar ty <+> s))
      l Subst.empty
  in
  generalize_all inferred ctx' Subst.(subst <+> substitions)

let module_ (m : Cst.modu) (ctx : ty_ctx) : (Tast.modu, Error.t list) result =
  match m with
  | Module (span, name, bindings) -> (
      match infer_bindings (Ctx.of_list (TyCtx.to_list ctx)) bindings 0 with
      | Ok ((bindings, _ctx), _st) -> Ok (Tast.Module (span, name, bindings))
      | Error errs -> Error errs)

let solve_module (m : Cst.modu) (ctx : ty_ctx) : (ty_ctx, Error.t list) result =
  let open Result.Syntax in
  let insert_to_ctx ctx (name, ty) = TyCtx.insert name ty ctx in
  let type_of_binding b =
    match b with
    | Tast.Def (type_, _, name, _) -> (name, type_)
  in
  let* m = module_ m ctx in
  match m with
  | Module (_span, _name, bindings) ->
      let bs = List.map type_of_binding bindings in
      let ctx = List.fold_left insert_to_ctx ctx bs in
      Ok ctx
