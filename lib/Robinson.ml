open Extensions
module TyCtx = Ctx.Make (String)
module StrMap = Map.Make (String)
module StrSet = Set.Make (String)
module StrSCC = SCC.Make (String)

type ty_ctx = Type.poly TyCtx.t
type constraints = (Type.mono * Type.mono) list
type subst = (string * Type.mono) list

(** The type solver Monad *)
module T = struct
  type 'a t = ty_ctx -> int -> ('a * constraints * int, Error.t list) result

  (* Monad operations *)
  let pure (x : 'a) : 'a t = fun _ctx s -> Ok (x, [], s)
  let fail e : 'a t = fun _ctx _s -> Error e

  let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
   fun ctx s ->
    match p ctx s with
    | Error e -> Error e
    | Ok (x, c1, s') -> (
        match (f x) ctx s' with
        | Error e -> Error e
        | Ok (y, c2, s'') -> Ok (y, c1 @ c2, s''))

  let ( let* ) = bind

  (* Writer operations *)
  let tell c : unit t = fun _ctx s -> Ok ((), c, s)

  (* Reader operations *)
  let ask : ty_ctx t = fun ctx s -> Ok (ctx, [], s)
  let local (f : ty_ctx -> ty_ctx) (p : 'a t) : 'a t = fun ctx s -> p (f ctx) s

  (* State operations *)
  let get : int t = fun _ctx s -> Ok (s, [], s)
  let set s : unit t = fun _ctx _s -> Ok ((), [], s)

  (* Traversable *)
  let traverse_list (f : 'a -> 'b t) (xs : 'a list) : 'b list t =
    List.fold_right
      (fun x r ctx s ->
        match f x ctx s with
        | Error e -> Error e
        | Ok (y, c1, s') -> (
            match r ctx s' with
            | Error e -> Error e
            | Ok (ys, c2, s'') -> Ok (y :: ys, c1 @ c2, s'')))
      xs (pure [])

  let accumulate_list (f : 'a -> 'b t) (xs : 'a list) : 'b list t =
    let rec loop oks errs cs = function
      | [] ->
          if errs = [] then
            let* _ = tell cs in
            pure (List.rev oks)
          else
            fail (List.rev errs)
      | x :: xs -> (
          fun ctx st ->
            match f x ctx st with
            | Error err -> loop oks (err @ errs) cs xs ctx st
            | Ok (ok, c, st') -> loop (ok :: oks) errs (c @ cs) xs ctx st')
    in
    loop [] [] [] xs
end

(** Report errors *)
module Report = struct
  open T

  let error ?span lines : Error.t =
    { kind = Error.Kind.Solver; location = span; lines }

  let unbound_var name span =
    fail [ error ~span [ Text ("Unbound variable: " ^ name) ] ]

  let unbound_type name span =
    fail [ error ~span [ Text ("Unbound type: " ^ name) ] ]

  let cannot_solve_constraint ty ty' =
    fail
      [
        error
          [
            Text
              ("Cannot solve constraint: " ^ Type.show_mono ty ^ " = "
             ^ Type.show_mono ty');
          ];
      ]

  let failed_occurs_check name =
    fail [ error [ Text ("Failed occurs check: " ^ name) ] ]
end

(** Resolve Cst type to Type *)
module Resolve = struct
  open T

  let rec ty : Cst.ty -> Type.mono t = function
    | TCon (span, name) -> (
        match name with
        | "Int" -> pure Type.Int
        | "Bool" -> pure Type.Bool
        | _ -> Report.unbound_type name span)
    | TVar (_, name) -> pure (Type.Var name)
    | TArr (_, from, to_) ->
        let* from = ty from in
        let* to_ = ty to_ in
        pure (Type.Arrow (from, to_))

  let scheme : Cst.scheme -> Type.poly t = function
    | TForall (_, ty_vars, t) ->
        let* t = ty t in
        pure (Type.Poly (ty_vars, t))
    | TMono t ->
        let* t = ty t in
        pure (Type.Poly ([], t))
end

(** The core of the type solving functionality *)
module Core = struct
  open T

  (** Constrains two types to equal each other *)
  let constrain ty ty' = tell [ (ty, ty') ]

  (** Scope a name and type to an operation *)
  let scope name ty p = local (TyCtx.insert name (Type.mono ty)) p

  (** Creates a fresh type variable *)
  let fresh_var =
    let* n = get in
    let tv = "t" ^ string_of_int n in
    let* _ = set (n + 1) in
    pure (Type.Var tv)

  (** Checks if a type variable occurs in a given type *)
  let rec occurs_check (name : string) (ty : Type.mono) : unit t =
    match ty with
    | Var name' when name = name' -> Report.failed_occurs_check name
    | Int | Bool | Var _ -> pure ()
    | Arrow (from, to_) ->
        let* _ = occurs_check name from in
        let* _ = occurs_check name to_ in
        pure ()

  (** Substitutes a type variable for a given type in another type *)
  let rec substitute (old : string) (new_ : Type.mono) (ty : Type.mono) :
      Type.mono =
    match ty with
    | Var name when name = old -> new_
    | Int | Bool | Var _ -> ty
    | Arrow (from, to_) ->
        let from = substitute old new_ from in
        let to_ = substitute old new_ to_ in
        Arrow (from, to_)

  (** Substitutes a type variable for a given type in a set of constraints  *)
  let substitute_constraints (name : string) (ty : Type.mono) (cs : constraints)
      : constraints =
    let rewrite = substitute name ty in
    List.map (fun (l, r) -> (rewrite l, rewrite r)) cs

  (** Solves a set of constraints to set of substitutions to perform *)
  let rec solve_constraints : constraints -> subst t = function
    | [] -> pure []
    | (ty, ty') :: cs -> (
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
        | _, _ -> Report.cannot_solve_constraint ty ty')

  (** Applies a substitution to a type *)
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

  (** Instantiates a polymorphic type to a monomorphic type *)
  let instantiate (ty : Type.poly) : Type.mono t =
    let (Poly (ty_vars, ty)) = ty in
    let* fresh_vars = traverse_list (fun _ -> fresh_var) ty_vars in
    let s = List.zip ty_vars fresh_vars in
    pure (apply s ty)

  (** Gets the free type variables of a type *)
  let rec free_ty_vars : Type.mono -> StrSet.t = function
    | Type.Int | Type.Bool -> StrSet.empty
    | Type.Var name -> StrSet.singleton name
    | Type.Arrow (from, to_) ->
        let from = free_ty_vars from in
        let to_ = free_ty_vars to_ in
        StrSet.union from to_

  (** Gets the free variables of an expression *)
  let rec free_vars : Cst.expr -> StrSet.t = function
    | ELit _ -> StrSet.empty
    | EVar (_, name) -> StrSet.singleton name
    | ELet (_, name, _, def, body) ->
        let def_vars = free_vars def in
        let body_vars = free_vars body in
        StrSet.(filter (fun v -> not (v = name)) (union def_vars body_vars))
    | EIf (_, cond, con, alt) ->
        let cond_vars = free_vars cond in
        let con_vars = free_vars con in
        let alt_vars = free_vars alt in
        StrSet.(union cond_vars (union con_vars alt_vars))
    | ELam (_, param, _, body) ->
        let body_vars = free_vars body in
        StrSet.filter (fun v -> not (v = param)) body_vars
    | EApp (_, func, arg) ->
        let func_vars = free_vars func in
        let arg_vars = free_vars arg in
        StrSet.union func_vars arg_vars
    | EAnn (_, expr, _) -> free_vars expr

  (** Gets the free terms of an expression, removing terms in the ctx *)
  let get_free_terms (ctx : ty_ctx) (bindings : Cst.bind list) =
    let free_terms =
      List.map
        (fun (Cst.Def (_, name, expr)) -> (name, free_vars expr))
        bindings
    in
    let list_of_set s = List.of_seq (StrSet.to_seq s) in
    let free_terms =
      List.map
        (fun (name, vars) ->
          ( name,
            List.filter
              (fun v ->
                match TyCtx.lookup v ctx with
                | None -> true
                | Some _ -> false)
              (list_of_set vars) ))
        free_terms
    in
    StrMap.of_seq (List.to_seq free_terms)

  (** Generalizes a monophorphic type to a polymmorphic type *)
  let generalize (ty : Type.mono) : Type.poly =
    let ty_vars = List.of_seq (StrSet.to_seq (free_ty_vars ty)) in
    Type.Poly (ty_vars, ty)

  (** Normalizes the type variables of a scheme *)
  let normalize_scheme (scheme : Type.poly) : subst * Type.poly =
    let (Poly (ty_vars, ty)) = scheme in
    let _, tv_mapping =
      List.fold_left
        (fun (n, new_vars) old_var ->
          (n + 1, new_vars @ [ (old_var, "t" ^ string_of_int n) ]))
        (0, []) ty_vars
    in
    let subst = List.map (fun (old, new_) -> (old, Type.Var new_)) tv_mapping in
    let new_ty_vars = List.map (fun (_, new_) -> new_) tv_mapping in
    (subst, Type.Poly (new_ty_vars, apply subst ty))
end

(** The engine that drives the Core  *)
module Engine = struct
  open T
  open Tast

  (** Returns a typed AST with its associated type *)
  let with_type expr =
    let ty = Tast.type_of_expr expr in
    pure (ty, expr)

  (** Infers the type of an expression *)
  let rec infer : Cst.expr -> (Type.mono * Tast.expr) t = function
    | ELit (span, Int value) -> with_type (ELit ((Int, span), Int value))
    | ELit (span, Bool value) -> with_type (ELit ((Bool, span), Bool value))
    | EVar (span, name) -> (
        let* ctx = ask in
        match TyCtx.lookup name ctx with
        | None -> Report.unbound_var name span
        | Some ty ->
            let* ty = Core.instantiate ty in
            with_type (EVar ((ty, span), name)))
    | ELet (span, name, ann, def, body) ->
        let* expr_t = Option.fold ~none:Core.fresh_var ~some:Resolve.ty ann in
        Core.scope name expr_t
          (let* def_t, def = infer def in
           Core.scope name def_t
             (let* body_t, body = infer body in
              let* _ = Core.constrain expr_t def_t in
              with_type (ELet ((body_t, span), name, def, body))))
    | EIf (span, cond, con, alt) ->
        let* cond_t, cond = infer cond in
        let* _ = Core.constrain cond_t Bool in
        let* con_t, con = infer con in
        let* alt_t, alt = infer alt in
        let* _ = Core.constrain con_t alt_t in
        with_type (EIf ((con_t, span), cond, con, alt))
    | ELam (span, param, ann, body) ->
        let* param_t = Option.fold ~none:Core.fresh_var ~some:Resolve.ty ann in
        Core.scope param param_t
          (let* body_t, body = infer body in
           with_type (ELam ((param_t, body_t, span), param, body)))
    | EApp (span, func, arg) ->
        let* out_t = Core.fresh_var in
        let* func_t, func = infer func in
        let* arg_t, arg = infer arg in
        let* _ = Core.constrain func_t (Type.Arrow (arg_t, out_t)) in
        with_type (EApp ((out_t, span), func, arg))
    | EAnn (_, expr, ann) ->
        let* ann_t = Resolve.ty ann in
        let* expr_t, expr = infer expr in
        let* _ = Core.constrain expr_t ann_t in
        with_type expr
end

(** Infers the type of a single binding *)
let infer_binding (b : Cst.bind) : Tast.bind T.t =
  let open T in
  let* ctx = ask in
  let make_constr name ty =
    match TyCtx.lookup name ctx with
    | None -> pure ()
    | Some scheme ->
        let* ty' = Core.instantiate scheme in
        Core.constrain ty ty'
  in
  match b with
  | Cst.Def ((span, None), name, expr) ->
      let* expr_t, expr = Engine.infer expr in
      let* _ = make_constr name expr_t in
      let ty = Core.generalize expr_t in
      pure (Tast.Def ((ty, span), name, expr))
  | Cst.Def ((span, Some ann), name, expr) ->
      let* ann_t = Resolve.scheme ann in
      let ann_t = Type.get_mono_type ann_t in
      let* expr_t, expr = Engine.infer expr in
      let* _ = Core.constrain ann_t expr_t in
      let* _ = make_constr name expr_t in
      let ty = Core.generalize expr_t in
      pure (Tast.Def ((ty, span), name, expr))

(** Infers the types of a bind group *)
let infer_bindings (bindings : Cst.bind list) : Tast.bind list T.t =
  let open T in
  let* top_level =
    bindings
    |> traverse_list (fun (Cst.Def ((_, ann), name, _)) ->
           match ann with
           | None ->
               let* ty = Core.fresh_var in
               pure (name, Type.mono ty)
           | Some ann ->
               let* ty = Resolve.scheme ann in
               pure (name, ty))
  in
  let top_level = TyCtx.of_list top_level in
  local (TyCtx.union top_level) (accumulate_list infer_binding bindings)

(** Solves the types of a bind group *)
let solve_binding_group (ctx : ty_ctx) (bindings : Cst.bind list) =
  match infer_bindings bindings ctx 0 with
  | Error es -> Error es
  | Ok (bindings, cs, s) -> (
      match Core.solve_constraints cs ctx s with
      | Error es -> Error es
      | Ok (subst, _, _s') ->
          Ok
            (List.map
               (function
                 | Tast.Def ((ty, span), name, expr) ->
                     let scheme =
                       ty |> Type.get_mono_type |> Core.apply subst
                       |> Core.generalize
                     in
                     let subst', scheme = Core.normalize_scheme scheme in
                     let expr =
                       Tast.map_type (Core.apply (subst @ subst')) expr
                     in
                     Tast.Def ((scheme, span), name, expr))
               bindings))

(** Solves all bindings one strongly connected componenent at a time *)
let solve_bindings (ctx : ty_ctx) (bindings : Cst.bind list) =
  let free_terms = Core.get_free_terms ctx bindings in
  let scc = StrSCC.(run (make free_terms)) in
  let top_level =
    StrMap.of_seq
      (List.to_seq
         (List.map
            (fun (Cst.Def (_, name, _) as bind) -> (name, bind))
            bindings))
  in
  let groups =
    List.map (List.filter_map (fun name -> StrMap.find_opt name top_level)) scc
  in
  let result =
    StateResult.traverse_list
      (fun bs ctx ->
        match solve_binding_group ctx bs with
        | Error e -> Error e
        | Ok bs' ->
            let new_ctx =
              List.fold_left
                (fun ctx (Tast.Def ((ty, _), name, _)) ->
                  TyCtx.insert name ty ctx)
                ctx bs'
            in
            Ok (bs', new_ctx))
      groups ctx
  in
  match result with
  | Error e -> Error e
  | Ok (bind_bind, _s) -> Ok (List.flatten bind_bind)

(** Infers the type of a module *)
let module_ (m : Cst.modu) (ctx : ty_ctx) : (Tast.modu, Error.t list) result =
  let open Result.Syntax in
  match m with
  | Module (span, name, bindings) ->
      let* bindings = solve_bindings ctx bindings in
      Ok (Tast.Module (span, name, bindings))

(** Solves the type of a module *)
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