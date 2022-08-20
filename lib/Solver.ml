open Extensions
module Str_map = Map.Make (String)
module Str_set = Set.Make (String)
module Str_scc = Scc.Make (String)
module Cst = Syn.Cst
module Tcst = Syn.Tcst

type constraint_ctx =
  (* Mismatched types *)
  | Mismatch of {
      left : Type.mono * Span.t;
      right : Type.mono * Span.t;
      message : string;
    }
  (* Unexpected type *)
  | Unexpected of {
      got : Type.mono * Span.t;
      message : string;
    }

(** Report errors *)
module Report = struct
  let error ?span lines : Error.t =
    { kind = Error.Kind.Solver; location = span; lines }

  let unbound_var name span =
    error ~span [ Text ("Unbound variable: " ^ name); Quote span ]

  let unbound_constructor name span =
    error ~span [ Text ("Unbound constructor: " ^ name); Quote span ]

  let constructor_arity_mismatch span name expected actual =
    error ~span
      [
        Text ("Arity mismatch for constructor: " ^ name);
        Quote span;
        Text ("Expected " ^ string_of_int expected ^ " variable(s)");
        Text ("But got " ^ string_of_int actual ^ " variable(s)");
      ]

  let unbound_type name span =
    error ~span [ Text ("Unbound type: " ^ name); Quote span ]

  let cannot_solve_type_constraint ty ty' constraint_ctx =
    let detail =
      match constraint_ctx with
      | Mismatch { left = ty, sp; right = ty', sp'; message } ->
          [
            Error.Line.Text ("Got " ^ Type.pretty_mono ty ^ " from:");
            Quote sp;
            Text ("And " ^ Type.pretty_mono ty' ^ " from:");
            Quote sp';
          ]
          @ if message = "" then [] else [ Text message ]
      | Unexpected { got = ty, sp; message } ->
          [
            Text ("Got " ^ Type.pretty_mono ty ^ " from:");
            Quote sp;
            Text message;
          ]
    in

    (* TODO: Should substitute constraints *)
    error
      ([
         Error.Line.Text
           ("Cannot solve constraint: "
           ^ Type.pretty_mono ty
           ^ " = "
           ^ Type.pretty_mono ty');
       ]
      @ detail)

  let cannot_solve_kind_constraint ki ki' =
    error
      [
        Text
          ("Cannot solve kind constraint: "
          ^ Type.pretty_kind ki
          ^ " = "
          ^ Type.pretty_kind ki');
      ]

  let failed_occurs_check name = error [ Text ("Failed occurs check: " ^ name) ]

  let duplicate_definition name spans =
    let spans = List.map (fun s -> Error.Line.Quote s) spans in
    error ([ Error.Line.Text ("Duplicate definitions of: " ^ name) ] @ spans)

  let overlapping_patterns cons spans =
    let quotes = List.map (fun sp -> Error.Line.Quote sp) spans in
    {
      Error.kind = Solver;
      location = None;
      lines = [ Error.Line.Text ("Overlapping patterns: " ^ cons) ] @ quotes;
    }

  let match_not_exhaustive span missing_cons =
    {
      Error.kind = Solver;
      location = Some span;
      lines =
        [
          Text "Match not exhaustive";
          Quote span;
          Text ("Missing patterns: " ^ String.concat ", " missing_cons);
        ];
    }
end

module type Monad = sig
  type 'a t

  val pure : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : Error.t list -> 'a t
  val ask : Ctx.t t
end

(** Resolve Cst type to Type *)
module Resolve (M : Monad) = struct
  open M

  let rec ty : Cst.Type.t -> Type.mono t = function
    | Con (_, "Int") -> pure Type.Int
    | Con (_, "Bool") -> pure Type.Bool
    | Con (span, name) -> (
        let* ctx = ask in
        match Ctx.lookup_ty name ctx with
        | None -> fail [ Report.unbound_type name span ]
        | Some _ -> pure (Type.Con name))
    | Var (_, name) -> pure (Type.Var name)
    | App (_, func, arg) ->
        let* func = ty func in
        let* arg = ty arg in
        pure (Type.App (func, arg))
    | Arr (_, from, to_) ->
        let* from = ty from in
        let* to_ = ty to_ in
        pure (Type.Arrow (from, to_))

  let scheme : Cst.Scheme.t -> Type.poly t = function
    | Forall (_, ty_vars, t) ->
        let* t = ty t in
        pure (Type.Poly (ty_vars, t))
    | Type (_, t) ->
        let* t = ty t in
        pure (Type.Poly ([], t))
end

(** The core of the type solving functionality *)
module Core = struct
  type subst = (string * Type.mono) list

  (** Substitutes a type variable for a given type in another type *)
  let rec substitute (old : string) (new_ : Type.mono) (ty : Type.mono) :
      Type.mono =
    match ty with
    | Var name when name = old -> new_
    | Int | Bool | Var _ | Con _ -> ty
    | App (func, arg) ->
        let func = substitute old new_ func in
        let arg = substitute old new_ arg in
        App (func, arg)
    | Arrow (from, to_) ->
        let from = substitute old new_ from in
        let to_ = substitute old new_ to_ in
        Arrow (from, to_)

  (** Applies a substitution to a type *)
  let rec apply (ss : subst) (ty : Type.mono) : Type.mono =
    match ss with
    | [] -> ty
    | (name, new_) :: ss' -> (
        match ty with
        | Var name' when name = name' -> apply ss' new_
        | Int | Bool | Var _ | Con _ -> apply ss' ty
        | App (func, arg) ->
            let func = substitute name new_ func in
            let arg = substitute name new_ arg in
            apply ss' (App (func, arg))
        | Arrow (from, to_) ->
            let from = substitute name new_ from in
            let to_ = substitute name new_ to_ in
            apply ss' (Arrow (from, to_)))

  (** Generalizes a monomorphic type to a polymorphic type *)
  let generalize (ty : Type.mono) : Type.poly =
    (* Gets the free type variables of a type *)
    let rec free_ty_vars : Type.mono -> Str_set.t = function
      | Type.Int | Type.Bool | Type.Con _ -> Str_set.empty
      | Type.Var name -> Str_set.singleton name
      | App (func, arg) ->
          let func = free_ty_vars func in
          let arg = free_ty_vars arg in
          Str_set.union func arg
      | Type.Arrow (from, to_) ->
          let from = free_ty_vars from in
          let to_ = free_ty_vars to_ in
          Str_set.union from to_
    in
    let ty_vars = List.of_seq (Str_set.to_seq (free_ty_vars ty)) in
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

(** Infer kinds of types *)
module Kinds = struct
  type constraints = (Type.kind * Type.kind) list
  type subst = (string * Type.kind) list

  module M = struct
    type 'a t = Ctx.t -> int -> ('a * constraints * int, Error.t list) result

    let pure x : 'a t = fun _ctx st -> Ok (x, [], st)

    let bind (t : 'a t) (f : 'a -> 'b t) : 'b t =
     fun ctx st ->
      match t ctx st with
      | Error e -> Error e
      | Ok (x, cs, st') -> (
          match (f x) ctx st' with
          | Error e -> Error e
          | Ok (y, cs', st'') -> Ok (y, cs @ cs', st''))

    let ( let* ) = bind
    let fail e : 'a t = fun _ctx _st -> Error e
    let ask : Ctx.t t = fun ctx st -> Ok (ctx, [], st)
    let local (f : Ctx.t -> Ctx.t) (p : 'a t) : 'a t = fun ctx -> p (f ctx)
    let tell c : unit t = fun _ctx s -> Ok ((), c, s)
    let get : int t = fun _ctx s -> Ok (s, [], s)
    let set s : unit t = fun _ctx _s -> Ok ((), [], s)

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

  module Resolve = Resolve (M)

  module Engine = struct
    open M

    let constrain ki ki' = tell [ (ki, ki') ]

    let fresh_var : Type.kind t =
      let* n = get in
      let kind = Type.KVar ("k" ^ string_of_int n) in
      let* _ = set (n + 1) in
      pure kind

    let rec free_types : Cst.Type.t -> Str_set.t = function
      (* TODO: Add primitive types to ctx? *)
      | Con (_, "Int") | Con (_, "Bool") -> Str_set.empty
      | Con (_, name) -> Str_set.singleton name
      | Var (_, name) -> Str_set.singleton name
      | App (_, func, arg) ->
          let func_vars = free_types func in
          let arg_vars = free_types arg in
          Str_set.union func_vars arg_vars
      | Arr (_, from, to_) ->
          let from_vars = free_types from in
          let to_vars = free_types to_ in
          Str_set.union from_vars to_vars

    let get_free_types (ctx : Ctx.t) (ty_defs : Cst.Binding.ty_def list) =
      let types_of_alts (alts : Cst.Binding.alt list) =
        List.fold_left Str_set.union Str_set.empty
          (List.flatten (List.map (fun (_, ty) -> List.map free_types ty) alts))
      in
      let free_types =
        ty_defs
        |> List.map (fun (_, name, tvars, alts) ->
               let tvars = Str_set.of_list tvars in
               let types = types_of_alts alts in
               let free_types =
                 types
                 |> Str_set.filter (fun ty -> not (Str_set.mem ty tvars))
                 |> Str_set.filter (fun ty ->
                        Option.is_none (Ctx.lookup_ty ty ctx))
               in
               (name, List.of_seq (Str_set.to_seq free_types)))
      in

      Str_map.of_seq (List.to_seq free_types)

    let rec occurs_check name : Type.kind -> unit t = function
      | KVar name' when name = name' -> fail [ Report.failed_occurs_check name ]
      | KType | KVar _ -> pure ()
      | KArrow (func, arg) ->
          let* _ = occurs_check name func in
          let* _ = occurs_check name arg in
          pure ()

    let rec substitute name new_ : Type.kind -> Type.kind = function
      | KVar name' when name' = name -> new_
      | (KType | KVar _) as kind -> kind
      | KArrow (func, arg) ->
          let func = substitute name new_ func in
          let arg = substitute name new_ arg in
          KArrow (func, arg)

    let substitute_constraints name new_ (cs : constraints) =
      let rewrite = substitute name new_ in
      List.map (fun (l, r) -> (rewrite l, rewrite r)) cs

    let rec solve_constraints : constraints -> subst t = function
      | [] -> pure []
      | (KType, KType) :: cs -> solve_constraints cs
      | (KVar name, KVar name') :: cs when name = name' -> solve_constraints cs
      | (KVar kv, ki) :: cs | (ki, KVar kv) :: cs ->
          let* _ = occurs_check kv ki in
          let cs = substitute_constraints kv ki cs in
          let* subs = solve_constraints cs in
          pure ((kv, ki) :: subs)
      | (KArrow (k1, k2), KArrow (k1', k2')) :: cs ->
          solve_constraints ((k1, k1') :: (k2, k2') :: cs)
      | (ki, ki') :: _ -> fail [ Report.cannot_solve_kind_constraint ki ki' ]

    let rec apply (ss : subst) (old : Type.kind) : Type.kind =
      match ss with
      | [] -> old
      | (name, new_) :: ss' -> (
          match old with
          | KVar name' when name = name' -> apply ss' new_
          | KType | KVar _ -> apply ss' old
          | KArrow (func, arg) ->
              let func' = substitute name new_ func in
              let arg' = substitute name new_ arg in
              apply ss' (Type.KArrow (func', arg')))
  end

  open M

  (* Types *)

  (** Infer the kind and type of a type definition *)
  let rec infer_ty : Cst.Type.t -> (Type.mono * Type.kind) t = function
    | Con (_, "Int") -> pure (Type.Int, Type.KType)
    | Con (_, "Bool") -> pure (Type.Bool, Type.KType)
    | Con (span, name) -> (
        let* ctx = ask in
        match Ctx.lookup_ty name ctx with
        | None -> fail [ Report.unbound_type name span ]
        | Some kind -> pure (Type.Con name, kind))
    | Var (span, name) -> (
        let* ctx = ask in
        match Ctx.lookup_ty name ctx with
        | None -> fail [ Report.unbound_type name span ]
        | Some kind -> pure (Type.Var name, kind))
    | App (_, func, arg) ->
        let* out_k = Engine.fresh_var in
        let* func_t, func_k = infer_ty func in
        let* arg_t, arg_k = infer_ty arg in
        let* _ = Engine.constrain func_k (KArrow (arg_k, out_k)) in
        pure (Type.App (func_t, arg_t), out_k)
    | Arr (_, param, body) ->
        let* param_t, param_k = infer_ty param in
        let* body_t, body_k = infer_ty body in
        pure (Type.Arrow (param_t, body_t), Type.KArrow (param_k, body_k))

  (** Infer a type definition *)
  let infer_ty_def ((_, name, params, alts) : Cst.Binding.ty_def) =
    let ty =
      List.fold_left (fun ty tv -> Type.App (ty, Var tv)) (Type.Con name) params
    in
    let* ctx = ask in
    let* fresh_kinds =
      traverse_list
        (fun param ->
          let* kind = Engine.fresh_var in
          pure (param, kind))
        params
    in
    let* kind = Engine.fresh_var in
    let _, kinds = List.unzip fresh_kinds in
    let syntax_kind =
      List.fold_left
        (fun kind fresh -> Type.KArrow (fresh, kind))
        Type.KType kinds
    in
    let* _ = Engine.constrain kind syntax_kind in
    let ctx' = Ctx.(union ctx (of_types_list fresh_kinds)) in
    let* constructors =
      local
        (fun _ -> Ctx.insert_ty name kind ctx')
        (accumulate_list
           (fun (con, tys) ->
             let* types_kinds =
               traverse_list
                 (fun ty ->
                   let* ty, kind = infer_ty ty in
                   let* _ = Engine.constrain kind KType in
                   pure (ty, kind))
                 tys
             in
             let types, _ = List.unzip types_kinds in
             let con_t =
               List.fold_right (fun l r -> Type.Arrow (l, r)) types ty
             in
             let _, scheme = Core.normalize_scheme (Core.generalize con_t) in
             pure (con, scheme))
           alts)
    in
    pure (name, kind, constructors)

  (* Bindings *)

  (** Solve a mutually recursive group of type definitions *)
  let solve_ty_def_group (ty_defs : Cst.Binding.ty_def list) :
      Tcst.Binding.ty_def list t =
    let* fresh_kinds =
      traverse_list
        (fun (_, name, _, _) ->
          let* kind = Engine.fresh_var in
          pure (name, kind))
        ty_defs
    in
    let new_types = Ctx.of_types_list fresh_kinds in
    local (Ctx.union new_types)
      (traverse_list
         (fun ((span, _, vars, alts) as ty_def) ->
           let alts =
             List.map
               (fun (con, tys) -> (con, List.map Tcst.type_of_cst tys))
               alts
           in
           let* name, kind, constructors = infer_ty_def ty_def in
           pure ((span, kind, constructors), name, vars, alts))
         ty_defs)

  let check_duplicate_ty_defs (ty_defs : Cst.Binding.ty_def list) =
    let open Result.Syntax in
    let defs = List.map (fun (span, name, _, _) -> (name, span)) ty_defs in
    let def_map =
      List.fold_left
        (fun map (name, span) ->
          match Str_map.find_opt name map with
          | None -> Str_map.add name [ span ] map
          | Some spans -> Str_map.add name (span :: spans) map)
        Str_map.empty defs
    in
    let* _ =
      Result.accumulate_list
        (fun (name, spans) ->
          if List.length spans > 1 then
            Error [ Report.duplicate_definition name spans ]
          else
            Ok ())
        (List.of_seq (Str_map.to_seq def_map))
    in
    Ok ()

  let check_duplicate_constructors (ty_defs : Cst.Binding.ty_def list) =
    let open Result.Syntax in
    let span_cons =
      List.flatten
        (List.map
           (fun (span, _, _, alts) ->
             List.map (fun (con, _) -> (con, span)) alts)
           ty_defs)
    in
    let cons_map =
      List.fold_left
        (fun map (name, span) ->
          match Str_map.find_opt name map with
          | None -> Str_map.add name (1, [ span ]) map
          | Some (n, spans) ->
              Str_map.add name
                (n + 1, span :: List.filter (fun s -> not (s = span)) spans)
                map)
        Str_map.empty span_cons
    in
    let* _ =
      Result.accumulate_list
        (fun (name, (n, spans)) ->
          if n > 1 then
            Error [ Report.duplicate_definition name spans ]
          else
            Ok ())
        (List.of_seq (Str_map.to_seq cons_map))
    in
    Ok ()

  (** Solve all type definitions, one strongly connected component at a time *)
  let solve_ty_defs (ctx : Ctx.t) (ty_defs : Cst.Binding.ty_def list) =
    let open Result.Syntax in
    (* TODO: check_duplicate_* error message should point to specific identifiers *)
    let* _ = check_duplicate_ty_defs ty_defs in
    let* _ = check_duplicate_constructors ty_defs in
    let free_types = Engine.get_free_types ctx ty_defs in
    let scc = Str_scc.(run (make free_types)) in
    let top_level =
      Str_map.of_seq
        (List.to_seq
           (List.map (fun ((_, name, _, _) as ty) -> (name, ty)) ty_defs))
    in
    let groups =
      List.map (List.map (fun name -> Str_map.find name top_level)) scc
    in
    (* TODO: Should we return this? *)
    let* results, _ctx' =
      State_result.accumulate_list
        (fun group ctx ->
          let* ty_defs, cs, s = solve_ty_def_group group ctx 0 in
          let* subst, _cs', _s' = Engine.solve_constraints cs ctx s in
          let ty_defs =
            List.map
              (fun ((span, kind, cons), name, vars, alts) ->
                let kind = Engine.apply subst kind in
                ((name, kind, cons), ((span, kind, cons), name, vars, alts)))
              ty_defs
          in
          let ctx' =
            List.fold_left
              (fun ctx ((name, kind, _), _) -> Ctx.insert_ty name kind ctx)
              ctx ty_defs
          in
          Ok (ty_defs, ctx'))
        groups ctx
    in
    let results = List.flatten results in
    let new_types, ty_defs = List.unzip results in
    let types =
      Ctx.of_types_list
        (List.map (fun (name, kind, _) -> (name, kind)) new_types)
    in
    let new_constructors =
      List.map
        (fun (name, _, alts) ->
          let cons, _ = List.unzip alts in
          (name, cons))
        new_types
    in
    let constructors = Ctx.of_constructors_list new_constructors in
    let constructors_of_ty_def (((_, _, cons), _, _, _) : Tcst.Binding.ty_def) =
      cons
    in
    let constructors_terms =
      Ctx.of_terms_list (List.flatten (List.map constructors_of_ty_def ty_defs))
    in
    Ok (Ctx.(union types (union constructors constructors_terms)), ty_defs)
end

(** Infer types  *)
module Types = struct
  type constraints = (Type.mono * Type.mono * constraint_ctx) list
  type subst = (string * Type.mono) list

  (** The type solver Monad *)
  module M = struct
    type 'a t = Ctx.t -> int -> ('a * constraints * int, Error.t list) result

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
    let ask : Ctx.t t = fun ctx s -> Ok (ctx, [], s)
    let local (f : Ctx.t -> Ctx.t) (p : 'a t) : 'a t = fun ctx s -> p (f ctx) s

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

  module Resolve = Resolve (M)

  (** The engine of the type solving functionality *)
  module Engine = struct
    open M

    (** Constrains two types to equal each other *)
    let constrain ty ty' ?(message = "") (sp, sp') =
      tell
        [ (ty, ty', Mismatch { left = (ty, sp); right = (ty', sp'); message }) ]

    let constrain_app fun_t (arg_t, ret_t) ?(message = "") (sp, sp') =
      tell
        [
          ( fun_t,
            Type.Arrow (arg_t, ret_t),
            Mismatch { left = (fun_t, sp); right = (arg_t, sp'); message } );
        ]

    let expect expected actual span message =
      tell [ (expected, actual, Unexpected { got = (actual, span); message }) ]

    (** Scope a name and type to an operation *)
    let scope name ty p = local (Ctx.insert name (Type.mono ty)) p

    (** Creates a fresh type variable *)
    let fresh_var =
      let* n = get in
      let tv = "t" ^ string_of_int n in
      let* _ = set (n + 1) in
      pure (Type.Var tv)

    (** Checks if a type variable occurs in a given type *)
    let rec occurs_check (name : string) (ty : Type.mono) : unit t =
      match ty with
      | Var name' when name = name' -> fail [ Report.failed_occurs_check name ]
      | Int | Bool | Var _ | Con _ -> pure ()
      | App (func, arg) ->
          let* _ = occurs_check name func in
          let* _ = occurs_check name arg in
          pure ()
      | Arrow (from, to_) ->
          let* _ = occurs_check name from in
          let* _ = occurs_check name to_ in
          pure ()

    (** Substitutes a type variable for a given type in a set of constraints  *)
    let substitute_constraints (name : string) (ty : Type.mono)
        (cs : constraints) : constraints =
      let rewrite = Core.substitute name ty in
      List.map (fun (l, r, e) -> (rewrite l, rewrite r, e)) cs

    let substitute_constraint_ctx (name : string) (new_ : Type.mono) = function
      | Mismatch { left = ty, sp; right = ty', sp'; message } ->
          Mismatch
            {
              left = (Core.substitute name new_ ty, sp);
              right = (Core.substitute name new_ ty', sp');
              message;
            }
      | Unexpected { got = ty', sp'; message } ->
          Unexpected { got = (Core.substitute name new_ ty', sp'); message }

    (** Solves a set of constraints to set of substitutions to perform *)
    let rec solve_constraints : constraints -> subst t = function
      | [] -> pure []
      | (ty, ty', extra) :: cs -> (
          match (ty, ty') with
          | Int, Int | Bool, Bool -> solve_constraints cs
          | Var tv, Var tv' when tv = tv' -> solve_constraints cs
          | Con name, Con name' when name = name' -> solve_constraints cs
          | Var tv, ty | ty, Var tv ->
              let* _ = occurs_check tv ty in
              let cs = substitute_constraints tv ty cs in
              let cs =
                List.map
                  (fun (ty, ty', c_ctx) ->
                    let c_ctx = substitute_constraint_ctx tv ty c_ctx in
                    (ty, ty', c_ctx))
                  cs
              in
              let* subs = solve_constraints cs in
              pure ((tv, ty) :: subs)
          | App (t1, t2), App (t1', t2') ->
              solve_constraints ((t1, t1', extra) :: (t2, t2', extra) :: cs)
          | Arrow (t1, t2), Arrow (t1', t2') ->
              solve_constraints ((t1, t1', extra) :: (t2, t2', extra) :: cs)
          | _, _ -> fail [ Report.cannot_solve_type_constraint ty ty' extra ])

    (** Instantiates a polymorphic type to a monomorphic type *)
    let instantiate (ty : Type.poly) : Type.mono t =
      let (Poly (ty_vars, ty)) = ty in
      let* fresh_vars = traverse_list (fun _ -> fresh_var) ty_vars in
      let s = List.zip ty_vars fresh_vars in
      pure (Core.apply s ty)

    (** Gets the free variables of an expression *)
    let rec free_vars : Cst.Expr.t -> Str_set.t = function
      | Lit _ -> Str_set.empty
      | Var (_, name) -> Str_set.singleton name
      | Let (_, name, _, def, body) | LetRec (_, name, _, def, body) ->
          let def_vars = free_vars def in
          let body_vars = free_vars body in
          Str_set.(filter (fun v -> not (v = name)) (union def_vars body_vars))
      | If (_, cond, con, alt) ->
          let cond_vars = free_vars cond in
          let con_vars = free_vars con in
          let alt_vars = free_vars alt in
          Str_set.(union cond_vars (union con_vars alt_vars))
      | Lam (_, param, _, body) ->
          let body_vars = free_vars body in
          Str_set.filter (fun v -> not (v = param)) body_vars
      | App (_, func, arg) ->
          let func_vars = free_vars func in
          let arg_vars = free_vars arg in
          Str_set.union func_vars arg_vars
      | Ann (_, expr, _) -> free_vars expr
      | Match (_, expr, alts) ->
          let expr_vars = free_vars expr in
          let alts_vars =
            Non_empty.fold_left
              (fun s (Cst.Pat.Con (_, _, vars), case) ->
                let vars = List.map (fun (v, _) -> v) vars in
                let case = free_vars case in
                let alt_vars =
                  List.fold_left (fun s v -> Str_set.remove v s) case vars
                in
                Str_set.union s alt_vars)
              Str_set.empty alts
          in
          Str_set.union expr_vars alts_vars

    (** Gets the free terms of an expression, removing terms in the ctx *)
    let get_free_terms (ctx : Ctx.t) (defs : Cst.Binding.tm_def list) =
      let free_terms =
        List.map (fun (_, name, _, expr) -> (name, free_vars expr)) defs
      in
      let list_of_set s = List.of_seq (Str_set.to_seq s) in
      let free_terms =
        List.map
          (fun (name, vars) ->
            ( name,
              List.filter
                (fun v ->
                  match Ctx.lookup v ctx with
                  | None -> true
                  | Some _ -> false)
                (list_of_set vars) ))
          free_terms
      in
      Str_map.of_seq (List.to_seq free_terms)
  end

  open M

  (** Returns a typed AST with its associated type *)
  let with_type expr =
    let ty = Tcst.type_of_expr expr in
    pure (ty, expr)

  (* Expressions *)

  (** Infers the type of an expression *)
  let rec infer : Cst.Expr.t -> (Type.mono * Tcst.Expr.t) t = function
    | Lit (span, Int (_, value)) ->
        with_type (Tcst.Expr.Lit ((span, Int), Tcst.Lit.Int (span, value)))
    | Lit (span, Bool (_, value)) ->
        with_type (Lit ((span, Bool), Bool (span, value)))
    | Var (span, name) -> infer_var span name
    | Let (span, name, ann, def, body) | LetRec (span, name, ann, def, body) ->
        infer_let span name ann def body
    | If (span, cond, con, alt) -> infer_if span cond con alt
    | Lam (span, param, ann, body) -> infer_lam span param ann body
    | App (span, func, arg) -> infer_app span func arg
    | Ann (_, expr, ann) -> infer_ann expr ann
    | Match (span, expr, alts) -> infer_match span expr alts

  and infer_var span name =
    let* ctx = ask in
    match Ctx.lookup name ctx with
    | None -> fail [ Report.unbound_var name span ]
    | Some ty ->
        let* ty = Engine.instantiate ty in
        with_type (Tcst.Expr.Var ((span, ty), name))

  and infer_let span name ann def body =
    let free_vars = Engine.free_vars def in
    let* expr_t = Option.fold ~none:Engine.fresh_var ~some:Resolve.ty ann in
    (* Only create recursive "let" if "name" is present in "def"  *)
    if Str_set.mem name free_vars then
      Engine.scope name expr_t
        (let* def_t, def = infer def in
         Engine.scope name def_t
           (let* body_t, body = infer body in
            let* _ =
              Engine.constrain expr_t def_t (span, Tcst.span_of_expr def)
            in
            with_type
              (LetRec
                 ( (span, body_t),
                   name,
                   Option.map Tcst.type_of_cst ann,
                   def,
                   body ))))
    else
      let* def_t, def = infer def in
      Engine.scope name def_t
        (let* body_t, body = infer body in
         let* _ = Engine.constrain expr_t def_t (span, Tcst.span_of_expr def) in
         with_type
           (Let
              ((span, body_t), name, Option.map Tcst.type_of_cst ann, def, body)))

  and infer_if span cond con alt =
    let* cond_t, cond = infer cond in
    let* _ =
      Engine.expect Bool cond_t
        (Tcst.span_of_expr cond (* TODO: Fix this *))
        "But if-condition must be Bool"
    in
    let* con_t, con = infer con in
    let* alt_t, alt = infer alt in
    let* _ =
      Engine.constrain con_t alt_t
        ~message:"If branches must have the same type"
        (Tcst.span_of_expr con, Tcst.span_of_expr alt)
    in
    with_type (If ((span, con_t), cond, con, alt))

  and infer_lam span param ann body =
    let* param_t = Option.fold ~none:Engine.fresh_var ~some:Resolve.ty ann in
    Engine.scope param param_t
      (let* body_t, body = infer body in
       with_type
         (Lam
            ( (span, param_t, body_t),
              param,
              Option.map Tcst.type_of_cst ann,
              body )))

  and infer_app span func arg =
    let* out_t = Engine.fresh_var in
    let* func_t, func = infer func in
    let* arg_t, arg = infer arg in
    let* _ =
      Engine.constrain_app func_t (arg_t, out_t)
        (Tcst.span_of_expr func, Tcst.span_of_expr arg (* TODO: Fix this *))
    in
    with_type (App ((span, out_t), func, arg))

  and infer_ann expr ann =
    let* ann_t = Resolve.ty ann in
    let* expr_t, expr = infer expr in
    let* _ =
      Engine.constrain expr_t ann_t (Tcst.span_of_expr expr, Cst.Type.get_x ann)
    in
    with_type
      (Ann ((Tcst.span_of_expr expr, expr_t), expr, Tcst.type_of_cst ann))

  and infer_match span expr alts =
    let* expr_t, expr = infer expr in
    let ((_, alt_expr) as alt), alts = (Non_empty.hd alts, Non_empty.tl alts) in
    let* alt_t, alt = infer_alt ~expr_t ~expr alt in
    (* Constrains each case to match the type of the first alternative *)
    let case_constrain case_t case_sp =
      Engine.constrain alt_t case_t (Cst.span_of_expr alt_expr, case_sp)
    in
    let* alts = traverse_list (infer_alt ~expr_t ~expr ~case_constrain) alts in
    let _, alts = List.unzip alts in
    with_type (Match ((span, alt_t), expr, Non_empty.make alt alts))

  and infer_alt ~expr_t ~expr
      ?(case_constrain = fun _case_t _case_sp -> pure ())
      (Cst.Pat.Con (p_span, (head, h_span), spanned_vars), case) :
      (Type.mono * (Tcst.Pat.t * Tcst.Expr.t)) t =
    let* ctx = ask in
    let vars, _var_spans = List.unzip spanned_vars in
    match Ctx.lookup head ctx with
    | None -> fail [ Report.unbound_constructor head h_span ]
    | Some scheme ->
        let* ty = Engine.instantiate scheme in
        let arity = Type.get_arity ty in
        if not (arity = List.length vars) then
          fail
            [
              Report.constructor_arity_mismatch p_span head arity
                (List.length vars);
            ]
        else
          let head_t = Type.final_type ty in
          let* _ =
            Engine.constrain expr_t head_t (Tcst.span_of_expr expr, h_span)
          in
          let ctx' =
            Ctx.of_terms_list
              (List.zip vars (List.map Type.mono (Type.split_arrow ty)))
          in
          local (Ctx.union ctx')
            (let* case_t, case = infer case in
             let* _ = case_constrain case_t (Tcst.span_of_expr case) in
             pure
               ( case_t,
                 (Tcst.Pat.Con (p_span, (head, h_span), spanned_vars), case) ))

  (* Secondary checks *)

  (** Check if match is exhaustive *)
  let check_match_exhaustiveness ctx :
      Tcst.Expr.t -> (Tcst.Expr.t, Error.t list) result =
    Tcst.Expr.fold_result
      (let open Result.Syntax in
      function
      | Tcst.Expr.MatchF ((span, _), expr, alts) as e ->
          let ty = Tcst.type_of_expr expr in
          (* TODO: Sort out error message *)
          let* ty_con = Option.to_result ~none:[] (Type.get_con ty) in
          let* cons =
            (* TODO: Sort out error message *)
            Option.to_result ~none:[] (Ctx.lookup_constructors ty_con ctx)
          in
          let pats, _ = List.unzip (Non_empty.to_list alts) in
          let spanned_heads =
            List.map (fun (Tcst.Pat.Con (_, head, _)) -> head) pats
          in
          let heads = List.map (fun (head, _) -> head) spanned_heads in
          let missing_cons =
            List.filter (fun con -> not (List.mem con heads)) cons
          in
          if List.length missing_cons > 0 then
            Error [ Report.match_not_exhaustive span missing_cons ]
          else
            let duplicate_heads =
              List.fold_left
                (fun m (h, sp) ->
                  Str_map.add h
                    (Option.fold ~none:[ sp ]
                       ~some:(fun sps -> sps @ [ sp ])
                       (Str_map.find_opt h m))
                    m)
                Str_map.empty spanned_heads
              |> Str_map.to_seq
              |> List.of_seq
              |> List.filter (fun (_, sps) -> List.length sps > 1)
            in
            if List.length heads > List.length cons then
              Error
                (List.map
                   (fun (head, spans) -> Report.overlapping_patterns head spans)
                   duplicate_heads)
            else
              Ok (Tcst.Expr.f_to_t e)
      | e -> Ok (Tcst.Expr.f_to_t e))

  (* Bindings *)

  (** Infers the type of a single binding *)
  let infer_def (def : Cst.Binding.tm_def) : Tcst.Binding.tm_def t =
    let* ctx = ask in
    let scheme_to_tcst : Cst.Scheme.t -> Tcst.Scheme.t = function
      | Type (x, ty) -> Type (x, Tcst.type_of_cst ty)
      | Forall (x, vars, ty) -> Forall (x, vars, Tcst.type_of_cst ty)
    in
    let make_constr name ty sp sp' =
      match Ctx.lookup name ctx with
      | None -> pure ()
      | Some scheme ->
          let* ty' = Engine.instantiate scheme in
          Engine.constrain ty ty' (sp, sp')
    in
    match def with
    | span, name, None, expr ->
        let* expr_t, expr = infer expr in
        let* _ = make_constr name expr_t (Tcst.span_of_expr expr) span in
        let scheme = Core.generalize expr_t in
        pure ((span, scheme), name, None, expr)
    | span, name, Some ann, expr ->
        let* ann_t = Resolve.scheme ann in
        let ann_t = Type.get_mono_type ann_t in
        let* expr_t, expr = infer expr in
        let* _ =
          Engine.constrain ann_t expr_t
            (span (* TODO: Fix this *), Tcst.span_of_expr expr)
        in
        let* _ = make_constr name expr_t (Tcst.span_of_expr expr) span in
        let scheme = Core.generalize expr_t in
        pure ((span, scheme), name, Some (scheme_to_tcst ann), expr)

  (** Infers the types of a bind group *)
  let infer_defs (defs : Cst.Binding.tm_def list) : Tcst.Binding.tm_def list t =
    let* top_level =
      defs
      |> traverse_list (function _, name, ann, _ ->
             (match ann with
             | None ->
                 let* ty = Engine.fresh_var in
                 pure (name, Type.mono ty)
             | Some ann ->
                 let* ty = Resolve.scheme ann in
                 pure (name, ty)))
    in
    let top_level = Ctx.of_terms_list top_level in
    local (Ctx.union top_level) (accumulate_list infer_def defs)

  (** Solves the types of a bind group *)
  let solve_def_group (ctx : Ctx.t) (defs : Cst.Binding.tm_def list) =
    let open Result.Syntax in
    match infer_defs defs ctx 0 with
    | Error es -> Error es
    | Ok (defs, cs, s) -> (
        match Engine.solve_constraints cs ctx s with
        | Error es -> Error es
        | Ok (subst, _, _s') ->
            Result.traverse_list
              (function
                | (span, scheme), name, ann, expr ->
                    let scheme =
                      scheme
                      |> Type.get_mono_type
                      |> Core.apply subst
                      |> Core.generalize
                    in
                    (* Substitute type, then normalize scheme *)
                    let expr = Tcst.map_type (Core.apply subst) expr in
                    let subst', scheme = Core.normalize_scheme scheme in
                    let expr = Tcst.map_type (Core.apply subst') expr in
                    let* expr = check_match_exhaustiveness ctx expr in
                    Ok ((span, scheme), name, ann, expr))
              defs)

  let check_duplicate_defs (tm_defs : Cst.Binding.tm_def list) =
    let open Result.Syntax in
    let defs = List.map (fun (span, name, _, _) -> (name, span)) tm_defs in
    let def_map =
      List.fold_left
        (fun map (name, span) ->
          match Str_map.find_opt name map with
          | None -> Str_map.add name [ span ] map
          | Some spans -> Str_map.add name (span :: spans) map)
        Str_map.empty defs
    in
    let* _ =
      Result.accumulate_list
        (fun (name, spans) ->
          if List.length spans > 1 then
            Error [ Report.duplicate_definition name spans ]
          else
            Ok ())
        (List.of_seq (Str_map.to_seq def_map))
    in
    Ok ()

  (** Solves all bindings one strongly connected component at a time *)
  let solve_defs (ctx : Ctx.t) (defs : Cst.Binding.tm_def list) =
    let open Result.Syntax in
    let* _ = check_duplicate_defs defs in
    let free_terms = Engine.get_free_terms ctx defs in
    let scc = Str_scc.(run (make free_terms)) in
    let top_level =
      Str_map.of_seq
        (List.to_seq
           (* TODO: Why filter map? *)
           (List.filter_map
              (function
                | (_, name, _, _) as bind -> Some (name, bind))
              defs))
    in
    let groups =
      List.map
        (List.filter_map (fun name -> Str_map.find_opt name top_level))
        scc
    in
    let* groups, _s =
      State_result.accumulate_list
        (fun bs ctx ->
          match solve_def_group ctx bs with
          | Error e -> Error e
          | Ok bs' ->
              let new_ctx =
                List.fold_left
                  (fun ctx (((_, scheme), name, _, _) : Tcst.Binding.tm_def) ->
                    Ctx.insert name scheme ctx)
                  ctx bs'
              in
              Ok (bs', new_ctx))
        groups ctx
    in
    Ok (List.flatten groups)
end

(** Infers the type of a module *)
let module_ (m : Cst.Module.t) (ctx : Ctx.t) :
    (Tcst.Module.t, Error.t list) result =
  let open Result.Syntax in
  match m with
  | Module (span, name, bindings) ->
      let defs, tys =
        List.fold_left
          (fun (defs, tys) bind ->
            match bind with
            | Cst.Binding.Def (span, name, scheme, expr) ->
                ((span, name, scheme, expr) :: defs, tys)
            | Cst.Binding.Type (span, name, vars, alts) ->
                (defs, (span, name, vars, alts) :: tys))
          ([], []) bindings
      in
      let* ctx', types = Kinds.solve_ty_defs ctx tys in
      let types' =
        List.map (fun ((_, kind, cons), name, _, _) -> (name, kind, cons)) types
      in
      (* TODO: Precedence? *)
      let ctx'' = Ctx.union ctx' ctx in
      let* terms = Types.solve_defs ctx'' defs in
      let terms' =
        List.map (fun ((_, scheme), name, _, _) -> (name, scheme)) terms
      in
      (* TODO: Refactor this *)
      let bindings =
        List.map (fun x -> Tcst.Binding.Def x) terms
        @ List.map (fun x -> Tcst.Binding.Type x) types
      in
      Ok (Tcst.Module.Module ((span, terms', types'), name, bindings))

(** Solves the type of a module *)
let solve_module (m : Cst.Module.t) (ctx : Ctx.t) : (Ctx.t, Error.t list) result
    =
  let open Result.Syntax in
  let insert_term_to_ctx ctx (name, ty) = Ctx.insert name ty ctx in
  let insert_type_to_ctx ctx (name, kind) = Ctx.insert_ty name kind ctx in
  let type_of_term tm_def =
    match tm_def with
    | name, scheme -> (name, scheme)
  in
  let kind_of_type ty_def =
    match ty_def with
    | name, kind, _ -> (name, kind)
  in
  let types_of_constructors ty_def =
    match ty_def with
    | _, _, alts -> alts
  in
  let* m = module_ m ctx in
  match m with
  | Module ((_, terms, types), _, _) ->
      let tms = List.map type_of_term terms in
      let constructors = List.flatten (List.map types_of_constructors types) in
      let tys = List.map kind_of_type types in
      let ctx = List.fold_left insert_term_to_ctx ctx (tms @ constructors) in
      let ctx = List.fold_left insert_type_to_ctx ctx tys in
      Ok ctx

let module_ (m : Cst.Module.t) (ctx : Ctx.t) :
    Tcst.Module.t option * Error.t list =
  match module_ m ctx with
  | Ok m -> (Some m, [])
  | Error e -> (None, e)

let solve_module (m : Cst.Module.t) (ctx : Ctx.t) : Ctx.t option * Error.t list
    =
  match solve_module m ctx with
  | Ok ctx -> (Some ctx, [])
  | Error errs -> (None, errs)