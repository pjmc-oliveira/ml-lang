open Extensions

(* The environment stores references to values.
   Whenever a value is forced to NF or WHNF it is updated
   in the environment.
   This way a value is evaluated at most once. *)
type tm_env = Value.t ref Env.t

module S = State_result
open State_result.Syntax

type 'a t = ('a, tm_env, Error.t list) State_result.t

let error ?location lines : Error.t = { kind = Interpreter; lines; location }

let traverse_list (f : 'a -> 'b t) (ls : 'a list) : 'b list t =
  List.fold_right
    (fun x ys_t ->
      let* y = f x in
      let* ys = ys_t in
      S.pure (y :: ys))
    ls (S.pure [])

let define name value env = Env.insert name value env

let defer (expr : Tast.expr) (env : tm_env) : Value.t =
  Value.Thunk { env; expr }

let fix name expr env = Value.Fix { env; name; expr }

(** Evaluate an expression in a context *)
let rec eval (e : Tast.expr) (env : tm_env) : (Value.t ref, Error.t) result =
  let open Result.Syntax in
  match e with
  | ELit (_, Int value) -> Ok (ref (Value.Int value))
  | ELit (_, Bool value) -> Ok (ref (Value.Bool value))
  | EVar (_, name) -> (
      match Env.lookup name env with
      | Some value -> Ok value
      | None -> failwith ("Impossible: Unbound variable: " ^ name))
  | ELet (_, name, def, body) ->
      let thunk = defer def env in
      let env' = define name (ref thunk) env in
      eval body env'
  | ELetRec (_, name, def, body) ->
      let fixpoint = fix name def env in
      let env' = define name (ref fixpoint) env in
      eval body env'
  | EIf (_, cond, con, alt) -> (
      let* cond = eval cond env in
      let* cond = force cond in
      match !cond with
      | Bool true -> eval con env
      | Bool false -> eval alt env
      | _ -> failwith ("Impossible if-cond not bool: " ^ Value.show !cond))
  | ELam (_, _, param, body) -> Ok (ref (Value.Closure { env; param; body }))
  | EApp (_, func, arg) -> (
      let arg' = defer arg env in
      let* func = eval func env in
      (* Only evaluate to WHNF so that we can apply the constructor lazily *)
      let* func = whnf func in
      match !func with
      | Closure { env = closure_ctx; param; body } ->
          let closure_ctx' = define param (ref arg') closure_ctx in
          eval body closure_ctx'
      | Con { head; arity; tail } ->
          (* TODO: there has to be a better way to deal with constructors *)
          Ok (ref (Value.Con { head; arity; tail = tail @ [ ref arg' ] }))
      | Native func ->
          (* Defer to create a thunk value from the ast
             then force to pass it into the native function *)
          let arg = defer arg env in
          let* arg = force (ref arg) in
          Ok (ref (func !arg))
      | _ ->
          failwith
            ("Imposible cannot apply to non-function: " ^ Value.show !func))
  | EMatch (_, expr, alts) -> (
      let* expr = eval expr env in
      let* expr = whnf expr in
      match !expr with
      | Con { head; tail; _ } -> (
          match
            List.find_opt (fun (Tast.PCon (name, _), _) -> name = head) alts
          with
          | None -> failwith ("Un-matched pattern: " ^ Value.show !expr)
          | Some (PCon (_, vars), case) ->
              let env' =
                List.fold_left
                  (fun env (name, value) -> define name value env)
                  env (List.zip vars tail)
              in
              eval case env')
      | _ ->
          failwith
            ("Impossible cannot match to non-constructor: " ^ Value.show !expr))

(** Force a value to weak-head normal form *)
and whnf (v : Value.t ref) : (Value.t ref, Error.t) result =
  let open Result.Syntax in
  match !v with
  | Int n -> Ok (ref (Value.Int n))
  | Bool b -> Ok (ref (Value.Bool b))
  | Con { head; arity; tail } -> Ok (ref (Value.Con { head; arity; tail }))
  | Closure f -> Ok (ref (Value.Closure f))
  | Thunk { env; expr } ->
      let* expr = eval expr env in
      v := !expr;
      whnf expr
  | Native _ -> Ok v
  | Fix { env; name; expr } ->
      let env' = define name (ref (Value.Fix { env; name; expr })) env in
      let* expr = eval expr env' in
      v := !expr;
      (* TODO: should this be a recursive or base call? *)
      whnf expr

(** Fully force a value to its normal form *)
and force (v : Value.t ref) : (Value.t ref, Error.t) result =
  let open Result.Syntax in
  match !v with
  | Int n -> Ok (ref (Value.Int n))
  | Bool b -> Ok (ref (Value.Bool b))
  | Con { head; arity; tail } ->
      let* tail =
        Result.traverse_list
          (fun v ->
            let* v' = force v in
            v := !v';
            Ok v')
          tail
      in
      Ok (ref (Value.Con { head; arity; tail }))
  | Closure f -> Ok (ref (Value.Closure f))
  | Thunk { env; expr } ->
      let* expr = eval expr env in
      v := !expr;
      force expr
  | Native _ -> Ok v
  | Fix { env; name; expr } ->
      let env' = define name (ref (Value.Fix { env; name; expr })) env in
      let* expr = eval expr env' in
      v := !expr;
      force expr

let term_def (b : Tast.tm_def) (env : tm_env) :
    (Value.t * tm_env, Error.t list) result =
  let open Result.Syntax in
  Result.map_error
    (fun e -> [ e ])
    (match b with
    | TmDef { expr; _ } ->
        let* value = eval expr env in
        Ok (!value, env))

let defer_term_def (tm_def : Tast.tm_def) (env : tm_env) :
    (Value.t * tm_env, Error.t list) result =
  match tm_def with
  | TmDef { name; expr; _ } ->
      let fixpoint = fix name expr env in
      let env' = define name (ref fixpoint) env in
      Ok (fixpoint, env')

let find_entrypoint entrypoint tm_defs : Tast.tm_def option =
  List.find_opt (fun (Tast.TmDef { name; _ }) -> name = entrypoint) tm_defs

let make_type_constructor ((head, tys) : string * Type.poly) =
  let arity = Type.get_arity (Type.get_mono_type tys) in
  Value.Con { head; arity; tail = [] }

let add_type_constructors (TyDef { alts; _ } : Tast.ty_def) (env : tm_env) :
    tm_env =
  let cons =
    List.map
      (fun ((name, _) as alt) -> (name, ref (make_type_constructor alt)))
      alts
  in
  let env' = Env.of_list cons in
  Env.union env' env

let module_ entrypoint (m : Tast.modu) : Value.t t =
  match m with
  | Module { terms; types; _ } ->
      let* env = S.get in
      let env' =
        List.fold_left
          (fun env ty_def -> add_type_constructors ty_def env)
          env types
      in
      let* _ = S.set env' in
      let* _ = traverse_list defer_term_def terms in
      let* b =
        match find_entrypoint entrypoint terms with
        | Some b -> S.pure b
        | None ->
            S.fail [ error [ Text ("Unbound entrypoint: " ^ entrypoint) ] ]
      in
      term_def b

let run ?(entrypoint = "main") ?(context = Env.empty) (m : Tast.modu) :
    (Value.t, Error.t list) result =
  let open Result.Syntax in
  match (module_ entrypoint) m context with
  | Ok (value, _) ->
      let* value = Result.map_error (fun e -> [ e ]) (force (ref value)) in
      Ok !value
  | Error e -> Error e
