module TyCtx = Ctx.Make (String)

type ty_ctx = Type.t TyCtx.t
type 'a t = ('a, ty_ctx, Error.t list) StateResult.t

module S = StateResult
open StateResult.Syntax

let solve (s : 'a t) (ctx : ty_ctx) : ('a, Error.t list) result =
  match s ctx with
  | Ok (x, _) -> Ok x
  | Error es -> Error es

let solve_ctx (s : 'a t) (ctx : ty_ctx) : (ty_ctx, Error.t list) result =
  match s ctx with
  | Ok (_, ctx') -> Ok ctx'
  | Error es -> Error es

let alt (l : 'a t) (r : 'a t) : 'a t =
 fun ctx ->
  match l ctx with
  | Ok (x, ctx') -> Ok (x, ctx')
  | Error e1 -> (
      match r ctx with
      | Ok (y, ctx') -> Ok (y, ctx')
      | Error e2 -> Error (e1 @ e2))

let result (s : 'a t) : ('a, Error.t list) result t =
  alt (S.map (fun x -> Ok x) s) (S.pure (Error []))

let traverse_list (f : 'a -> 'b t) (ls : 'a list) : 'b list t =
  List.fold_right
    (fun x ys_t ->
      let* y = f x in
      let* ys = ys_t in
      S.pure (y :: ys))
    ls (S.pure [])

let unbound_var name span : Error.t =
  {
    kind = Error.Kind.Solver;
    location = Some span;
    lines = [ Text ("Unbound variable: " ^ name) ];
  }

let expression (e : Cst.expr) : (Tast.expr * Type.t) t =
  match e with
  | Const { value; span } ->
      let type_ = Type.Int in
      S.pure (Tast.Expr.Const { value; span; type_ }, type_)
  | Var { name; span } -> (
      let* ctx = S.get in
      match TyCtx.lookup name ctx with
      | None -> S.fail [ unbound_var name span ]
      | Some type_ -> S.pure (Tast.Expr.Var { name; span; type_ }, type_))

let binding (b : Cst.binding) : Tast.binding t =
  match b with
  | Def { name; expr; span } ->
      let* expr, type_ = expression expr in
      let* _ = S.mut (TyCtx.insert name type_) in
      S.pure (Tast.Binding.Def { name; expr; span; type_ })

let rec multiple_passes (remaining : int) (bs : Cst.binding list) :
    Tast.binding list t =
 fun ctx ->
  match traverse_list (fun b -> result (binding b)) bs ctx with
  | Ok (bs', ctx') ->
      let current = List.length (List.filter Result.is_error bs') in
      if current = 0 then
        Ok (List.map Result.get_ok bs', ctx')
      else if current < remaining then
        multiple_passes current bs ctx'
      else
        let es = List.map Result.get_error (List.filter Result.is_error bs') in
        Error (List.flatten es)
  | Error es -> Error es

let module_ (m : Cst.module_) : Tast.module_ t =
  match m with
  | Module { name; bindings; span } ->
      let* bindings = multiple_passes (List.length bindings) bindings in
      S.pure (Tast.Module.Module { name; bindings; span })
