module TyCtx = Ctx.Make (String)

type ty_ctx = Type.t TyCtx.t

(* open Result.Syntax *)

type 'a t = ty_ctx -> ('a * ty_ctx, Error.t list) result

let solve (s : 'a t) (ctx : ty_ctx) : ('a, Error.t list) result =
  match s ctx with
  | Ok (x, _) -> Ok x
  | Error es -> Error es

let pure (x : 'a) : 'a t = fun ctx -> Ok (x, ctx)

let bind (s : 'a t) (f : 'a -> 'b t) : 'b t =
 fun ctx ->
  match s ctx with
  | Ok (x, ctx') -> (f x) ctx'
  | Error es -> Error es

let prod (tx : 'a t) (ty : 'b t) =
  bind tx (fun x -> bind ty (fun y -> pure (x, y)))

let fail (e : Error.t list) : 'a t = fun _ -> Error e
let get : ty_ctx t = fun ctx -> Ok (ctx, ctx)
let set ctx : unit t = fun _ -> Ok ((), ctx)
let mut f : unit t = fun ctx -> Ok ((), f ctx)

module Syntax = struct
  let ( let* ) = bind
  let ( and* ) = prod
end

open Syntax

let traverse_list (f : 'a -> 'b t) (ls : 'a list) : 'b list t =
  List.fold_right
    (fun x ys_t ->
      let* y = f x in
      let* ys = ys_t in
      pure (y :: ys))
    ls (pure [])

let undound_var name span : Error.t =
  {
    kind = Error.Kind.Solver;
    location = Some span;
    lines = [ Text ("Unbound variable: " ^ name) ];
  }

let expression (e : Cst.expr) : (Tast.expr * Type.t) t =
  match e with
  | Const { value; span } ->
      let type_ = Type.Int in
      pure (Tast.Expr.Const { value; span; type_ }, type_)
  | Var { name; span } -> (
      let* ctx = get in
      match TyCtx.lookup name ctx with
      | None -> fail [ undound_var name span ]
      | Some type_ -> pure (Tast.Expr.Var { name; span; type_ }, type_))

let binding (b : Cst.binding) : Tast.binding t =
  match b with
  | Def { name; expr; span } ->
      let* expr, type_ = expression expr in
      pure (Tast.Binding.Def { name; expr; span; type_ })

let module_ (m : Cst.module_) : Tast.module_ t =
  match m with
  | Module { name; bindings; span } ->
      let* bindings = traverse_list binding bindings in
      pure (Tast.Module.Module { name; bindings; span })
