open Extensions

module type Applicative = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val traverse_result : ('a -> ('b, 'e) result) -> 'a t -> ('b t, 'e) result
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Ext = sig
  type x_ty
  type x_scheme
  type x_lit
  type x_pat

  val pp_x_ty : Format.formatter -> x_ty -> unit
  val pp_x_scheme : Format.formatter -> x_scheme -> unit
  val pp_x_lit : Format.formatter -> x_lit -> unit
  val pp_x_pat : Format.formatter -> x_pat -> unit

  module Expr : sig
    type x_lit
    type x_var
    type x_let
    type x_if
    type x_lam
    type x_app
    type x_ann
    type x_match

    val pp_x_lit : Format.formatter -> x_lit -> unit
    val pp_x_var : Format.formatter -> x_var -> unit
    val pp_x_let : Format.formatter -> x_let -> unit
    val pp_x_if : Format.formatter -> x_if -> unit
    val pp_x_lam : Format.formatter -> x_lam -> unit
    val pp_x_app : Format.formatter -> x_app -> unit
    val pp_x_ann : Format.formatter -> x_ann -> unit
    val pp_x_match : Format.formatter -> x_match -> unit
  end

  module Binding : sig
    type x_tm_def
    type x_ty_def

    val pp_x_tm_def : Format.formatter -> x_tm_def -> unit
    val pp_x_ty_def : Format.formatter -> x_ty_def -> unit
  end

  type x_module

  val pp_x_module : Format.formatter -> x_module -> unit
end

module Make (X : Ext) (F : Applicative) = struct
  module Type = struct
    type t =
      | Con of X.x_ty * string F.t
      | Var of X.x_ty * string F.t
      | App of X.x_ty * t F.t * t F.t
      | Arr of X.x_ty * t F.t * t F.t
    [@@deriving show { with_path = false }]

    let map_x f = function
      | Con (x, n) -> Con (f x, n)
      | Var (x, n) -> Var (f x, n)
      | App (x, fn, arg) -> App (f x, fn, arg)
      | Arr (x, param, body) -> Arr (f x, param, body)

    let get_x = function
      | Con (x, _) -> x
      | Var (x, _) -> x
      | App (x, _, _) -> x
      | Arr (x, _, _) -> x
  end

  module Scheme = struct
    type t =
      | Type of X.x_scheme * Type.t F.t
      | Forall of X.x_scheme * string F.t list * Type.t F.t
    [@@deriving show { with_path = false }]
  end

  module Lit = struct
    type t =
      | Int of X.x_lit * int F.t
      | Bool of X.x_lit * bool F.t
    [@@deriving show { with_path = false }]
  end

  module Pat = struct
    type t =
      | Con of X.x_pat * (string F.t * X.x_pat) * (string F.t * X.x_pat) list
    [@@deriving show { with_path = false }]
  end

  module Expr = struct
    type t =
      | Lit of X.Expr.x_lit * Lit.t F.t
      | Var of X.Expr.x_var * string F.t
      | Let of X.Expr.x_let * string F.t * Type.t F.t option * t F.t * t F.t
      | LetRec of X.Expr.x_let * string F.t * Type.t F.t option * t F.t * t F.t
      | If of X.Expr.x_if * t F.t * t F.t * t F.t
      | Lam of X.Expr.x_lam * string F.t * Type.t F.t option * t F.t
      | App of X.Expr.x_app * t F.t * t F.t
      | Ann of X.Expr.x_ann * t F.t * Type.t F.t
      | Match of X.Expr.x_match * t F.t * (Pat.t F.t * t F.t) F.t Non_empty.t
    [@@deriving show { with_path = false }]

    type 'a f =
      | LitF of X.Expr.x_lit * Lit.t F.t
      | VarF of X.Expr.x_var * string F.t
      | LetF of X.Expr.x_let * string F.t * Type.t F.t option * 'a F.t * 'a F.t
      | LetRecF of
          X.Expr.x_let * string F.t * Type.t F.t option * 'a F.t * 'a F.t
      | IfF of X.Expr.x_if * 'a F.t * 'a F.t * 'a F.t
      | LamF of X.Expr.x_lam * string F.t * Type.t F.t option * 'a F.t
      | AppF of X.Expr.x_app * 'a F.t * 'a F.t
      | AnnF of X.Expr.x_ann * 'a F.t * Type.t F.t
      | MatchF of X.Expr.x_match * 'a F.t * (Pat.t F.t * 'a F.t) F.t Non_empty.t
    [@@deriving show { with_path = false }]

    let f_to_t = function
      | LitF (x, lit) -> Lit (x, lit)
      | VarF (x, name) -> Var (x, name)
      | LetF (x, name, ann, def, body) -> Let (x, name, ann, def, body)
      | LetRecF (x, name, ann, def, body) -> LetRec (x, name, ann, def, body)
      | IfF (x, cond, con, alt) -> If (x, cond, con, alt)
      | LamF (x, param, ann, body) -> Lam (x, param, ann, body)
      | AppF (x, func, arg) -> App (x, func, arg)
      | AnnF (x, expr, ann) -> Ann (x, expr, ann)
      | MatchF (x, expr, alts) -> Match (x, expr, alts)

    let rec fold f = function
      | Lit (x, lit) -> f (LitF (x, lit))
      | Var (x, name) -> f (VarF (x, name))
      | Let (x, name, ann, def, body) ->
          let def = F.map (fold f) def in
          let body = F.map (fold f) body in
          f (LetF (x, name, ann, def, body))
      | LetRec (x, name, ann, def, body) ->
          let def = F.map (fold f) def in
          let body = F.map (fold f) body in
          f (LetRecF (x, name, ann, def, body))
      | If (x, cond, con, alt) ->
          let cond = F.map (fold f) cond in
          let con = F.map (fold f) con in
          let alt = F.map (fold f) alt in
          f (IfF (x, cond, con, alt))
      | Lam (x, param, ann, body) ->
          let body = F.map (fold f) body in
          f (LamF (x, param, ann, body))
      | App (x, func, arg) ->
          let func = F.map (fold f) func in
          let arg = F.map (fold f) arg in
          f (AppF (x, func, arg))
      | Ann (x, expr, ann) ->
          let expr = F.map (fold f) expr in
          f (AnnF (x, expr, ann))
      | Match (x, expr, alts) ->
          let expr = F.map (fold f) expr in
          let alts =
            Non_empty.map (F.map (fun (p, e) -> (p, F.map (fold f) e))) alts
          in
          f (MatchF (x, expr, alts))

    let rec fold_result (f : 'a f -> ('a, 'e) result) : t -> ('a, 'e) result =
      let open Result.Syntax in
      function
      | Lit (x, lit) -> f (LitF (x, lit))
      | Var (x, name) -> f (VarF (x, name))
      | Let (x, name, ann, def, body) ->
          let* def = F.traverse_result (fold_result f) def in
          let* body = F.traverse_result (fold_result f) body in
          f (LetF (x, name, ann, def, body))
      | LetRec (x, name, ann, def, body) ->
          let* def = F.traverse_result (fold_result f) def in
          let* body = F.traverse_result (fold_result f) body in
          f (LetRecF (x, name, ann, def, body))
      | If (x, cond, con, alt) ->
          let* cond = F.traverse_result (fold_result f) cond in
          let* con = F.traverse_result (fold_result f) con in
          let* alt = F.traverse_result (fold_result f) alt in
          f (IfF (x, cond, con, alt))
      | Lam (x, param, ann, body) ->
          let* body = F.traverse_result (fold_result f) body in
          f (LamF (x, param, ann, body))
      | App (x, func, arg) ->
          let* func = F.traverse_result (fold_result f) func in
          let* arg = F.traverse_result (fold_result f) arg in
          f (AppF (x, func, arg))
      | Ann (x, expr, ann) ->
          let* expr = F.traverse_result (fold_result f) expr in
          f (AnnF (x, expr, ann))
      | Match (x, expr, alts) ->
          let* expr = F.traverse_result (fold_result f) expr in
          let* alts =
            Result.traverse_list
              (F.traverse_result (fun (p, e) ->
                   let* e = F.traverse_result (fold_result f) e in
                   Ok (p, e)))
              (Non_empty.to_list alts)
          in
          f (MatchF (x, expr, Non_empty.of_list alts))
  end

  module Binding = struct
    type tm_def =
      X.Binding.x_tm_def * string F.t * Scheme.t F.t option * Expr.t F.t
    [@@deriving show { with_path = false }]

    type alt = string F.t * Type.t F.t list
    [@@deriving show { with_path = false }]

    type ty_def =
      X.Binding.x_ty_def * string F.t * string F.t list * alt F.t list
    [@@deriving show { with_path = false }]

    type t =
      | Def of tm_def
      | Type of ty_def
    [@@deriving show { with_path = false }]
  end

  module Module = struct
    type t = Module of X.x_module * string F.t * Binding.t F.t list
    [@@deriving show { with_path = false }]
  end
end

module Identity = struct
  type 'a t = 'a [@@deriving show { with_path = false }]

  let map f x = f x
  let traverse_result f x = f x
end

(** The un-annotated abstract syntax tree *)
module Ast =
  Make
    (struct
      type x_ty = unit [@@deriving show { with_path = false }]
      type x_scheme = unit [@@deriving show { with_path = false }]
      type x_lit = unit [@@deriving show { with_path = false }]
      type x_pat = unit [@@deriving show { with_path = false }]

      module Expr = struct
        type x_lit = unit [@@deriving show { with_path = false }]
        type x_var = unit [@@deriving show { with_path = false }]
        type x_let = unit [@@deriving show { with_path = false }]
        type x_if = unit [@@deriving show { with_path = false }]
        type x_lam = unit [@@deriving show { with_path = false }]
        type x_app = unit [@@deriving show { with_path = false }]
        type x_ann = unit [@@deriving show { with_path = false }]
        type x_match = unit [@@deriving show { with_path = false }]
      end

      module Binding = struct
        type x_tm_def = unit [@@deriving show { with_path = false }]
        type x_ty_def = unit [@@deriving show { with_path = false }]
      end

      type x_module = unit [@@deriving show { with_path = false }]
    end)
    (Identity)

(** The concrete syntax tree, annotated with spans *)
module Cst = struct
  include
    Make
      (struct
        type x_ty = Span.t [@@deriving show { with_path = false }]
        type x_scheme = Span.t [@@deriving show { with_path = false }]
        type x_lit = Span.t [@@deriving show { with_path = false }]
        type x_pat = Span.t [@@deriving show { with_path = false }]

        module Expr = struct
          type x_lit = Span.t [@@deriving show { with_path = false }]
          type x_var = Span.t [@@deriving show { with_path = false }]
          type x_let = Span.t [@@deriving show { with_path = false }]
          type x_if = Span.t [@@deriving show { with_path = false }]
          type x_lam = Span.t [@@deriving show { with_path = false }]
          type x_app = Span.t [@@deriving show { with_path = false }]
          type x_ann = Span.t [@@deriving show { with_path = false }]
          type x_match = Span.t [@@deriving show { with_path = false }]
        end

        module Binding = struct
          type x_tm_def = Span.t [@@deriving show { with_path = false }]
          type x_ty_def = Span.t [@@deriving show { with_path = false }]
        end

        type x_module = Span.t [@@deriving show { with_path = false }]
      end)
      (Identity)

  let span_of_expr = function
    | Expr.Lit (x, _) -> x
    | Expr.Var (x, _) -> x
    | Expr.Let (x, _, _, _, _) -> x
    | Expr.LetRec (x, _, _, _, _) -> x
    | Expr.If (x, _, _, _) -> x
    | Expr.Lam (x, _, _, _) -> x
    | Expr.App (x, _, _) -> x
    | Expr.Ann (x, _, _) -> x
    | Expr.Match (x, _, _) -> x
end

(** Convert from a concrete to an abstract module *)
let cst_to_ast : Cst.Module.t -> Ast.Module.t =
  let rec type_to_ast : Cst.Type.t -> Ast.Type.t = function
    | Con (_, name) -> Con ((), name)
    | Var (_, name) -> Var ((), name)
    | App (_, fn, arg) -> App ((), type_to_ast fn, type_to_ast arg)
    | Arr (_, param, body) -> Arr ((), type_to_ast param, type_to_ast body)
  in
  let lit_to_ast : Cst.Lit.t -> Ast.Lit.t = function
    | Int (_, n) -> Int ((), n)
    | Bool (_, b) -> Bool ((), b)
  in
  let pat_to_ast : Cst.Pat.t -> Ast.Pat.t = function
    | Con (_, (head, _), vars) ->
        Con ((), (head, ()), List.map (fun (var, _) -> (var, ())) vars)
  in
  let rec expr_to_ast : Cst.Expr.t -> Ast.Expr.t = function
    | Lit (_, lit) -> Lit ((), lit_to_ast lit)
    | Var (_, name) -> Var ((), name)
    | Let (_, name, ann, def, body) ->
        Let
          ( (),
            name,
            Option.map type_to_ast ann,
            expr_to_ast def,
            expr_to_ast body )
    | LetRec (_, name, ann, def, body) ->
        LetRec
          ( (),
            name,
            Option.map type_to_ast ann,
            expr_to_ast def,
            expr_to_ast body )
    | If (_, cond, con, alt) ->
        If ((), expr_to_ast cond, expr_to_ast con, expr_to_ast alt)
    | Lam (_, param, ann, body) ->
        Lam ((), param, Option.map type_to_ast ann, expr_to_ast body)
    | App (_, fn, arg) -> App ((), expr_to_ast fn, expr_to_ast arg)
    | Ann (_, expr, ann) -> Ann ((), expr_to_ast expr, type_to_ast ann)
    | Match (_, expr, branches) ->
        Match
          ( (),
            expr_to_ast expr,
            Non_empty.map
              (fun (pat, branch) -> (pat_to_ast pat, expr_to_ast branch))
              branches )
  in

  let scheme_to_ast : Cst.Scheme.t -> Ast.Scheme.t = function
    | Type (_, ty) -> Type ((), type_to_ast ty)
    | Forall (_, ty_vars, ty) -> Forall ((), ty_vars, type_to_ast ty)
  in
  let bind_to_ast : Cst.Binding.t -> Ast.Binding.t = function
    | Def (_, name, scheme, expr) ->
        Def ((), name, Option.map scheme_to_ast scheme, expr_to_ast expr)
    | Type (_, name, params, alts) ->
        let alts =
          List.map (fun (con, tys) -> (con, List.map type_to_ast tys)) alts
        in
        Type ((), name, params, alts)
  in
  function
  | Module (_, name, binds) -> Module ((), name, List.map bind_to_ast binds)

(* Alias referring to type.ml *)
module Ty = Type

(** The typed, concrete syntax tree *)
module Tcst = struct
  include
    Make
      (struct
        type x_ty = Span.t [@@deriving show { with_path = false }]
        type x_scheme = Span.t [@@deriving show { with_path = false }]
        type x_lit = Span.t [@@deriving show { with_path = false }]
        type x_pat = Span.t [@@deriving show { with_path = false }]

        module Expr = struct
          type x_lit = Span.t * Type.mono
          [@@deriving show { with_path = false }]

          type x_var = Span.t * Type.mono
          [@@deriving show { with_path = false }]

          type x_let = Span.t * Type.mono
          [@@deriving show { with_path = false }]

          type x_if = Span.t * Type.mono [@@deriving show { with_path = false }]

          type x_lam = Span.t * Type.mono * Type.mono
          [@@deriving show { with_path = false }]

          type x_app = Span.t * Type.mono
          [@@deriving show { with_path = false }]

          type x_ann = Span.t * Type.mono
          [@@deriving show { with_path = false }]

          type x_match = Span.t * Type.mono
          [@@deriving show { with_path = false }]
        end

        module Binding = struct
          type x_tm_def = Span.t * Type.poly
          [@@deriving show { with_path = false }]

          type x_ty_def = Span.t * Type.kind * (string * Ty.poly) list
          [@@deriving show { with_path = false }]
        end

        type x_module =
          Span.t
          * (string * Type.poly) list
          * (string * Type.kind * (string * Ty.poly) list) list
        [@@deriving show { with_path = false }]
      end)
      (Identity)

  let span_of_expr = function
    | Expr.Lit ((sp, _), _) -> sp
    | Expr.Var ((sp, _), _) -> sp
    | Expr.Let ((sp, _), _, _, _, _) -> sp
    | Expr.LetRec ((sp, _), _, _, _, _) -> sp
    | Expr.If ((sp, _), _, _, _) -> sp
    | Expr.Lam ((sp, _, _), _, _, _) -> sp
    | Expr.App ((sp, _), _, _) -> sp
    | Expr.Ann ((sp, _), _, _) -> sp
    | Expr.Match ((sp, _), _, _) -> sp

  let type_of_expr = function
    | Expr.Lit ((_, ty), _) -> ty
    | Expr.Var ((_, ty), _) -> ty
    | Expr.Let ((_, ty), _, _, _, _) -> ty
    | Expr.LetRec ((_, ty), _, _, _, _) -> ty
    | Expr.If ((_, ty), _, _, _) -> ty
    | Expr.Lam ((_, ty, ty'), _, _, _) -> Ty.Arrow (ty, ty')
    | Expr.App ((_, ty), _, _) -> ty
    | Expr.Ann ((_, ty), _, _) -> ty
    | Expr.Match ((_, ty), _, _) -> ty

  let map_type f : Expr.t -> Expr.t =
    Expr.fold (function
      | LitF ((sp, ty), lit) -> Expr.Lit ((sp, f ty), lit)
      | VarF ((sp, ty), name) -> Var ((sp, f ty), name)
      | LetF ((sp, ty), name, ann, def, body) ->
          Let ((sp, f ty), name, ann, def, body)
      | LetRecF ((sp, ty), name, ann, def, body) ->
          LetRec ((sp, f ty), name, ann, def, body)
      | IfF ((sp, ty), cond, con, alt) -> If ((sp, f ty), cond, con, alt)
      | LamF ((sp, ty, ty'), param, ann, body) ->
          Lam ((sp, f ty, f ty'), param, ann, body)
      | AppF ((sp, ty), func, arg) -> App ((sp, f ty), func, arg)
      | AnnF ((sp, ty), expr, ann) -> Ann ((sp, f ty), expr, ann)
      | MatchF ((sp, ty), expr, alts) -> Match ((sp, f ty), expr, alts))

  let rec type_of_cst : Cst.Type.t -> Type.t = function
    | Con (x, name) -> Con (x, name)
    | Var (x, name) -> Var (x, name)
    | App (x, fn, arg) -> App (x, type_of_cst fn, type_of_cst arg)
    | Arr (x, param, body) -> Arr (x, type_of_cst param, type_of_cst body)
end