module Tcst = Syn.Tcst

(** Desugars a Tcst pattern to Tast pattern *)
let pattern : Tcst.Pat.t -> Ir.pat = function
  | Tcst.Pat.Con (_, (head, _), vars) ->
      PCon (head, List.map (fun (var, _) -> var) vars)

(** Desugars a Tcst expression to Tast expression *)
let expression : Tcst.Expr.t -> Ir.expr =
  Tcst.Expr.fold (function
    | LitF ((_, ty), Int (_, n)) -> Ir.Lit (ty, Int n)
    | LitF ((_, ty), Bool (_, b)) -> Lit (ty, Bool b)
    | VarF ((_, ty), s) -> Var (ty, s)
    | LetF ((_, ty), n, _, d, b) -> Let (ty, n, d, b)
    | LetRecF ((_, ty), n, _, d, b) -> LetRec (ty, n, d, b)
    | IfF ((_, ty), c, t, f) -> If (ty, c, t, f)
    | LamF ((_, ty, ty'), p, _, b) -> Lam (ty, ty', p, b)
    | AppF ((_, ty), f, x) -> App (ty, f, x)
    | AnnF ((_, _), e, _) -> e
    | MatchF ((_, ty), e, bs) ->
        let bs = Non_empty.(to_list (map (fun (p, e) -> (pattern p, e)) bs)) in
        Match (ty, e, bs))

(** Desugars a Tcst ty_def to Tast ty_def *)
let ty : Tcst.Binding.ty_def -> Ir.ty_def = function
  | (_, kind, alts), name, _, _ -> TyDef { name; kind; alts }

(** Desugars a Tcst tm_def to Tast tm_def *)
let tm : Tcst.Binding.tm_def -> Ir.tm_def = function
  | (_, scheme), name, _, e -> TmDef { name; scheme; expr = expression e }

(** Desugars a Tcst binding to Tast binding *)
let binding : Tcst.Binding.t -> [ `Type of Ir.ty_def | `Term of Ir.tm_def ] =
  function
  | Def def -> `Term (tm def)
  | Type def -> `Type (ty def)

(** Desugars a Tcst module to Tast module *)
let module_ : Tcst.Module.t -> Ir.modu = function
  | Module (_, name, bindings) ->
      let bindings = List.map binding bindings in
      let rec partition tys tms = function
        | [] -> (List.rev tys, List.rev tms)
        | `Term tm :: bs -> partition tys (tm :: tms) bs
        | `Type ty :: bs -> partition (ty :: tys) tms bs
      in
      let types, terms = partition [] [] bindings in
      Ir.Module { name; types; terms }
