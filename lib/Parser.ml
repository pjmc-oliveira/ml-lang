module Cst = Syn.Cst

module Combinator = struct
  type tokens = (Token.t * Span.t) list * Span.t option
  type errors = Error.t list

  module Consumed = struct
    type t =
      | Consumed
      | Unconsumed

    let both l r =
      match l with
      | Consumed -> Consumed
      | _ -> r
  end

  type 'a t = tokens -> Consumed.t * ('a * tokens, errors) result

  let pure x : 'a t = fun tks -> (Unconsumed, Ok (x, tks))
  let fail e : 'a t = fun _ -> (Unconsumed, Error e)
  let error ?location lines : Error.t = { kind = Parser; lines; location }
  let fail_lines ?location lines = fail [ error ?location lines ]

  let parse (p : 'a t) tks : 'a option * errors =
    match p (tks, None) with
    | _, Error errs -> (None, errs)
    | _, Ok (x, ([], _)) -> (Some x, [])
    | _, Ok (_, _) -> failwith "Warning: Unconsumed input\n"

  let map f (p : 'a t) : 'b t =
   fun s ->
    match p s with
    | c, Ok (x, s') -> (c, Ok (f x, s'))
    | c, Error e -> (c, Error e)

  let prod (p : 'a t) (q : 'b t) : ('a * 'b) t =
   fun s ->
    match p s with
    | c, Error e -> (c, Error e)
    | c, Ok (x, s') -> (
        match q s' with
        | c', Error e -> (c', Error e)
        | c', Ok (y, s'') -> (Consumed.both c c', Ok ((x, y), s'')))

  let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    match p s with
    | c, Error e -> (c, Error e)
    | c, Ok (x, s') ->
        let c', r = (f x) s' in
        (Consumed.both c c', r)

  let alt (p : 'a t) (q : 'a t) : 'a t =
   fun s ->
    match p s with
    | Unconsumed, Error _ -> q s
    | Consumed, Error e -> (Consumed, Error e)
    | c, Ok (x, s') -> (c, Ok (x, s'))

  let one_of err (ps : 'a t list) : 'a t =
    List.fold_right alt ps (fun _ -> (Unconsumed, Error [ err ]))

  let optional (p : 'a t) : 'a option t =
    alt (map (fun x -> Some x) p) (pure None)

  module Syntax = struct
    let ( let+ ) = map
    let ( and+ ) = prod
    let ( let* ) = bind
    let ( and* ) = ( and+ )
  end

  open Syntax

  module Infix = struct
    let ( *> ) tx ty =
      let* _ = tx in
      let* y = ty in
      pure y

    let ( <* ) tx ty =
      let* x = tx in
      let* _ = ty in
      pure x

    let ( >>= ) = bind
  end

  let try_ p : 'a t =
   fun s ->
    match p s with
    | c, Ok (x, s') -> (c, Ok (x, s'))
    | _, Error e -> (Unconsumed, Error e)

  let rec some p =
    let* x = p in
    let* xs = many p in
    pure (Non_empty.make x xs)

  and many p = alt (try_ (map Non_empty.to_list (some p))) (pure [])

  let accumulate (p : 'a t) ~(recover : unit t) : 'a list t =
    let rec loop c oks errs s =
      match p s with
      | c', Ok (ok, s') -> loop (Consumed.both c c') (ok :: oks) errs s'
      | Unconsumed, Error _ ->
          if errs = [] then
            (c, Ok (List.rev oks, s))
          else
            (c, Error List.(rev (flatten errs)))
      | Consumed, Error err -> (
          match recover s with
          | _, Ok (_, s') -> loop Consumed.Consumed oks (err :: errs) s'
          | _, Error err -> (Consumed, Error List.(rev (flatten (err :: errs))))
          )
    in
    loop Unconsumed [] []

  let token : Token.t t = function
    | [], _ -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | (tk, loc) :: tks, _ -> (Consumed, Ok (tk, (tks, Some loc)))

  let peek : Token.t option t = function
    | [], _ -> (Unconsumed, Ok (None, ([], None)))
    | (tk, loc) :: tks, _ ->
        (Consumed, Ok (Some tk, ((tk, loc) :: tks, Some loc)))

  let next_position : Span.t t =
   fun tks ->
    match tks with
    | [], _ -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | (_tk, loc) :: _tks, _ -> (Unconsumed, Ok (loc, tks))

  let last_position : Span.t t =
   fun tks ->
    match tks with
    | _, None -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | _, Some loc -> (Unconsumed, Ok (loc, tks))

  let span_of (p : 'a t) : ('a * Span.t) t =
    let* p1 = next_position in
    let* res = p in
    let* p2 = last_position in
    pure (res, Span.between p1 p2)

  let with_span (p : (Span.t -> 'a) t) : 'a t =
    let* f, sp = span_of p in
    pure (f sp)

  let eof : unit t = function
    | [], loc -> (Unconsumed, Ok ((), ([], loc)))
    | _ -> (Unconsumed, Error [ error [ Text "Expected EOF" ] ])

  let rec drop_until predicate : unit t =
    let* t = token in
    if predicate t then
      drop_until predicate
    else
      pure ()

  let expect expected =
    let* tk = token in
    if tk = expected then
      pure ()
    else
      let* pos = last_position in
      fail_lines ~location:pos
        [
          Text ("Unexpected token: " ^ Token.to_string tk);
          Quote pos;
          Text ("Expected: " ^ Token.to_string expected);
        ]

  let accept expected = try_ (expect expected)

  let lower_identifier =
    let* tk = token in
    match tk with
    | LowerIdent s -> pure s
    | _ ->
        let* pos = last_position in
        fail_lines ~location:pos
          [
            Text "Expected lowercase identifier";
            Quote pos;
            Text ("But got: " ^ Token.to_string tk);
          ]

  let upper_identifier =
    let* tk = token in
    match tk with
    | UpperIdent s -> pure s
    | _ ->
        let* pos = last_position in
        fail_lines ~location:pos
          [
            Text "Expected uppercase identifier";
            Quote pos;
            Text ("But got: " ^ Token.to_string tk);
          ]
end

open Combinator
open Combinator.Syntax
open Combinator.Infix

let toplevel =
  let* () = drop_until (fun t -> t = Def || t = Type) in
  pure ()

let rec type_ () : Cst.Type.t t =
  with_span
    (let* from = type_application () in
     one_of
       (error [ Text "Expected type" ])
       [
         (let* to_ = accept Arrow *> type_ () in
          pure (fun span -> Cst.Type.Arr (span, from, to_)));
         pure (fun _ -> from);
       ])

and type_application () =
  with_span
    (let* func = type_atom () in
     let* args = many (type_atom ()) in
     let ty =
       List.fold_left
         (fun func arg span ->
           let func = func span in
           Cst.Type.App (span, func, arg))
         (fun _ -> func)
         args
     in
     pure ty)

and type_atom () =
  with_span
    (let* tk = token in
     match tk with
     | UpperIdent name -> pure (fun span -> Cst.Type.Con (span, name))
     | LowerIdent name -> pure (fun span -> Cst.Type.Var (span, name))
     | LeftParen ->
         let* expr = type_ () in
         let* _ = expect RightParen in
         pure (fun span -> Cst.Type.map_x (fun _ -> span) expr)
     | _ -> fail_lines [ Text "Expected type atom" ])

let type_scheme () : Cst.Scheme.t t =
  with_span
    (let* ty_vars =
       optional (accept Forall *> some lower_identifier <* expect Dot)
     in
     let* ty = type_ () in
     match ty_vars with
     | None -> pure (fun span -> Cst.Scheme.Type (span, ty))
     | Some ty_vars ->
         pure (fun span ->
             Cst.Scheme.Forall (span, Non_empty.to_list ty_vars, ty)))

let rec expression () : Cst.Expr.t t =
  one_of
    (error [ Text "Expected expression" ])
    [ let_in (); if_then_else (); lambda (); match_with (); annotation () ]

and let_in () =
  with_span
    (let* _ = accept Let in
     let* name = lower_identifier in
     let* def_t = optional (accept Colon *> type_ ()) in
     let* _ = expect Equal in
     let* def = expression () in
     let* _ = expect In in
     let* body = expression () in
     pure (fun span -> Cst.Expr.Let (span, name, def_t, def, body)))

and if_then_else () =
  with_span
    (let* _ = accept If in
     let* cond = expression () in
     let* _ = expect Then in
     let* con = expression () in
     let* _ = expect Else in
     let* alt = expression () in
     pure (fun span -> Cst.Expr.If (span, cond, con, alt)))

and lambda () =
  with_span
    (let* _ = accept BackSlash in
     let* param = lower_identifier in
     let* param_t = optional (accept Colon *> type_ () <* expect Dot) in
     let* body = expression () in
     pure (fun span -> Cst.Expr.Lam (span, param, param_t, body)))

and match_with () =
  with_span
    (let* _ = accept Match in
     let* expr = expression () in
     let* _ = expect With in
     let* alts = some (match_branch ()) in
     let* _ = expect End in
     pure (fun span -> Cst.Expr.Match (span, expr, alts)))

and match_branch () : (Cst.Pat.t * Cst.Expr.t) t =
  let* _ = accept Pipe in
  let* pat =
    with_span
      (let* head = span_of upper_identifier in
       let* vars = many (span_of lower_identifier) in
       pure (fun span -> Cst.Pat.Con (span, head, vars)))
  in
  let* _ = expect Arrow in
  let* body = expression () in
  pure (pat, body)

and annotation () =
  with_span
    (let* expr = application () in
     one_of
       (error [ Text "Expected annotation" ])
       [
         (let* ann = accept Colon *> type_ () in
          pure (fun span -> Cst.Expr.Ann (span, expr, ann)));
         pure (fun _ -> expr);
       ])

and application () =
  let* expr =
    with_span
      (let* func = atom () in
       let* args = many (atom ()) in
       let expr =
         List.fold_left
           (fun func arg span ->
             let func = func span in
             Cst.Expr.App (span, func, arg))
           (fun _ -> func)
           args
       in
       pure expr)
  in
  (* Allow for a dangling lambda as last expression *)
  let* block = optional (lambda ()) in
  match block with
  | None -> pure expr
  | Some block ->
      with_span (pure (fun span -> Cst.Expr.App (span, expr, block)))

and atom () =
  with_span
    (let* tk = token in
     match tk with
     | Int value -> pure (fun span -> Cst.(Expr.Lit (span, Int (span, value))))
     | Bool value ->
         pure (fun span -> Cst.(Expr.Lit (span, Bool (span, value))))
     | LowerIdent name -> pure (fun span -> Cst.Expr.Var (span, name))
     | UpperIdent name -> pure (fun span -> Cst.Expr.Var (span, name))
     | LeftParen ->
         let* expr = expression () in
         let* _ = expect RightParen in
         pure (fun _ -> expr)
     | _ -> fail_lines [ Text "Expected atom" ])

let def : (Span.t -> Cst.Binding.t) t =
  let* () = accept Def in
  let* name = lower_identifier in
  let* ann = optional (accept Colon *> type_scheme ()) in
  let* () = expect Equal in
  let* expr = expression () in
  pure (fun span -> Cst.Binding.Def (span, name, ann, expr))

let ty_alternatives =
  let single_alt =
    let* head = upper_identifier in
    (* Only want type atoms, complex types should be wrapped in parens *)
    let* tys = many (type_atom ()) in
    pure (head, tys)
  in
  let* _ = optional (accept Pipe) in
  let* alt = single_alt in
  let* alts = many (accept Pipe *> single_alt) in
  pure (alt :: alts)

let ty_def : (Span.t -> Cst.Binding.t) t =
  let* _ = accept Type in
  let* name = upper_identifier in
  let* vars = many lower_identifier in
  let* _ = expect Equal in
  let* alts = ty_alternatives in
  pure (fun span -> Cst.Binding.Type (span, name, vars, alts))

let binding : Cst.Binding.t t =
  let* b, sp =
    span_of (one_of (error [ Text "Expected binding" ]) [ def; ty_def ])
  in
  pure (b sp)

let module_ : Cst.Module.t t =
  let parse_module =
    let* () = accept Module in
    let* name = upper_identifier in
    let* () = expect Equal in
    let* () = expect LeftBrace in
    (* TODO: accumulate is not working properly *)
    let* bindings = accumulate binding ~recover:toplevel in
    let* () = expect RightBrace in
    let* () = eof in
    pure (fun span -> Cst.Module.Module (span, name, bindings))
  in
  let* m, sp = span_of parse_module in
  pure (m sp)

let parse = Combinator.parse