module Combinator = struct
  type tokens = (Token.t * Source.span) list * Source.span option
  type errors = Error.t list

  module Consumed = struct
    type t = Consumed | Unconsumed

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

  let parse (p : 'a t) tks : ('a, errors) result =
    match p (tks, None) with
    | _, Error errs -> Error errs
    | _, Ok (x, ([], _)) -> Ok x
    | _, Ok (x, _) ->
        print_string "Warning: Unnconsumed input\n";
        Ok x

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
    pure (x :: xs)

  and many p = alt (try_ (some p)) (pure [])

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

  let next_position : Source.span t =
   fun tks ->
    match tks with
    | [], _ -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | (_tk, loc) :: _tks, _ -> (Unconsumed, Ok (loc, tks))

  let last_position : Source.span t =
   fun tks ->
    match tks with
    | _, None -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | _, Some loc -> (Unconsumed, Ok (loc, tks))

  let span_of (p : 'a t) : ('a * Source.span) t =
    let* p1 = next_position in
    let* res = p in
    let* p2 = last_position in
    pure (res, Source.Span.between p1 p2)

  let with_span (p : (Source.span -> 'a) t) : 'a t =
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
          Text ("Expected: " ^ Token.to_string expected);
          Text ("But got: " ^ Token.to_string tk);
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
            Text "Expected lower identifier";
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
            Text "Expected upper identifier";
            Text ("But got: " ^ Token.to_string tk);
          ]
end

open Combinator
open Combinator.Syntax
open Combinator.Infix

type ty = Source.span Cst.ty
type expr = Source.span Cst.expr
type binding = Source.span Cst.binding
type module_ = Source.span Cst.module_

let toplevel =
  let* () = drop_until (fun t -> t = Def) in
  pure ()

let rec type_ () : ty t =
  with_span
    (let* from = type_atom () in
     one_of
       (error [ Text "Expected type" ])
       [
         (let* to_ = accept Arrow *> type_ () in
          pure (fun span -> Cst.Type.Arrow { from; to_; span }));
         pure (fun _ -> from);
       ])

and type_scheme () : ty t =
  with_span
    (let* ty_vars =
       optional (accept Forall *> some lower_identifier <* expect Dot)
     in
     let* ty = type_ () in
     match ty_vars with
     | None -> pure (fun _ -> ty)
     | Some ty_vars ->
         pure (fun span -> Cst.Type.Forall { ty_vars; type_ = ty; span }))

and type_atom () : ty t =
  with_span
    (let* tk = token in
     match tk with
     | UpperIdent name -> pure (fun span -> Cst.Type.Const { name; span })
     | LowerIdent name -> pure (fun span -> Cst.Type.Var { name; span })
     | LeftParen ->
         let* expr = type_ () in
         let* _ = expect RightParen in
         pure (fun span -> Cst.Type.map (fun _ -> span) expr)
     | _ -> fail_lines [ Text "Expected type atom" ])

let rec expression () : expr t =
  one_of
    (error [ Text "Expected expression" ])
    [ let_in (); it_then_else (); lambda (); annotation () ]

and let_in () =
  with_span
    (let* _ = accept Let in
     let* name = lower_identifier in
     let* def_t = optional (accept Colon *> type_ ()) in
     let* _ = expect Equal in
     let* def = expression () in
     let* _ = expect In in
     let* body = expression () in
     pure (fun span -> Cst.Expr.Let { name; def_t; def; body; span }))

and it_then_else () =
  with_span
    (let* _ = accept If in
     let* cond = expression () in
     let* _ = expect Then in
     let* con = expression () in
     let* _ = expect Else in
     let* alt = expression () in
     pure (fun span -> Cst.Expr.If { cond; con; alt; span }))

and lambda () =
  with_span
    (let* _ = accept BackSlash in
     let* param = lower_identifier in
     let* param_t = optional (accept Colon *> type_ () <* expect Dot) in
     let* body = expression () in
     pure (fun span -> Cst.Expr.Lam { param; param_t; body; span }))

and annotation () =
  with_span
    (let* expr = application () in
     one_of
       (error [ Text "Expected annotation" ])
       [
         (let* ann = accept Colon *> type_ () in
          pure (fun span -> Cst.Expr.Ann { expr; ann; span }));
         pure (fun _ -> expr);
       ])

and application () : expr t =
  with_span
    (let* func = atom () in
     let* args = many (atom ()) in
     let expr =
       List.fold_left
         (fun func arg span ->
           let func = func span in
           Cst.Expr.App { func; arg; span })
         (fun _ -> func)
         args
     in
     pure expr)

and atom () : expr t =
  with_span
    (let* tk = token in
     match tk with
     | Int value -> pure (fun span -> Cst.Expr.Int { value; span })
     | Bool value -> pure (fun span -> Cst.Expr.Bool { value; span })
     | LowerIdent name -> pure (fun span -> Cst.Expr.Var { name; span })
     | LeftParen ->
         let* expr = expression () in
         let* _ = expect RightParen in
         pure (fun span -> Cst.Expr.map (fun _ -> span) expr)
     | _ -> fail_lines [ Text "Expected atom" ])

let def : (Source.span -> binding) t =
  let* () = accept Def in
  let* name = lower_identifier in
  let* ann = optional (accept Colon *> type_scheme ()) in
  let* () = expect Equal in
  let* expr = expression () in
  pure (fun span -> Cst.Binding.Def { name; ann; expr; span })

let binding : binding t =
  let* b, sp = span_of (one_of (error [ Text "Expected binding" ]) [ def ]) in
  pure (b sp)

let module_ : module_ t =
  let parse_module =
    let* () = accept Module in
    let* name = upper_identifier in
    let* () = expect Equal in
    let* () = expect LeftBrace in
    let* bindings = accumulate binding ~recover:toplevel in
    let* () = expect RightBrace in
    let* () = eof in
    pure (fun span -> Cst.Module.Module { name; bindings; span })
  in
  let* m, sp = span_of parse_module in
  pure (m sp)

let parse = Combinator.parse