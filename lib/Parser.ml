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

  module Syntax = struct
    let ( let+ ) = map
    let ( and+ ) = prod
    let ( let* ) = bind
    let ( and* ) = ( and+ )
  end

  open Syntax

  let rec some p =
    let* x = p in
    let* xs = many p in
    pure (x :: xs)

  and many p = alt (some p) (pure [])

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

  let span (p : 'a t) : ('a * Source.span) t =
    let* p1 = next_position in
    let* res = p in
    let* p2 = last_position in
    pure (res, Source.Span.between p1 p2)

  let eof : unit t = function
    | [], loc -> (Unconsumed, Ok ((), ([], loc)))
    | _ -> (Unconsumed, Error [ error [ Text "Expected EOF" ] ])

  let try_ p : 'a t =
   fun s ->
    match p s with
    | c, Ok (x, s') -> (c, Ok (x, s'))
    | _, Error e -> (Unconsumed, Error e)

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

  let identifier =
    let* tk = token in
    match tk with
    | Ident s -> pure s
    | _ ->
        let* pos = last_position in
        fail_lines ~location:pos
          [
            Text "Expected identifier"; Text ("But got: " ^ Token.to_string tk);
          ]
end

open Combinator
open Combinator.Syntax

let toplevel =
  let* () = drop_until (fun t -> t = Def) in
  pure ()

let rec expression () =
  let expr =
    one_of
      (error [ Text "Expected expression" ])
      [ let_in (); it_then_else (); atom () ]
  in
  let* expr, sp = span expr in
  pure (expr sp)

and let_in () =
  let* _ = accept Let in
  let* name = identifier in
  let* _ = expect Equal in
  let* def = expression () in
  let* _ = expect In in
  let* body = expression () in
  pure (fun span -> Cst.Expr.Let { name; def; body; span })

and it_then_else () =
  let* _ = accept If in
  let* cond = expression () in
  let* _ = expect Then in
  let* con = expression () in
  let* _ = expect Else in
  let* alt = expression () in
  pure (fun span -> Cst.Expr.If { cond; con; alt; span })

and atom () =
  let* tk = token in
  match tk with
  | Int value -> pure (fun span -> Cst.Expr.Int { value; span })
  | Bool value -> pure (fun span -> Cst.Expr.Bool { value; span })
  | Ident name -> pure (fun span -> Cst.Expr.Var { name; span })
  | LeftParen ->
      let* expr = expression () in
      let* _ = expect RightParen in
      pure (fun span -> Cst.Expr.map_span (fun _ -> span) expr)
  | _ -> fail_lines [ Text "Expected atom" ]

let def =
  let* () = accept Def in
  let* name = identifier in
  let* () = expect Equal in
  let* expr = expression () in
  pure (fun span -> Cst.Binding.Def { name; expr; span })

let binding =
  let* b, sp = span (one_of (error [ Text "Expected binding" ]) [ def ]) in
  pure (b sp)

let module_ =
  let parse_module =
    let* () = accept Module in
    let* name = identifier in
    let* () = expect Equal in
    let* () = expect LeftBrace in
    let* bindings = accumulate binding ~recover:toplevel in
    (* let* bindings = many binding in *)
    let* () = expect RightBrace in
    let* () = eof in
    pure (fun span -> Cst.Module.Module { name; bindings; span })
  in
  let* m, sp = span parse_module in
  pure (m sp)

let parse = Combinator.parse