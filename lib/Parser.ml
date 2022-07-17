module Combinator = struct
  type tokens = (Token.t * Loc.t) list
  type errors = Error.t list

  module Consumed = struct
    type t = Consumed | Unconsumed

    let both l r = match l with Consumed -> Consumed | _ -> r
  end

  type 'a t = tokens -> Consumed.t * ('a * tokens, errors) result

  let pure x : 'a t = fun tks -> (Unconsumed, Ok (x, tks))
  let fail e : 'a t = fun _ -> (Unconsumed, Error e)
  let error ?location lines : Error.t = { kind = Parser; lines; location }
  let fail_lines ?location lines = fail [ error ?location lines ]

  let parse (p : 'a t) tks : ('a, errors) result =
    match p tks with
    | _, Error errs -> Error errs
    | _, Ok (x, []) -> Ok x
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
    | [] -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | (tk, _loc) :: tks -> (Consumed, Ok (tk, tks))

  let position : Loc.t t =
   fun tks ->
    match tks with
    | [] -> (Unconsumed, Error [ error [ Text "Unexpected EOF" ] ])
    | (_tk, loc) :: _tks -> (Unconsumed, Ok (loc, tks))

  let span (p : 'a t) : ('a * Loc.t) t =
    let* p1 = position in
    let* res = p in
    let* p2 = position in
    pure (res, Loc.between p1 p2)

  let eof : unit t = function
    | [] -> (Unconsumed, Ok ((), []))
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
      let* pos = position in
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
        let* pos = position in
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

let atom =
  let* tk = token in
  match tk with
  | Int n -> pure (Ast.Const n)
  | Ident s -> pure (Ast.Var s)
  | _ -> fail_lines [ Text "Expected atom" ]

let expression tks = atom tks

let def =
  let* () = accept Def in
  let* name = identifier in
  let* () = expect Equal in
  let* expr = expression in
  pure (Ast.Def { name; expr })

let binding = one_of (error [ Text "Expected binding" ]) [ def ]

let module_ =
  let* () = accept Module in
  let* name = identifier in
  let* () = expect Equal in
  let* () = expect LeftBrace in
  let* bindings = accumulate binding ~recover:toplevel in
  (* let* bindings = many binding in *)
  let* () = expect RightBrace in
  let* () = eof in
  pure (Ast.Module { name; bindings })

let parse = Combinator.parse