module Combinator = struct
  type tokens = (Token.t * Loc.t) list
  type 'a t = tokens -> ('a * tokens, Error.t list) result

  let pure x : 'a t = fun tks -> Ok (x, tks)
  let fail e : 'a t = fun _ -> Error e
  let error ?location lines : Error.t = { kind = Parser; lines; location }
  let fail_lines ?location lines = fail [ error ?location lines ]
  let parse (p : 'a t) tks = p tks

  let map f (p : 'a t) : 'b t =
   fun s -> match p s with Ok (x, s') -> Ok (f x, s') | Error e -> Error e

  let prod (p : 'a t) (q : 'b t) : ('a * 'b) t =
   fun s ->
    match p s with
    | Error e -> Error e
    | Ok (x, s') -> (
        match q s' with Error e -> Error e | Ok (y, s'') -> Ok ((x, y), s''))

  let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s -> match p s with Error e -> Error e | Ok (x, s') -> (f x) s'

  let alt (p : 'a t) (q : 'a t) : 'a t =
   fun s -> match p s with Error _ -> q s | Ok (x, s') -> Ok (x, s')

  let one_of (err : 'e) (ps : 'a t list) : 'a t =
    List.fold_right alt ps (fun _ -> Error err)

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

  let token : Token.t t = function
    | [] -> Error [ error [ Text "Unexpected EOF" ] ]
    | (tk, _loc) :: tks -> Ok (tk, tks)

  let position : Loc.t t =
   fun tks ->
    match tks with
    | [] -> Error [ error [ Text "Unexpected EOF" ] ]
    | (_tk, loc) :: _tks -> Ok (loc, tks)

  let span (p : 'a t) : ('a * Loc.t) t =
    let* p1 = position in
    let* res = p in
    let* p2 = position in
    pure (res, Loc.between p1 p2)

  let eof : unit t = function
    | [] -> Error [ error [ Text "Unexpected EOF" ] ]
    | [ (Eof, _loc) ] -> Ok ((), [])
    | _ -> Error [ error [ Text "Expected EOF" ] ]

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

let atom =
  let* tk = token in
  match tk with
  | Int n -> pure (Ast.Const n)
  | Ident s -> pure (Ast.Var s)
  | _ -> fail_lines [ Text "Unexpected token" ]

let expression tks = atom tks

let def =
  let* name = identifier in
  let* () = expect Equal in
  let* expr = expression in
  pure (Ast.Def { name; expr })

let binding =
  let* tk = token in
  match tk with Def -> def | _ -> fail_lines [ Text "Unexpected token" ]

let module_ =
  let* () = expect Module in
  let* name = identifier in
  let* () = expect Equal in
  let* () = expect LeftBrace in
  let* bindings = many binding in
  let* () = expect RightBrace in
  let* () = eof in
  pure (Ast.Module { name; bindings })

let parse = Combinator.parse