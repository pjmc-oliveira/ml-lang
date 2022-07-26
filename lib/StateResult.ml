type ('a, 's, 'e) t = 's -> ('a * 's, 'e) result

let pure x : ('a, 's, 'e) t = fun s -> Ok (x, s)

let map f p : ('b, 's, 'e) t =
 fun s ->
  match p s with
  | Ok (x, s') -> Ok (f x, s')
  | Error e -> Error e

let map_error f p : ('b, 's, 'e) t =
 fun s ->
  match p s with
  | Ok (x, s') -> Ok (x, s')
  | Error e -> Error (f e)

let bind (p : ('a, 's, 'e) t) (f : 'a -> ('b, 's, 'e) t) : ('b, 's, 'e) t =
 fun s ->
  match p s with
  | Ok (x, s') -> (f x) s'
  | Error e -> Error e

let prod tx ty = bind tx (fun x -> bind ty (fun y -> pure (x, y)))
let fail (e : 'e) : ('a, 's, 'e) t = fun _ -> Error e
let get : ('s, 's, 'e) t = fun s -> Ok (s, s)
let set s : (unit, 's, 'e) t = fun _ -> Ok ((), s)
let mut f : (unit, 's, 'e) t = fun s -> Ok ((), f s)
let scope (s : 's) (p : ('a, 's, 'e) t) : ('a, 's, 'e) t = fun _ -> p s

let guard cond err : (unit, 's, 'e) t =
  if cond then
    pure ()
  else
    fail err

let local (p : ('a, 's, 'e) t) : ('a, 's, 'e) t =
 fun s ->
  match p s with
  | Ok (x, _) -> Ok (x, s)
  | Error e -> Error e

let traverse_list (f : 'a -> ('b, 's, 'e) t) (xs : 'a list) :
    ('b list, 's, 'e) t =
  List.fold_right
    (fun x st s ->
      match f x s with
      | Error e -> Error e
      | Ok (y, s') -> (
          match st s' with
          | Error e -> Error e
          | Ok (ys, s'') -> Ok (y :: ys, s'')))
    xs (pure [])

module Syntax = struct
  let ( let* ) = bind
  let ( and* ) = prod
end