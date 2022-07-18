type ('a, 's, 'e) t = 's -> ('a * 's, 'e) result

let pure x : ('a, 's, 'e) t = fun s -> Ok (x, s)

let map f p : ('b, 's, 'e) t =
 fun s ->
  match p s with
  | Ok (x, s') -> Ok (f x, s')
  | Error e -> Error e

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

module Syntax = struct
  let ( let* ) = bind
  let ( and* ) = prod
end