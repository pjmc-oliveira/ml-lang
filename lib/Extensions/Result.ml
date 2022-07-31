include Stdlib.Result

module Syntax = struct
  let ( let+ ) = Stdlib.Result.map

  let ( and+ ) tx ty =
    match (tx, ty) with
    | Ok x, Ok y -> Ok (x, y)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  let ( let* ) = Stdlib.Result.bind
  let ( and* ) = ( and+ )
end

let traverse_list (f : 'a -> ('b, 'e) t) (xs : 'a list) : ('b list, 'e) result =
  List.fold_right
    (fun x next ->
      match f x with
      | Error e -> Error e
      | Ok y -> (
          match next with
          | Error e -> Error e
          | Ok ys -> Ok (y :: ys)))
    xs (Ok [])