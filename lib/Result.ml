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