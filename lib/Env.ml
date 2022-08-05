module StrMap = Map.Make (String)

type 'a t = 'a StrMap.t
type key = String.t

let empty = StrMap.empty
let insert = StrMap.add
let lookup = StrMap.find_opt
let remove = StrMap.remove
let of_list pairs = StrMap.of_seq (List.to_seq pairs)
let to_list env = StrMap.to_seq env |> List.of_seq
let equal = StrMap.equal

let union left right =
  (* TODO: Which precedence does this have? *)
  StrMap.fold insert right left

let map = StrMap.map
