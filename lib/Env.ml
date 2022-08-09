module Str_map = Map.Make (String)

type 'a t = 'a Str_map.t
type key = String.t

let empty = Str_map.empty
let insert = Str_map.add
let lookup = Str_map.find_opt
let remove = Str_map.remove
let of_list pairs = Str_map.of_seq (List.to_seq pairs)
let to_list env = Str_map.to_seq env |> List.of_seq
let equal = Str_map.equal

let union left right =
  (* TODO: Which precedence does this have? *)
  Str_map.fold insert right left

let map = Str_map.map
