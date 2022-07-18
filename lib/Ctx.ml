module type S = sig
  type 'a t
  type key

  val empty : 'a t
  val insert : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a option
  val remove : key -> 'a t -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module type Ord = sig
  type t

  val compare : t -> t -> int
end

module Make (Key : Ord) : S with type key = Key.t = struct
  module KeyMap = Map.Make (Key)

  type 'a t = 'a KeyMap.t
  type key = Key.t

  let empty = KeyMap.empty
  let insert = KeyMap.add
  let lookup = KeyMap.find_opt
  let remove = KeyMap.remove
  let of_list pairs = KeyMap.of_seq (List.to_seq pairs)
  let to_list ctx = KeyMap.to_seq ctx |> List.of_seq
  let equal = KeyMap.equal
end