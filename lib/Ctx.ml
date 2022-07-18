module type S = sig
  type 'a t
  type key

  val empty : 'a t
  val insert : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a option
  val remove : key -> 'a t -> 'a t
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
end