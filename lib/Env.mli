(** Represents the runtime environment *)

type 'a t
type key = string

val empty : 'a t
val insert : key -> 'a -> 'a t -> 'a t
val lookup : key -> 'a t -> 'a option
val remove : key -> 'a t -> 'a t
val of_list : (key * 'a) list -> 'a t
val to_list : 'a t -> (key * 'a) list
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val union : 'a t -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
