(** Represents the compile-time context *)

type t
type key = string

val empty : t
val insert : key -> Type.poly -> t -> t
val lookup : key -> t -> Type.poly option
val insert_ty : key -> Type.kind -> t -> t
val lookup_ty : key -> t -> Type.kind option
val lookup_constructors : key -> t -> string list option
val of_list : terms:(key * Type.poly) list -> types:(key * Type.kind) list -> t
val of_terms_list : (key * Type.poly) list -> t
val of_types_list : (key * Type.kind) list -> t
val of_constructors_list : (key * string list) list -> t
val to_terms_list : t -> (key * Type.poly) list
val to_types_list : t -> (key * Type.kind) list
val tm_equal : (Type.poly -> Type.poly -> bool) -> t -> t -> bool
val ty_equal : (Type.kind -> Type.kind -> bool) -> t -> t -> bool
val union : t -> t -> t
