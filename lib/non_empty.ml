type 'a t = 'a * 'a list [@@deriving show { with_path = false }]

(** Create a Non_empty.t from a list.
    Throws if list is empty *)
let of_list : 'a list -> 'a t = function
  | h :: t -> (h, t)
  | [] -> failwith "Cannot create empty Non_empty.y"

let to_list ((h, t) : 'a t) = h :: t
let make h t : 'a t = (h, t)
let map f ((h, t) : 'a t) : 'b t = (f h, List.map f t)
let hd ((h, _) : 'a t) = h
let tl ((_, t) : 'a t) = t
let fold_left f acc ((h, t) : 'a t) = List.fold_left f acc (h :: t)
