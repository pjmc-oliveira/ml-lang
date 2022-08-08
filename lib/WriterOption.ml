module type Monoid = sig
  type t

  val empty : t
  val concat : t -> t -> t
end

module Make (W : Monoid) = struct
  type 'a t = 'a option * W.t

  let map f ((x, w) : 'a t) : 'b t = (Option.map f x, w)
  let pure x : 'a t = (Some x, W.empty)

  let apply ((mf, w) : ('a -> 'b) t) ((mx, w') : 'a t) : 'b t =
    match (mf, mx) with
    | Some f, Some x -> (Some (f x), W.concat w w')
    | _, _ -> (None, W.concat w w')

  let prod ((mx, w) : 'a t) ((my, w') : 'b t) : ('a * 'b) t =
    match (mx, my) with
    | Some x, Some y -> (Some (x, y), W.concat w w')
    | _, _ -> (None, W.concat w w')

  let bind ((mx, w) : 'a t) (f : 'a -> 'b t) : 'b t =
    match mx with
    | None -> (None, w)
    | Some x -> (
        match f x with
        | None, w' -> (None, W.concat w w')
        | Some y, w' -> (Some y, W.concat w w'))

  module Syntax = struct
    let ( let+ ) w f : 'b t = f w
    let ( and+ ) = prod
    let ( let* ) = bind
  end
end
