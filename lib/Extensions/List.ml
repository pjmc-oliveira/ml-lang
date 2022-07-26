include Stdlib.List

let show f ls = "[" ^ String.concat "; " (map f ls) ^ "]"

let rec zip_with f xs ys =
  match (xs, ys) with
  | [], _ | _, [] -> []
  | x :: xs', y :: ys' -> f x y :: zip_with f xs' ys'

let zip xs ys : ('a * 'b) list = zip_with (fun x y -> (x, y)) xs ys
let singleton x = [ x ]
