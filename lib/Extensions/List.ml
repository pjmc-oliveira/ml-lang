include Stdlib.List

let show f ls = "[" ^ String.concat "; " (map f ls) ^ "]"

let rec zip_with f xs ys =
  match (xs, ys) with
  | [], _ | _, [] -> []
  | x :: xs', y :: ys' -> f x y :: zip_with f xs' ys'

let zip xs ys : ('a * 'b) list = zip_with (fun x y -> (x, y)) xs ys
let singleton x = [ x ]

let unzip xys =
  let rec loop xs ys = function
    | [] -> (rev xs, rev ys)
    | (x, y) :: xys -> loop (x :: xs) (y :: ys) xys
  in
  loop [] [] xys

let rec drop n list =
  match (n, list) with
  | 0, _ -> list
  | _, [] -> []
  | n, _ :: list' -> drop (n - 1) list'

let take n list =
  let rec loop acc = function
    | 0, _ | _, [] -> rev acc
    | n, x :: xs -> loop (x :: acc) (n - 1, xs)
  in
  loop [] (n, list)

let slice start stop list = take (stop - start) (drop start list)

let rec range start stop =
  if start >= stop then [] else start :: range (start + 1) stop

let enumerate list =
  let rec loop n = function
    | [] -> []
    | x :: xs -> (n, x) :: loop (n + 1) xs
  in
  loop 0 list
