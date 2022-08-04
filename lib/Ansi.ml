(** This module handles using ANSI terminal escape codes *)

let escape s = "\u{001b}" ^ s

type color = Red | Cyan | NoColor

let reset = escape "[0m"

let code_of_color = function
  | Red -> escape "[31m"
  | Cyan -> escape "[36m"
  | NoColor -> ""

let pretty ?(color = NoColor) s = code_of_color color ^ s ^ reset
