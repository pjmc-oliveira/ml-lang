(** This module handles using ANSI terminal escape codes *)

let escape s = "\u{001b}" ^ s

type color =
  | Red
  | Cyan
  | Green
  | NoColor

type font =
  | Bold
  | NoFont

let reset = escape "[0m"

let code_of_font = function
  | Bold -> escape "[1m"
  | NoFont -> ""

let code_of_color = function
  | Red -> escape "[31m"
  | Cyan -> escape "[36m"
  | Green -> escape "[32m"
  | NoColor -> ""

let pretty ?(color = NoColor) ?(font = NoFont) s =
  code_of_font font ^ code_of_color color ^ s ^ reset
