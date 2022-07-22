include Stdlib.Char

let is_alpha_lower c = code 'a' <= code c && code c <= code 'z'
let is_alpha_upper c = code 'A' <= code c && code c <= code 'Z'
let is_digit c = code '0' <= code c && code c <= code '9'
let is_alpha c = is_alpha_lower c || is_alpha_upper c
let is_alphanum c = is_alpha c || is_digit c
let is_space c = c = ' ' || c = '\n' || c = '\t'