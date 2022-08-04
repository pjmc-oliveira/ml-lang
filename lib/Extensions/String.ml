include Stdlib.String

let left_pad ?(padding = ' ') n str =
  let str_len = length str in
  let pad_len = max (n - str_len) 0 in
  let pad = make pad_len padding in
  pad ^ str

let of_char c = make 1 c