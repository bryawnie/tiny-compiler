(* constants *)
let true_encoding = 0x8000000000000007L
let false_encoding = 0x0000000000000007L
let bool_bit = 0x8000000000000000L
let max_int = Int64.div Int64.max_int 2L
let min_int = Int64.div Int64.min_int 2L

(* Encodes Integers of 63 bits *)
let encode_int (n: int64) : int64 =
  if n > max_int || n < min_int then
    failwith ("Integer overflow: " ^ (Int64.to_string n))
  else
    Int64.shift_left n 1

(* Encodes Booleans *)
let encode_bool (b: bool) : int64 =
  if b then true_encoding else false_encoding