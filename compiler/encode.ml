open Asm

(* constants *)
let true_encoding = 0x8000000000000001L
let false_encoding = 0x0000000000000001L
let bool_bit = 0x8000000000000000L
let max_int = Int64.div Int64.max_int 2L
let min_int = Int64.div Int64.min_int 2L

let encode_int (n: int64) : arg =
  if n > max_int || n < min_int then
    failwith ("Integer overflow: " ^ (Int64.to_string n))
  else
    Const (Int64.shift_left n 1)

let encode_bool (b: bool) : arg =
  if b then Const true_encoding else Const false_encoding