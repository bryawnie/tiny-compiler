open Asm

(* constants *)
let true_encoding = 0x8000000000000001L
let false_encoding = 0x0000000000000001L
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


(* TYPE TAGS *)
let int_tag = Const 0L (* All values ending in 0 are integers *)
let bool_tag = Const 1L (* 001 - boolean value *)
let tuple_tag = Const 3L (* 011 - tuple pointer *)
let record_tag = Const 5L (* 101 - record pointer *) 
let function_tag = Const 7L (* 111 - function pointer*)
let tag_bitmask = Const 7L (*111*)