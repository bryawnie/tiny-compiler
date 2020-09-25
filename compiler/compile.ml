open Expr
open Asm

let gensym =
  let counter = ref 0 in 
  (fun basename ->
    counter := !counter + 1;
    Format.sprintf "%s_%d" basename !counter)

(* constants *)
let true_encoding = -1L (* 0b1111...1111 *)
let false_encoding = 1L (* 0b0000...0001 *)
let max_int = Int64.div Int64.max_int 2L
let min_int = Int64.div Int64.min_int 2L

let encode_int (n: int64) : arg =
  if n > max_int || n < min_int then
    failwith ("Integer overflow: " ^ (Int64.to_string n))
  else
    Const (Int64.shift_left n 1)

let encode_bool (b: bool) : arg =
  if b then Const true_encoding else Const false_encoding

let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, encode_int n) ]
  | Bool p -> [ IMov (Reg RAX, encode_bool p) ]
  | Id x  -> [ IMov (Reg RAX, RegOffset (RSP, lookup x env))]
  | Add1 e -> compile_expr e env @ [IAdd (Reg RAX, Const 2L)]
  | Sub1 e -> compile_expr e env @ [ISub (Reg RAX, Const 2L)]
  | Let (id,v,b) -> 
      let (new_env, slot) = extend_env id env in
      let compiled_val = compile_expr v env in
      let save_val     = [ IMov(RegOffset (RSP, slot), Reg RAX) ] in
      compiled_val @ save_val @ (compile_expr b new_env)
  | BinOp (op,l,r) -> 
      let compiled_right = compile_expr r env in
      let (new_env,slot) = extend_env (gensym "tmp") env in
      let save_right     = [ IMov(RegOffset (RSP, slot), Reg RAX) ] in
      let compiled_left  = compile_expr l new_env in
      let apply_op  =
        match op with
        | Add -> [IAdd (Reg RAX, RegOffset (RSP, slot))]
        | Sub -> [ISub (Reg RAX, RegOffset (RSP, slot))]
        | Mul -> [IMul (Reg RAX, RegOffset (RSP, slot))] @ [ISar (Reg RAX, Const 1L)]
      in
      compiled_right @ save_right @ compiled_left @ apply_op


let compile_prog : expr Fmt.t =
  fun fmt e ->
  let instrs = compile_expr e empty_env in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
Fmt.pf fmt "%s@\n%a" prelude pp_instrs (instrs @ [ IRet ])


let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse (sexp_from_string src)) compile_prog