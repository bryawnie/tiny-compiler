open Expr
open Asm

let rec compile_expr (e : expr) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const n) ] 
  | Add1 e -> compile_expr e @ [IAdd (Reg RAX, Const 1L)]
  | Sub1 e -> compile_expr e @ [IAdd (Reg RAX, Const (-1L))]


let compile_prog : expr Fmt.t =
  fun fmt e ->
  let instrs = compile_expr e in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
Fmt.pf fmt "%s@\n%a" prelude pp_instrs (instrs @ [ IRet ])


let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse (sexp_from_string src)) compile_prog