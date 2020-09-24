open Expr
open Asm

let gensym =
  let counter = ref 0 in 
  (fun basename ->
    counter := !counter + 1;
    Format.sprintf "%s_%d" basename !counter)

let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const n) ] 
  | Id x  -> [ IMov (Reg RAX, RegOffset (RSP, lookup x env))]
  | Add1 e -> compile_expr e env @ [IAdd (Reg RAX, Const 1L)]
  | Sub1 e -> compile_expr e env @ [IAdd (Reg RAX, Const (-1L))]
  | Let (id,v,b) -> 
      let (new_env, slot) = extend_env id env in
      let compiled_val = compile_expr v env in
      let save_val     = [ IMov(RegOffset (RSP, slot), Reg RAX) ] in
      compiled_val @ save_val @ (compile_expr b new_env)
  | BinOp (_,l,r) -> (* Solo sumas de momento *)
      let compiled_right = compile_expr r env in
      let (new_env,slot) = extend_env (gensym "tmp") env in
      let save_right     = [ IMov(RegOffset (RSP, slot), Reg RAX) ] in
      let compiled_left  = compile_expr l new_env in
      compiled_right @ save_right @ compiled_left @ [IAdd (Reg RAX, RegOffset (RSP, slot))]


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