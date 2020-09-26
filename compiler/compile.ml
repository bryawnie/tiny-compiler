open Expr
open Asm
open Env
open Encode

let gensym  =
  let counter = ref 0 in 
  (fun basename ->
    counter := !counter + 1;
    Format.sprintf "%s_%d" basename !counter)

let gensym_if  =
  let counter = ref 0 in 
  (fun () ->
    counter := !counter + 1;
    (Format.sprintf "if_false_%d" !counter, Format.sprintf "done_%d" !counter) )

let rec compile_expr (e : expr) (env: env) : instruction list =
(*(* pattern for binary operations (Add, Mul, etc...) *)
  let compile_binop (op: arg * arg -> instruction)
    (env: env) (e1:expr) (e2:expr) : instruction list =
    compile_expr e1 env @ [IMov (Reg RAX, Reg RBX)] @
    compile_expr e2 env @ [op (Reg RBX, Reg RAX)]
  in *)
  match e with 
  | Num n -> [ IMov (Reg RAX, encode_int n) ]
  | Bool p -> [ IMov (Reg RAX, encode_bool p) ]
  | UnOp (op, e) ->
    begin match op with
      | Add1 -> compile_expr e env @ [IAdd (Reg RAX, Const 2L)]
      | Sub1 -> compile_expr e env @ [ISub (Reg RAX, Const 2L)]
      | Not -> compile_expr e env @ [INeg (Reg RAX)]
    end
  | Id x  -> [ IMov (Reg RAX, RegOffset (RSP, lookup x env))]
(*   | And (p, q) -> compile_binop (function a, b -> IAnd(a,b)) env p q
  | Or (p, q) -> compile_binop (function a, b -> IOr(a,b)) env p q *)
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
        | Mul -> [IMul (Reg RAX, RegOffset (RSP, slot))]
          @ [ISar (Reg RAX, Const 1L)]
        | Div -> [IMov (Reg RBX, RegOffset (RSP, slot))]
          @ [IMov (Reg RDX, Const 0L)] @ [IDiv (Reg RBX)]
          @ [ISal (Reg RAX, Const 1L)]
        | And -> [IAnd (Reg RAX, RegOffset (RSP, slot))]
        | Or -> [IOr (Reg RAX, RegOffset (RSP, slot))]
      in
      compiled_right @ save_right @ compiled_left @ apply_op
  | If (c, t, f) -> 
    let (else_label, done_label) = gensym_if () in
    (compile_expr c env) @
    [
      ICmp (Reg RAX, Const false_encoding);
      IJe  (else_label)
    ]
    @ (compile_expr t env)
    @ [ IJmp (done_label); ILabel(else_label) ]
    @ (compile_expr f env)
    @ [ ILabel(done_label) ]


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