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
  match e with 
  | Num n -> [ IMov (Reg RAX, encode_int n) ]
  | Bool p -> [ IMov (Reg RAX, encode_bool p) ]
  | UnOp (op, e) ->
    begin match op with
      | Add1 -> compile_expr e env @ [IAdd (Reg RAX, Const 2L)]
      | Sub1 -> compile_expr e env @ [ISub (Reg RAX, Const 2L)]
      | Not -> compile_expr e env @
        (* bool_bit is a 64-bit operand, so it must be moved into a
          register before use *)
        [IMov (Reg RBX, Const bool_bit) ; IAdd (Reg RAX, Reg RBX)]
    end
  | Id x  -> [ IMov (Reg RAX, RegOffset (RSP, lookup x env)) ]
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
        | _ -> Fmt.failwith "unrecognized binary operator: %a" pp_binop op
      in
      compiled_right @ save_right @ compiled_left @ apply_op
  | LazyBinOp (op, l, r) ->
    let compare, end_block =
    begin
      match op with
      | And -> [IMov (Reg RBX, Const false_encoding); ICmp (Reg RAX, Reg RBX)],
        [IMov (Reg RAX, Const true_encoding)]
      | Or -> [IMov (Reg RBX, Const true_encoding); ICmp (Reg RAX, Reg RBX)],
       [IMov (Reg RAX, Const false_encoding)]
      | _ -> Fmt.failwith "unrecognized lazy binary operator: %a" pp_binop op
    end in
    let label = gensym "L" in
    let jmp = [ IJe label ] in
    let end_label = [ ILabel label ] in
    let compiled_left =  compile_expr l env in
    let compiled_right = compile_expr r env in
    compiled_left @ compare @ jmp @ compiled_right @ compare @ jmp @ end_block @ end_label

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