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

let gensym_less  =
  let counter = ref 0 in 
  (fun () ->
    counter := !counter + 1;
    Format.sprintf "less_%d" !counter )

let gensym_eq  =
  let counter = ref 0 in 
  (fun () ->
    counter := !counter + 1;
    Format.sprintf "eq_%d" !counter )

(* Compiles instructions common to binary operators *)
let compile_binop_preamble (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compiled_right = compiler r env in
  let new_env, slot = extend_env (gensym "tmp") env in
  let compiled_left = compiler l new_env in
  compiled_right @ [ IMov (RegOffset (RSP, slot), Reg RAX) ] @ compiled_left
  @ [ IMov (Reg RBX, RegOffset (RSP, slot)) ]

(* Compiles and/or operators *)
let compile_shortcut_binop (l: expr) (r: expr) (env: env) (lbl: string) (skip_on: bool)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compare = [IMov (Reg RBX, Const (encode_bool skip_on)) ;
    ICmp (Reg RAX, Reg RBX) ; IJe lbl] in
  compiler l env @ compare @ compiler r env @ compare
  @ [IMov (Reg RAX, Const (encode_bool (not skip_on))) ; ILabel lbl]

let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const (encode_int n)) ]
  | Bool p -> [ IMov (Reg RAX, Const (encode_bool p)) ]
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
    begin
      match op with
      | Add -> compile_binop_preamble l r env compile_expr 
        @ [IAdd (Reg RAX, Reg RBX)]
      | Sub -> compile_binop_preamble l r env compile_expr
        @ [ISub (Reg RAX, Reg RBX)]
      | Mul -> compile_binop_preamble l r env compile_expr
        @ [IMul (Reg RAX, Reg RBX) ; ISar (Reg RAX, Const 1L)]
      | Div -> compile_binop_preamble l r env compile_expr
        @ [IMov (Reg RDX, Const 0L) ; IDiv (Reg RBX) ; ISal (Reg RAX, Const 1L)]
      | Less -> 
        let less_lbl = gensym_less () in
        compile_binop_preamble l r env compile_expr @
        [ ICmp (Reg RAX, Reg RBX) ;
          IMov (Reg RAX, Const true_encoding) ;
          IJl less_lbl ;
          IMov (Reg RAX, Const false_encoding) ;
          ILabel less_lbl ]
      | Eq ->
        let eq_lbl =  gensym_less () in
        compile_binop_preamble l r env compile_expr @
        [ ICmp (Reg RAX, Reg RBX) ;
          IMov (Reg RAX, Const true_encoding) ;
          IJe eq_lbl ;
          IMov (Reg RAX, Const false_encoding) ;
          ILabel eq_lbl]
      (* And & Or have shortucut semantics *)
      | And -> compile_shortcut_binop l r env (gensym "and") false compile_expr
      | Or -> compile_shortcut_binop l r env (gensym "or") true compile_expr
    end
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