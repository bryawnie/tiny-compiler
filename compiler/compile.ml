open Expr
open Asm
open Env
open Encode

(* 
  A gensym for standard symbols and specific instructions
  (Hope it provides a better understanding of generated asm) 
*)
let gensym  =
  let counter = ref 0 in 
  let if_counter = ref 0 in 
  let eq_counter = ref 0 in
  let less_counter = ref 0 in  
  (fun basename ->
    match basename with
    | "if" -> (if_counter := !if_counter + 1;
      Format.sprintf "if_false_%d" !if_counter)
    | "done" -> Format.sprintf "done_%d" !if_counter
    | "eq" -> (eq_counter := !eq_counter + 1;
      Format.sprintf "eq_%d" !eq_counter )
    | "less" -> (less_counter := !less_counter + 1;
    Format.sprintf "less_%d" !less_counter )
    | _ -> (counter := !counter + 1;
      Format.sprintf "%s_%d" basename !counter))


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
  let compare = [
    IMov (Reg RBX, Const (encode_bool skip_on)) ;
    ICmp (Reg RAX, Reg RBX) ; 
    IJe lbl
  ] in
  compiler l env @ compare @ compiler r env @ compare
  @ [IMov (Reg RAX, Const (encode_bool (not skip_on))) ; ILabel lbl]

(* Compiles < and = comparators *)
let compile_binop_comparator (cmp_label: string) (inst: instruction) : instruction list =
  let preamble  =  [
    ICmp (Reg RAX, Reg RBX) ; 
    IMov (Reg RAX, Const true_encoding) 
  ] in 
  let ending    =  [
    IMov (Reg RAX, Const false_encoding) ; 
    ILabel cmp_label 
  ] in 
  preamble @ [inst] @ ending
  
(* Compiles IF expressions *)        
let compile_if (compile: expr -> env -> instruction list) (t: expr) (f: expr) (env: env): instruction list =
  let false_lbl = gensym "if" in
  let done_lbl  = gensym "done" in 
  let preamble  = [
    ICmp (Reg RAX, Const false_encoding);
    IJe  (false_lbl)
  ] in
  preamble
  @ (compile t env)
  @ [ IJmp (done_lbl); ILabel(false_lbl) ]
  @ (compile f env)
  @ [ ILabel (done_lbl) ]


(* THE MAIN compiler function *)
let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const (encode_int n)) ]
  | Bool p -> [ IMov (Reg RAX, Const (encode_bool p)) ]
  | UnOp (op, e) ->
    begin match op with
      | Add1 -> compile_expr e env @ [IAdd (Reg RAX, Const 2L)]
      | Sub1 -> compile_expr e env @ [ISub (Reg RAX, Const 2L)]
      | Not -> compile_expr e env @ [IMov (Reg RBX, Const bool_bit) ; IAdd (Reg RAX, Reg RBX)]
        (* bool_bit is a 64-bit operand, so it must be moved into a
          register before use *)
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
        let less_lbl = gensym "less" in
        compile_binop_preamble l r env compile_expr 
        @ compile_binop_comparator less_lbl (IJl less_lbl)
      | Eq ->
        let eq_lbl =  gensym "eq" in
        compile_binop_preamble l r env compile_expr
        @ compile_binop_comparator eq_lbl (IJe eq_lbl)
      (* And & Or have shortucut semantics *)
      | And -> compile_shortcut_binop l r env (gensym "and") false compile_expr
      | Or ->  compile_shortcut_binop l r env (gensym "or")  true  compile_expr
    end
  | If (c, t, f) -> (compile_expr c env) @ compile_if compile_expr t f env
 

(* Generates the compiled program *)
let compile_prog : expr Fmt.t =
  fun fmt e ->
  let instrs = compile_expr e empty_env in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
Fmt.pf fmt "%s@\n%a" prelude pp_instrs (instrs @ [ IRet ])

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse (sexp_from_string src)) compile_prog