open Expr
open Asm
open Env
open Encode

(** Register usage conventions **)
(*  RAX = return value | first argument to a instruction
    R11 = second argument to a instruction *)
let return_register = Reg RAX
let argument_register = Reg R11
let error_code_register = Reg RBX
let error_register = Reg RCX

(* INTEGER ERROR CODES *)
let not_a_number = Const 1L
let not_a_boolean = Const 2L 

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


(* Compiles instructions common to binary operators. Moves left argument
into RAX (return_register), and the right one into R11 (argument_register).*)
let compile_binop_preamble (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compiled_right = compiler r env in
  let new_env, slot = extend_env (gensym "tmp") env in
  let compiled_left = compiler l new_env in
  compiled_right @ [ IMov (RegOffset (RBP, slot), return_register) ] @ compiled_left
  @ [ IMov (argument_register, RegOffset (RBP, slot)) ]

(* Compiles and/or operators *)
let compile_shortcut_binop (l: expr) (r: expr) (env: env) (lbl: string) (skip_on: bool)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compare = [
    IMov (argument_register, Const (encode_bool skip_on)) ;
    ICmp (return_register, argument_register) ; 
    IJe lbl
  ] in
  compiler l env @ compare @ compiler r env @ compare
  @ [IMov (return_register, Const (encode_bool (not skip_on))) ; ILabel lbl]

(* Compiles < and = comparators *)
let compile_binop_comparator (cmp_label: string) (inst: instruction) : instruction list =
  let preamble  =  [
    ICmp (return_register, argument_register) ; 
    IMov (return_register, Const true_encoding) 
  ] in 
  let ending    =  [
    IMov (return_register, Const false_encoding) ; 
    ILabel cmp_label 
  ] in 
  preamble @ [inst] @ ending
  
(* Compiles IF expressions *)        
let compile_if (compile: expr -> env -> instruction list) (t: expr) (f: expr) (env: env): instruction list =
  let false_lbl = gensym "if" in
  let done_lbl  = gensym "done" in 
  let preamble  = [
    ICmp (return_register, Const false_encoding);
    IJe  (false_lbl)
  ] in
  preamble
  @ (compile t env)
  @ [ IJmp (done_lbl); ILabel(false_lbl) ]
  @ (compile f env)
  @ [ ILabel (done_lbl) ]

let check_arg (register: arg) (type_error: arg) : instruction list =
  [IMov (error_code_register, type_error)] @
  [IMov (error_register, register)] @
  [ITest (error_register, Const 1L)] @
  if type_error == not_a_number then
  [IJnz "error_handler"] else [IJz "error_handler"]
  
let check_binops (type_error: arg) =
  check_arg return_register type_error @ check_arg argument_register type_error

(* THE MAIN compiler function *)
let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n -> [ IMov (return_register, Const (encode_int n)) ]
  | Bool p -> [ IMov (return_register, Const (encode_bool p)) ]
  | UnOp (op, e) ->
    begin match op with
      | Add1 -> compile_expr e env @ check_arg return_register not_a_number @ [IAdd (return_register, Const 2L)]
      | Sub1 -> compile_expr e env @ check_arg return_register not_a_number @ [ISub (return_register, Const 2L)]
      | Not ->  compile_expr e env @ check_arg return_register not_a_boolean @ [IMov (argument_register, Const bool_bit) ;
        IXor (return_register, argument_register)]
        (* bool_bit is a 64-bit value, so it must be moved into a register
        before use as an operand *)
    end
  | Id x  -> [ IMov (return_register, RegOffset (RBP, lookup x env)) ]
  | Let (id,v,b) -> 
      let (new_env, slot) = extend_env id env in
      let compiled_val = compile_expr v env in
      let save_val     = [ IMov(RegOffset (RBP, slot), return_register) ] in
      compiled_val @ save_val @ (compile_expr b new_env)
  | BinOp (op, l, r) ->
    begin
      match op with
      | Add -> compile_binop_preamble l r env compile_expr 
        @ check_binops not_a_number (* Type Checking *)
        @ [IAdd (return_register, argument_register)]
      | Sub -> compile_binop_preamble l r env compile_expr
        @ check_binops not_a_number (* Type Checking *)
        @ [ISub (return_register, argument_register)]
      | Mul -> compile_binop_preamble l r env compile_expr
        @ check_binops not_a_number (* Type Checking *)
        @ [IMul (return_register, argument_register) ; ISar (return_register, Const 1L)]
      | Div -> compile_binop_preamble l r env compile_expr
        @ check_binops not_a_number (* Type Checking *)
        @ [ICqo ; IDiv (argument_register) ; ISal (return_register, Const 1L)]
      | Less -> 
        let less_lbl = gensym "less" in
        compile_binop_preamble l r env compile_expr 
        @ check_binops not_a_number (* Type Checking *)
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

let error_handler =
 [ILabel "error_handler";
  IMov (Reg RSI, error_register);
  IMov (Reg RDI, error_code_register);
  ICall "error"]

(* Generates the compiled program *)
let compile_prog : expr Fmt.t =
  fun fmt e ->
  let instrs = compile_expr e empty_env in
  let prelude ="
section .text
extern error
global our_code_starts_here
our_code_starts_here:
  push RBP
  mov  RBP, RSP
  sub  RSP, 80" in (* Momentáneo *)
  let epilogue = "
  mov  RSP, RBP
  pop  RBP
  ret" in
Fmt.pf fmt "%s@\n%a%s\n%a" prelude pp_instrs instrs epilogue pp_instrs error_handler

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse (sexp_from_string src)) compile_prog