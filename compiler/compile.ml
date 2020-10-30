open Ast
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
let arg_regs = [
  Reg RDI; Reg RSI; Reg RDX; Reg RCX; Reg R8; Reg R9
]

(* INTEGER ERROR CODES *)
let not_a_number = Const 1L
let not_a_boolean = Const 2L 

(* FOREIGN FUNCTIONS *)
let foreign_functions : fun_env = [
  "print", 1;
  "min", 2;
  "min_of_8", 8;
]

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

(* Type Checker for a register and a type expected *)
let check_arg (register: arg) (type_error: arg) : instruction list =
  [IMov (error_code_register, type_error)] @
  [IMov (error_register, register)] @
  [ITest (error_register, Const 1L)] @
  if type_error == not_a_number then
  [IJnz "error_handler"] else [IJz "error_handler"]
  
(* Shortcut typechecker for binary operations *)
let check_binops (type_error: arg) =
  check_arg return_register type_error @ check_arg argument_register type_error

(* Compiles instructions common to binary operators. Moves left argument
into RAX (return_register), and the right one into R11 (argument_register).*)
let compile_binop_preamble (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compiled_right = compiler r env in
  let new_env, loc = extend_env (gensym "tmp") env in
  let compiled_left = compiler l new_env in
  compiled_right @ [ IMov (loc, return_register) ] @ 
  compiled_left  @ [ IMov (argument_register, loc) ]

(* Compiles and/or operators *)
let compile_shortcut_binop (l: expr) (r: expr) (env: env) (lbl: string) (skip_on: bool)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compare = [
    IMov (argument_register, Const (encode_bool skip_on)) ;
    ICmp (return_register, argument_register) ; 
    IJe lbl
  ] in
  compiler l env @ check_arg return_register not_a_boolean @ compare @ 
  compiler r env @ check_arg return_register not_a_boolean @ compare
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
 
let rec push_regs (n: int) (regs: arg list): instruction list =
  if n>0 then
    begin match regs with
    | [] -> []
    | r::tail -> [IPush r] @ push_regs (n-1) tail
    end
  else []

let rec pop_regs (n: int) (regs: arg list): instruction list =
  if n>0 then
    begin match regs with
    | [] -> []
    | r::tail -> [IPop r] @ pop_regs (n-1) tail
    end
  else []

let rec prepare_call (ins: instruction list list) (regs: arg list): instruction list list =
  match ins, regs with
  | [], _ -> []
  | i::tail_args, reg::tail_regs -> [i @ [IMov (reg, return_register)] ] @ prepare_call tail_args tail_regs
  | i::tail_args, [] -> [ i @ [IPush return_register]] @ prepare_call tail_args []


let rec instLL_to_instL (ins: instruction list list) =
  match ins with
  | [] -> []
  | x::tail -> x @ instLL_to_instL tail

(* let fun_asm_c (name: string) (args: arg list) (compile: expr -> env -> instruction list): instruction list =
  let args_prepared = List.rev(prepare_args args args_regs) in
  let arity = List.length args in
  let args_in_stack = arity - 6 in
  [ILabel (gensym name)] @
  [IPush return_register ] @
  args_prepared @
  [ICall name]  @
  if args_in_stack > 0 then [IAdd (Reg RSP, Const (Int64.of_int args_in_stack) )] else [] @
  [IPop (return_register)] @
  [ILabel (gensym ("end_"^name))]


let call_print () : instruction list =
  [ILabel (gensym "print")] @
  [IPush (return_register)] @
  [IMov (Reg RDI, return_register)] @
  [ICall "print_value"] @
  [IPop (return_register)] @
  [ILabel (gensym "end_print")] *)

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
  | Id x  -> [ IMov (return_register, lookup x env) ]
  | Let (id,v,b) -> 
      let (new_env, loc) = extend_env id env in
      let compiled_val = compile_expr v env in
      let save_val     = [ IMov(loc, return_register) ] in
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
      | Eq -> (* Doesn't need a type checking *)
        let eq_lbl =  gensym "eq" in
        compile_binop_preamble l r env compile_expr
        @ compile_binop_comparator eq_lbl (IJe eq_lbl)
      (* And & Or have shortucut semantics *)
      | And -> compile_shortcut_binop l r env (gensym "and") false compile_expr
      | Or ->  compile_shortcut_binop l r env (gensym "or")  true  compile_expr
    end
  | If (c, t, f) -> (compile_expr c env) @ check_arg return_register not_a_boolean @ compile_if compile_expr t f env
  | App (fname, args) ->
    let arity = fun_lookup fname env in
    let argc = List.length args in
    if (argc != arity)
      then Fmt.failwith 
        "Arity mismatch: function f expects %d arguments but got %d" arity argc
      else
        let curry_compile = fun expr -> compile_expr expr env in
        let arity = List.length args in
        let compiled_args = List.map curry_compile args in
        let pushed_regs   = List.rev (push_regs arity arg_regs) in
        let prepare_args  = List.rev (prepare_call compiled_args arg_regs) in 
        let prepare_call  = instLL_to_instL prepare_args in
        let restore_rsp   = if arity > 6 
          then [IAdd (Reg RSP, Const (Int64.of_int (8 * (arity - 6))))]
          else [] in
        let popped_regs   = pop_regs arity arg_regs in
        pushed_regs @ prepare_call @ [ICall fname] @ restore_rsp @ popped_regs
  | Void -> []


(* Label for handling errors *)
let error_handler =
 [ILabel "error_handler";
  IMov (Reg RSI, error_register);
  IMov (Reg RDI, error_code_register);
  ICall "error"]

(* Callee - Save  @ Init *)
let callee_prologue = [
  IPush (Reg RBP);
  IMov (Reg RBP, Reg RSP);
]

(* Callee - Save  @ End *)
let callee_epilogue = [
  IMov (Reg RSP, Reg RBP);
  IPop (Reg RBP)
]

let compile_declaration (d : decl) : instruction list =
  let (FunDef (fname, params, body)) = d in
  let lenv = make_function_let_env params empty_env in
  [ILabel fname] @ callee_prologue @ [ISub (Reg RSP, Const 160L)]
  @ compile_expr body (lenv, foreign_functions) @ callee_epilogue @ [IRet]

(* Generates the compiled program *)
let compile_prog : prog Fmt.t =
  fun fmt p ->
    match p with Program (decs, exp) ->
      let declarations = List.concat @@ List.map compile_declaration decs in
      let fenv = fun_env_from_decls decs foreign_functions in
      let instrs = compile_expr exp (empty_env, fenv) in
      let prelude ="
section .text
extern print
extern min
extern min_of_8
extern error
global our_code_starts_here
our_code_starts_here:" in
      Fmt.pf fmt "%s@\n%a" prelude pp_instrs 
        (callee_prologue @ [ISub (Reg RSP, Const 160L)] (* Change this *)
        @ instrs @ callee_epilogue @ [IRet] @ error_handler @ declarations)

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse_prog (sexp_list_from_string src)) compile_prog

(* let check_equal () =
  let lbl = gensym "boolean_first_op" in
  let done_lbl = gensym "done_type_checking" in
  [ITest (return_register, Const 1L)] @
  [IJnz lbl] @
  check_arg argument_register not_a_number @
  [IJmp done_lbl] @
  [ILabel lbl] @
  check_arg argument_register not_a_boolean @
  [ILabel done_lbl] *)
