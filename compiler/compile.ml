open Ast
open Asm
open Env
open Encode
open Gensym

(** Register usage conventions **)
(*  
  RAX = return value | first argument to a instruction
  R11 = second argument to a instruction
  RBX = Error code register
  RCX = Error var register 
*)
let ret_reg = Reg RAX
let arg_reg = Reg R11
let err_cod_reg = Reg RBX
let err_reg = Reg RCX
let heap_reg = R15
let arg_regs = [
  Reg RDI; Reg RSI; Reg RDX; Reg RCX; Reg R8; Reg R9
]

(* INTEGER ERROR CODES *)
let not_a_number = Const 1L
let not_a_boolean = Const 2L 
let not_a_tuple = Const 3L


(* This function handles the request for a type checking *)
let type_checking (register: arg) (type_expected: dtype): instruction list =
  let prelude (type_error) = [IMov (err_cod_reg, type_error) ; IMov (err_reg, register)] in
  match type_expected with
  | IntT -> 
    prelude (not_a_number)  @ [ITest (err_reg, Const 1L) ; IJnz "error_handler"]
  | BoolT -> 
    prelude (not_a_boolean) @
    [ITest (err_reg, Const 1L) ; IJz  "error_handler"] @ (* Not Integer *)
    [ITest (err_reg, Const 7L) ; IJz  "error_handler"]
  | TupleT -> 
    prelude (not_a_tuple) @
    [ITest (err_reg, Const 1L) ; IJz  "error_handler"] 
  | AnyT -> [] 
  
(* Returns the instruction list to backup the necesary registers before a function call *)
let rec push_regs (n: int) (regs: arg list): instruction list =
  if n>0 then
    begin match regs with
    | [] -> []
    | r::tail -> [IPush r] @ push_regs (n-1) tail
    end
  else []

(* Returns the instruction list to restore the necesary registers after a function call *)
let rec pop_regs (n: int) (regs: arg list): instruction list =
  if n>0 then
    begin match regs with
    | [] -> []
    | r::tail -> [IPop r] @ pop_regs (n-1) tail
    end
  else []

(* Prelude of callee to prepare a call *)
let rec prepare_call ?(types= []) (ins: instruction list list) (regs: arg list) (check: bool) : instruction list list =
  match ins, regs with
  | [], _ -> []
  | ins::tail_args, reg::tail_regs -> 
    [ins @ (if check then type_checking ret_reg (List.hd types) else []) @ [IMov (reg, ret_reg)] ] 
    @ prepare_call tail_args tail_regs check ~types:(if check then (List.tl types) else types)
  | ins::tail_args, [] -> 
    [ ins @ (if check then type_checking ret_reg (List.hd types) else []) @ [IPush ret_reg]] 
    @ prepare_call tail_args [] check ~types:(if check then (List.tl types) else types)


(* -----------------------------------
  |                UNOPS             |                
  ------------------------------------*)

(* 
  -- Compiles unop expressions --
  They can be Add1, Sub1, Not (boolean unop)
  To do not, we make a XOR between boolean bit and the var.
  Bool_bit is a 64-bit value, so it must be moved into a register before use as an operand.
*)
let compile_unop (expr: instruction list) (op: unOp): instruction list =
  match op with
  | Add1 -> expr @ type_checking ret_reg IntT @ [IAdd (ret_reg, Const 2L)]
  | Sub1 -> expr @ type_checking ret_reg IntT @ [ISub (ret_reg, Const 2L)]
  | Not ->  expr @ type_checking ret_reg BoolT @ [IMov (arg_reg, Const bool_bit) ; IXor (ret_reg, arg_reg)]


(* -----------------------------------
  |                BINOPS             |                
  ------------------------------------*)

(* Shortcut typechecker for binary operations *)
let check_binops (type_error: dtype) =
  type_checking ret_reg type_error @ type_checking arg_reg type_error

(* Compiles instructions common to binary operators. Moves left argument
into RAX (ret_reg), and the right one into R11 (arg_reg).*)
let compile_binop_preamble (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compiled_right = compiler r env in
  let new_env, loc = extend_env (tmp_gensym ()) env in
  let compiled_left = compiler l new_env in
  compiled_right @ [ IMov (loc, ret_reg) ] @ 
  compiled_left  @ [ IMov (arg_reg, loc) ]

(* Compiles and/or operators *)
let compile_shortcut_binop (l: expr) (r: expr) (env: env) (lbl: string) (skip_on: bool)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compare = [
    IMov (arg_reg, Const (encode_bool skip_on)) ;
    ICmp (ret_reg, arg_reg) ; 
    IJe lbl
  ] in
  compiler l env @ type_checking ret_reg BoolT @ compare @ 
  compiler r env @ type_checking ret_reg BoolT @ compare
  @ [IMov (ret_reg, Const (encode_bool (not skip_on))) ; ILabel lbl]

(* Compiles < and = comparators *)
let compile_binop_comparator (cmp_label: string) (inst: instruction) : instruction list =
  let preamble  =  [
    ICmp (ret_reg, arg_reg) ; 
    IMov (ret_reg, Const true_encoding) 
  ] in 
  let ending    =  [
    IMov (ret_reg, Const false_encoding) ; 
    ILabel cmp_label 
  ] in 
  preamble @ [inst] @ ending

(* 
  Compiles and does the type-checking of arithmetical binop expressions.
*)
let comp_and_check_arithmetic_binop (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list = 
  compile_binop_preamble l r env compiler
  @ check_binops IntT

(* 
  -- Compiles binop expressions --
  They can be + - * / < = and or
  Arithmetical binops (such as + - * /) are derivated to comp_and_check_arithmetic_binop
*)
let compile_binop (op: binOp) (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list = 
  match op with
    | Add -> comp_and_check_arithmetic_binop l r env compiler
      @ [IAdd (ret_reg, arg_reg)]
    | Sub -> comp_and_check_arithmetic_binop l r env compiler
      @ [ISub (ret_reg, arg_reg)]
    | Mul -> comp_and_check_arithmetic_binop l r env compiler
      @ [IMul (ret_reg, arg_reg) ; ISar (ret_reg, Const 1L)]
    | Div -> comp_and_check_arithmetic_binop l r env compiler
      @ [ICqo ; IDiv (arg_reg) ; ISal (ret_reg, Const 1L)]
    | Less -> 
      let less_lbl = less_gensym () in
      compile_binop_preamble l r env compiler 
      @ check_binops IntT (* Type Checking *)
      @ compile_binop_comparator less_lbl (IJl less_lbl)
    | Eq -> (* Doesn't need a type checking *)
      let eq_lbl =  equal_gensym () in
      compile_binop_preamble l r env compiler
      @ compile_binop_comparator eq_lbl (IJe eq_lbl)
    (* And & Or have shortucut semantics *)
    | And -> compile_shortcut_binop l r env (and_gensym ()) false compiler
    | Or ->  compile_shortcut_binop l r env (or_gensym ())  true  compiler


(* -----------------------------------
  |                 IF               |                
  ------------------------------------*)

(* Compiles IF expressions *)        
let compile_if (compile: expr -> env -> instruction list) (t: expr) (f: expr) (env: env): instruction list =
  let false_lbl = if_false_gensym () in
  let done_lbl  = done_gensym () in 
  let preamble  = [
    ICmp (ret_reg, Const false_encoding);
    IJe  (false_lbl)
  ] in
  preamble
  @ (compile t env)
  @ [ IJmp (done_lbl); ILabel(false_lbl) ]
  @ (compile f env)
  @ [ ILabel (done_lbl) ]
 

(* -----------------------------------
  |        FOREIGN FUNCTIONS (C)      |                
  ------------------------------------*)
let compile_sys_call (fname: string) (args: expr list) (env: env)
  (compilexpr: expr -> env -> instruction list) : instruction list = 
  let params, type_return = sys_lookup fname env in
  let arity = List.length params in
  let argc = List.length args in
  if (argc != arity) 
    then Fmt.failwith 
    "Arity mismatch: system function %s expects %d arguments but got %d"
    fname arity argc
  else 
    (* type check *)
    let compiled_args = List.map (fun e -> compilexpr e env) args in 
    let pushed_regs   = List.rev (push_regs arity arg_regs) in
    let prepare_call  = List.concat 
    (List.rev (prepare_call compiled_args arg_regs true ~types:params)) in 
    let restore_rsp   = if arity > 6 
      then [IAdd (Reg RSP, Const (Int64.of_int (8 * (arity - 6))))]
      else [] in
    let popped_regs   = pop_regs arity arg_regs in
    pushed_regs @ prepare_call @ [ICall fname] @ restore_rsp @ popped_regs 
    @ type_checking ret_reg type_return


(* -----------------------------------
  |       FIRST ORDER FUNCTIONS      |                
  ------------------------------------*)
let compile_fof_call  (fname: string) (args: expr list) (env: env)
  (compilexpr: expr -> env -> instruction list) : instruction list =  
  let arity = fun_lookup fname env in
  let argc = List.length args in
  if (argc != arity)
    then Fmt.failwith 
      "Arity mismatch: function %s expects %d arguments but got %d" fname arity argc
    else
      let compiled_args = List.map (fun expr -> compilexpr expr env) args in
      let pushed_regs   = List.rev (push_regs arity arg_regs) in
      let prepare_call  = List.concat (List.rev (prepare_call compiled_args arg_regs false)) in 
      let restore_rsp   = if arity > 6 
        then [IAdd (Reg RSP, Const (Int64.of_int (8 * (arity - 6))))]
        else [] in
      let popped_regs   = pop_regs arity arg_regs in
      pushed_regs @ prepare_call @ [ICall fname] @ restore_rsp @ popped_regs    

(* -----------------------------------
  |              TUPLES              |                
  ------------------------------------*)
let comp_tuple_elem (index: int) (exp: expr) (env: env) 
  (compilexpr: expr -> env -> instruction list) : instruction list =
  let compiled_element = compilexpr exp env in
  compiled_element @ [IMov (RegOffset (heap_reg, index), ret_reg)]

let rec comp_tuple_elems (elems: expr list) (env: env) (index: int)
  (compilexpr: expr -> env -> instruction list) : instruction list =
  match elems with
  | [] -> []
  | el::tail -> comp_tuple_elem index el env compilexpr @ comp_tuple_elems tail env (index + 1) compilexpr

let compile_tuple (elems: expr list) (env: env) 
  (compilexpr: expr -> env -> instruction list) : instruction list = 
  let assign_elements = comp_tuple_elems elems env 1 compilexpr in 
  let size = List.length elems in
  [IMov (RegOffset (heap_reg, 0), Const (Int64.of_int size))] @ assign_elements @
  [
    IMov (ret_reg, Reg heap_reg);     (* Start creating the tuple value itself *)
    IAdd (ret_reg, Const 1L);         (* Tag the tuple *)
    IAdd (Reg heap_reg, Const (Int64.of_int (size*8 + 8 ))) (* Bump the heap pointer *)
  ]
  @ if (size+1) mod 2 = 1 then
    [IAdd (Reg heap_reg, Const 8L)]
  else []
  @ type_checking ret_reg TupleT


(* -----------------------------------
  |                MAIN              |                
  ------------------------------------*)

(* THE MAIN compiler function *)
let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n   ->  [ IMov (ret_reg, Const (encode_int n)) ]
  | Bool p  ->  [ IMov (ret_reg, Const (encode_bool p)) ]
  | UnOp (op, e)  -> compile_unop (compile_expr e env) op
  | Id x    ->  [ IMov (ret_reg, let_lookup x env) ]
  | Let (id,v,b) -> 
      let (new_env, loc)  = extend_env id env in
      let compiled_val    = compile_expr v env in
      let save_val  = [ IMov(loc, ret_reg) ] in
      compiled_val @ save_val @ (compile_expr b new_env)
  | BinOp (op, l, r) -> compile_binop op l r env compile_expr
  | If (c, t, f) -> (compile_expr c env) 
      @ type_checking ret_reg BoolT 
      @ compile_if compile_expr t f env
  | Sys (fname, args) -> compile_sys_call fname args env compile_expr
  | App (fname, args) -> compile_fof_call fname args env compile_expr
  | Tuple exprs -> compile_tuple exprs env compile_expr
  (* | Get (tup, index) -> []  *)
  | Void -> []


(* The maximum integer in a int list *)
let rec max_list (l: int list): int =
  match l with
  | [] -> 0
  | h::tail -> max h (max_list tail)


(* Counts the max number of vars defined at the same time *)
let rec varcount (e:expr): int =
  match e with
  | Num _ -> 0
  | Bool _ -> 0
  | Id _ -> 0
  | UnOp (_, sub_ex) -> varcount sub_ex
  | BinOp (_, l_ex, r_ex) -> 1 + max (varcount l_ex) (varcount r_ex)
  | Let (_, val_ex, body_ex) -> 1 + max (varcount val_ex) (varcount body_ex)
  | If (_, t_ex, f_ex) -> max (varcount t_ex) (varcount f_ex)
  | App (_, args_ex) -> max_list (List.map varcount args_ex)
  | Sys (_, args_ex)-> max_list (List.map varcount args_ex)
  | Tuple exprs -> max_list (List.map varcount exprs)
  (* | Get (tup, index) -> 0  *)
  | Void -> 0

(* 
  Gets the necesary space to save local vars 
  Uses a sound overaproximate.
*)
let stack_offset_for_local (exp: expr): int =
  let vars = varcount exp in
  if vars mod 2 = 0 then vars * 8 else (vars * 8 + 8)

(* Label for handling errors *)
let error_handler =
 [ILabel "error_handler";
  IMov (Reg RSI, err_reg);
  IMov (Reg RDI, err_cod_reg);
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

(* Checks if an element exists in a list *)
let rec exist elem lst =
  match lst with
  | [] -> false
  | hd::tl -> elem = hd || exist elem tl

(* Finds duplicated in a string list *)
let rec dupExist lst: 'a =
  match lst with
  | [] -> ""
  | hd::tl -> if (exist hd tl) then hd else dupExist tl

(* Compiles definitions for first order functions *)
let compile_def (fname: string) (params: string list) (body: expr) (env: env): 
  instruction list =
  let _, fenv, senv = env in
  let dup_arg = dupExist params in
  if dup_arg != "" 
    then Fmt.failwith "Duplicate parameter name in function %s: %s"
      fname dup_arg
  else
    let lenv = let_env_from_params params empty_env in
    let stack_offset = Int64.of_int (stack_offset_for_local body) in
    [IEmpty ; ILabel fname] @ callee_prologue 
    @ [ISub (Reg RSP, Const stack_offset)]
    @ compile_expr body (lenv, fenv, senv) @ callee_epilogue @ [IRet]

(* Compiles a declaration for a function *)
let compile_declaration (dec: decl) (env: env) 
(funs: instruction list) (exts: instruction list): instruction list * instruction list =
  match dec with 
  | FunDef (fname, params, body) -> 
    compile_def fname params body env @ funs , exts
  | SysFunDef (fname, _, _) -> funs, [IExtern fname] @ exts

(* Compiles declarations for functions *)  
let rec compile_declarations (decls: decl list) (env: env) :
instruction list * instruction list =
  match decls with
  | [] -> [], []
  | dec::tail ->
    let funs, exts = compile_declarations tail env in
    compile_declaration dec env funs exts

(* Generates the compiled program *)
let compile_prog : prog Fmt.t =
  fun fmt p ->
    match p with Program (decs, exp) ->
      let fenv = fun_env_from_decls decs empty_fun_env in
      let senv = sys_env_from_decls decs empty_sys_env in
      let env  = empty_env, fenv, senv in
      let functions, externs = compile_declarations decs env in
      let instrs = compile_expr exp env in
      let stack_offset = Int64.of_int (stack_offset_for_local exp) in
      Fmt.pf fmt 
"section .text
extern error
%a
global our_code_starts_here
our_code_starts_here:
%a
  mov  R15, RSI                 ;; load R15 with the passed HEAP
  add  R15, 7                   ;; Add 7 to the next multiple of 7
  mov  R11, 0xfffffffffffffff8  ;; R11 is now 11111...1000
  and  R15, R11                 ;; Round back down
%a" 
      pp_instrs externs pp_instrs callee_prologue pp_instrs (
      [ISub (Reg RSP, Const stack_offset)] @ instrs @ callee_epilogue @ [IRet]
      @ error_handler @ functions)

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse_prog (sexp_list_from_string src)) compile_prog
