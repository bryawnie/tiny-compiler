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
let ret_reg = RAX
let aux_reg = R11
let err_code_reg = RBX
let type_code_reg = RCX
let heap_reg = R15
let arg_regs = [
  Reg RDI; Reg RSI; Reg RDX; Reg RCX; Reg R8; Reg R9
]

(* INTEGER ERROR CODES *)
let not_a_number = Const 1L
let not_a_boolean = Const 2L 
let not_a_tuple = Const 3L
let neg_index = Const 4L
let index_overflow = Const 5L
let not_a_record = Const 6L
let record_type_error = Const 7L

(* TYPE TAGS *)
let int_tag = Const 0L (* All values ending in 0 are integers *)
let bool_tag = Const 1L (* 001 - boolean value *)
let tuple_tag = Const 3L (* 011 - tuple pointer *)
let record_tag = Const 5L (* 101 - record pointer *) 
let tag_bitmask = Const 7L (*111*)

(* other constants *)
let rax_error_handler = "error_type_handler"
let rcx_error_handler = "error_index_handler"

let str_type (t: dtype): string =
  match t with
  | IntT    -> "Integer"
  | BoolT   -> "Boolean"
  | TupleT  -> "Tuple"
  | RecordT -> "Record"
  | AnyT    -> "Any"

(* This function handles the request for a type checking *)
let type_checking (register: arg) (expected_type: dtype): instruction list =
  let tag_check error_code type_tag =
    [IMov ((Reg err_code_reg), error_code) ; IMov ((Reg type_code_reg), register) ;
    IAnd ((Reg type_code_reg), tag_bitmask) ; ICmp ((Reg type_code_reg), type_tag) ;
    IJnz "error_type_handler"]
  in
  let beg_comm = [IComment (Fmt.str "Begin Type Checking %s" @@ str_type expected_type)] in
  let end_comm = [IComment (Fmt.str "End Type Checking %s" @@ str_type expected_type) ] in
  beg_comm @
  begin match expected_type with
  | IntT -> [
    ITest (register, Const 1L) ; 
    IMov ((Reg err_code_reg), not_a_number) ;
    IJnz "error_type_handler";
  ]
  | BoolT -> tag_check not_a_boolean bool_tag
  | TupleT -> tag_check not_a_tuple tuple_tag
  | RecordT -> tag_check not_a_record record_tag
  | AnyT -> [] (* AnyT == no typecheck *)
  end
  @ end_comm
  
let rec take (k:int) (l: 'a list) : 'a list =
  if k > 0 then
  begin match l with
  | [] -> []
  | x :: xs -> x::take (k-1) xs
  end else []

(* Returns the instruction list to backup the necesary registers before a function call *)
let push_regs (n: int) (regs: arg list): instruction list =
  List.map (fun r -> IPush r) (take n regs)


(* Returns the instruction list to restore the necesary registers after a function call *)
let pop_regs (n: int) (regs: arg list): instruction list =
  List.map (fun r -> IPop r) (take n regs)


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
  | Add1 -> expr @ type_checking (Reg ret_reg) IntT @ [IAdd ((Reg ret_reg), Const 2L)]
  | Sub1 -> expr @ type_checking (Reg ret_reg) IntT @ [ISub ((Reg ret_reg), Const 2L)]
  | Not ->  expr @ type_checking (Reg ret_reg) BoolT @ [IMov ((Reg aux_reg), Const bool_bit) ; IXor ((Reg ret_reg), (Reg aux_reg))]


(* -----------------------------------
  |                BINOPS             |                
  ------------------------------------*)

(* Compiles instructions common to binary operators. Moves left argument
into RAX ((Reg ret_reg)), and the right one into R11 ((Reg arg_reg)).*)
let compile_binop_preamble (l: expr) (typeL: dtype) (r: expr) (typeR: dtype) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compiled_left = compiler l env in
  let new_env, loc = extend_env (tmp_gensym ()) env in
  let compiled_right = compiler r new_env in
  compiled_left  @ type_checking (Reg ret_reg) typeL @ [ IMov (loc, (Reg ret_reg)) ] @
  compiled_right @ type_checking (Reg ret_reg) typeR @ [ IMov ((Reg aux_reg), (Reg ret_reg)) ] @ [ IMov ((Reg ret_reg), loc) ]

(* Compiles and/or operators *)
let compile_shortcut_binop (l: expr) (r: expr) (env: env) (lbl: string) (skip_on: bool)
  (compiler: expr -> env -> instruction list) : instruction list =
  let compare = [
    IMov ((Reg aux_reg), Const (encode_bool skip_on)) ;
    ICmp ((Reg ret_reg), (Reg aux_reg)) ; 
    IJe lbl
  ] in
  compiler l env @ type_checking (Reg ret_reg) BoolT @ compare @ 
  compiler r env @ type_checking (Reg ret_reg) BoolT @ compare
  @ [IMov ((Reg ret_reg), Const (encode_bool (not skip_on))) ; ILabel lbl]

(* Compiles < and = comparators *)
let compile_binop_comparator (cmp_label: string) (inst: instruction) : instruction list =
  let preamble  =  [
    ICmp ((Reg ret_reg), (Reg aux_reg)) ; 
    IMov ((Reg ret_reg), Const true_encoding) 
  ] in 
  let ending    =  [
    IMov ((Reg ret_reg), Const false_encoding) ; 
    ILabel cmp_label 
  ] in 
  preamble @ [inst] @ ending

(* 
  -- Compiles binop expressions --
  They can be + - * / < = and or
  Arithmetical binops (such as + - * /) are derivated to comp_and_check_arithmetic_binop
*)
let compile_binop (op: binOp) (l: expr) (r: expr) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list = 
  match op with
    | Add -> compile_binop_preamble l IntT r IntT env compiler
      @ [IAdd ((Reg ret_reg), (Reg aux_reg))]
    | Sub -> compile_binop_preamble l IntT r IntT env compiler
      @ [ISub ((Reg ret_reg), (Reg aux_reg))]
    | Mul -> compile_binop_preamble l IntT r IntT env compiler
      @ [IMul ((Reg ret_reg), (Reg aux_reg)) ; ISar ((Reg ret_reg), Const 1L)]
    | Div -> compile_binop_preamble l IntT r IntT env compiler
      @ [ICqo ; IDiv ((Reg aux_reg)) ; ISal ((Reg ret_reg), Const 1L)]
    | Less -> 
      let less_lbl = less_gensym () in
      compile_binop_preamble l IntT r IntT env compiler 
      @ compile_binop_comparator less_lbl (IJl less_lbl)
    | Eq -> (* Doesn't need a type checking *)
      let eq_lbl =  equal_gensym () in
      compile_binop_preamble l AnyT r AnyT env compiler
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
    ICmp ((Reg ret_reg), Const false_encoding);
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
let decode (t: dtype): instruction list =
  match t with
  | IntT    -> [ISar (Reg ret_reg, Const 1L)]
  | BoolT   -> [IShr (Reg ret_reg, Const 63L)]
  | TupleT  -> [ISub (Reg ret_reg, tuple_tag)]
  | RecordT -> [ISub (Reg ret_reg, record_tag)]
  | AnyT    -> []

let encode (t: dtype): instruction list =
  match t with
  | IntT    -> [ISal (Reg ret_reg, Const 1L)]
  | BoolT   -> [IShl (Reg ret_reg, Const 63L); IAdd (Reg ret_reg, Const 1L)]
  | TupleT  -> [IAdd (Reg ret_reg, tuple_tag)]
  | RecordT -> [IAdd (Reg ret_reg, record_tag)]
  | AnyT    -> []

let rec save_args ?(types= []) (comp_exprs: instruction list list) (env: env): instruction list =
  let new_env, loc_arg = extend_env (tmp_gensym ()) env in
  match comp_exprs, types with
  | [], _ -> []
  | compiled::tail, [] -> compiled @ [IMov (loc_arg, Reg ret_reg)] @ save_args tail new_env
  | compiled::tail, t::ts -> compiled @ type_checking (Reg ret_reg) t @ decode t @
    [IMov (loc_arg, Reg ret_reg)] @ save_args tail new_env ~types:ts

let rec pass_args (num_args: int) (regs: arg list) (env: env): instruction list =
  let new_env, loc_arg = extend_env (tmp_gensym ()) env in
  match regs with
    | [] -> 
      if num_args > 0 then 
        pass_args (num_args - 1) [] new_env @ [IMov (Reg aux_reg, loc_arg); IPush (Reg aux_reg)] 
      else []
    | x::xs -> 
      if num_args > 0 then 
        pass_args (num_args - 1) xs new_env @ [IMov (Reg aux_reg, loc_arg); IMov (x, Reg aux_reg)] 
      else []


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
    let saved_args    = save_args compiled_args env ~types:params in
    let pushed_regs   = List.rev (push_regs arity arg_regs) in
    let passed_args   = pass_args arity arg_regs env in
    let restore_rsp   = if arity > 6 
      then [IAdd (Reg RSP, Const (Int64.of_int (8 * (arity - 6))))]
      else [] in
    let popped_regs   = pop_regs arity arg_regs in
    saved_args @ pushed_regs @ passed_args @ [ICall fname] @ restore_rsp @ popped_regs @ encode type_return
    (* @ type_checking (Reg ret_reg) type_return *)


(* -----------------------------------
  |       FIRST ORDER FUNCTIONS      |                
  ------------------------------------*)
let compile_fof_call  (fname: string) (args: expr list) (env: env)
  (compilexpr: expr -> env -> instruction list) : instruction list =  
  let flbl, arity = fun_lookup fname env in
  let argc = List.length args in
  if (argc != arity)
    then Fmt.failwith 
      "Arity mismatch: function %s expects %d arguments but got %d" fname arity argc
    else
      let compiled_args = List.map (fun expr -> compilexpr expr env) args in
      let saved_args    = save_args compiled_args env in
      let pushed_regs   = List.rev (push_regs arity arg_regs) in
      let passed_args   = pass_args arity arg_regs env in
      let restore_rsp   = if arity > 6 
        then [IAdd (Reg RSP, Const (Int64.of_int (8 * (arity - 6))))]
        else [] in
      let popped_regs   = pop_regs arity arg_regs in
      saved_args @ pushed_regs @ passed_args @ [ICall flbl] @ restore_rsp @ popped_regs    

(* -----------------------------------
  |              TUPLES              |                
  ------------------------------------*)
(* Compiles an element that will be assigned to a Tuple
  saving it in stack *)
let comp_tuple_elem (exp: expr) (env: env) 
  (compilexpr: expr -> env -> instruction list) : instruction list  * env =
  let new_env, loc_arg = extend_env (tmp_gensym ()) env in
  let compiled_element = compilexpr exp new_env in
  (compiled_element @ [IMov (loc_arg, Reg ret_reg)], new_env)

(* Compiles every value in tuple *)
let rec comp_tuple_elems (elems: expr list) (env: env)
  (compilexpr: expr -> env -> instruction list) : instruction list =
  match elems with
  | [] -> []
  | el::tail -> 
    let compiled_elem, new_env = comp_tuple_elem el env compilexpr in 
    let compiled_src = comp_tuple_elems tail new_env compilexpr in
    compiled_elem @ compiled_src

(* Assigns the tuple values *)
let rec assign_tuple_values (index: int) (size: int) (env: env) : instruction list =
  if index > size then [] else
  let new_env, loc_arg = extend_env (tmp_gensym ()) env in
  [IMov (Reg aux_reg, loc_arg)  ;
   IMov (RegOffset (heap_reg, index), Reg aux_reg) ]
  @ assign_tuple_values (index+1) size new_env

(* Compiles a tuple *)
let compile_tuple (elems: expr list) (env: env) 
  (compilexpr: expr -> env -> instruction list) : instruction list = 
  let compile_elems = comp_tuple_elems elems env compilexpr in 
  let size = List.length elems in
  let assign_tuple = assign_tuple_values 1 size env in
  compile_elems @ 
  [IMov (RegOffset (heap_reg, 0), Const (encode_int (Int64.of_int size)))] @ 
  assign_tuple @
  [
    IMov ((Reg ret_reg), Reg heap_reg);     (* Start creating the tuple value itself *)
    IAdd ((Reg ret_reg), tuple_tag);         (* Tag the tuple *)
    IAdd (Reg heap_reg, Const (Int64.of_int (size*8 + 8 ))) (* Bump the heap pointer *)
  ]
  @ if (size+1) mod 2 = 1 then
    [IAdd (Reg heap_reg, Const 8L)]
  else []
  @ type_checking (Reg ret_reg) TupleT

(* -----------------------------------
  |               GET                |
  ------------------------------------*)
(* Checks positive index and avoid overflows *)
let check_index (index: arg) : instruction list =
  [ (* Negative Index Error *)
    IComment "Begin Check: Positive Index for Tuples";
    IMov ((Reg err_code_reg), neg_index) ; 
    IMov ((Reg type_code_reg), index) ;
    ICmp (index, Const 0L);     
    IJl  "error_index_handler";
    IComment "End Check: Positive Index for Tuples";
  ] @ [ (* Index out of bounds *)
    IComment "Begin Check: Index out of Bounds";
    IMov ((Reg err_code_reg), index_overflow) ; 
    ICmp (index, RegOffset (RAX, 0)); 
    IJge "error_index_handler";
    IComment "End Check: Index out of Bounds";
  ]

(* Compiles get expressions *)
let compile_get (tup: expr) (index: expr) (env: env) 
  (compilexpr: expr -> env -> instruction list) : instruction list = 
  let comp_ind = compilexpr index env in
  let env1, loc_idx = extend_env (tmp_gensym ()) env in
  let comp_tup = compilexpr tup env1 in
  let check_index_t = type_checking (Reg ret_reg) IntT in
  let check_tuple_t = type_checking (Reg ret_reg) TupleT in 
  comp_ind @ check_index_t @ [IMov (loc_idx, (Reg ret_reg))] @ 
  comp_tup @ check_tuple_t @
  [ 
    ISub ((Reg ret_reg), tuple_tag) ; 
    IMov ((Reg aux_reg), loc_idx) 
  ] @ check_index (Reg aux_reg) @
  [ 
    IAdd ((Reg aux_reg), Const 2L);
    ISar ((Reg aux_reg), Const 1L);
    IMul ((Reg aux_reg), Const 8L);     (* Index * 8 *)
    IAdd ((Reg ret_reg), (Reg aux_reg));      (* RAX + 8*(i+1) *)
    IMov ((Reg ret_reg), RegOffset (RAX, 0))
  ]

(* -----------------------------------
  |               SET                |
  ------------------------------------*)

(* Compiles set expressions for tuples *)
let compile_set (tup: expr) (index: expr) (value: expr) (env: env) 
  (compilexpr: expr -> env -> instruction list) : instruction list = 
  let comp_val = compilexpr value env in
  let env1, loc_val = extend_env (tmp_gensym ()) env in
  let comp_ind = compilexpr index env1 in
  let env2, loc_idx = extend_env (tmp_gensym ()) env1 in
  let comp_tup = compilexpr tup env2 in
  let check_index_t = type_checking (Reg ret_reg) IntT in
  let check_tuple_t = type_checking (Reg ret_reg) TupleT in 
  comp_val @ [IMov (loc_val, (Reg ret_reg))] @
  comp_ind @ check_index_t @ [ IMov (loc_idx, (Reg ret_reg))] @ 
  comp_tup @ check_tuple_t @
  [
    ISub ((Reg ret_reg), tuple_tag);
    IMov ((Reg aux_reg), loc_idx)
  ]
  @ check_index (Reg aux_reg) @
  [
    IAdd ((Reg aux_reg), Const 2L);
    ISar ((Reg aux_reg), Const 1L) ; 
    IMul ((Reg aux_reg), Const 8L);     (* Index * 8 *)
    IAdd ((Reg ret_reg), (Reg aux_reg));      (* RAX + 8*(i+1) *)
    IMov (Reg RDX, loc_val);
    IMov (RegOffset (RAX, 0), Reg RDX);
    ISub ((Reg ret_reg), (Reg aux_reg));      (* RAX - 8*(i+1) *)
    IAdd ((Reg ret_reg), tuple_tag) ;
  ]

(* -----------------------------------
  |               LET                |
  ------------------------------------*)
(* Introduces a new id in scope *)
let compile_let_expr (compilexpr: expr -> env -> instruction list)
  (env: env) (id: string) (value: expr): instruction list * env =
  let (new_env, loc)  = extend_env id env in
  let compiled_val    = compilexpr value env in
  compiled_val @ [ IMov(loc, Reg ret_reg) ], new_env

(* Introduces muliple new ids in scope *)
let rec compile_let_exprs (compilexpr: expr -> env -> instruction list) 
  (env: env) (pairs: (string*expr) list): instruction list * env =
  match pairs with
  | [] -> ([], env)
  | (id, v)::tail ->
    let compiled_expr, new_env = compile_let_expr compilexpr env id v in
    let compiled_tail, final_env = compile_let_exprs compilexpr new_env tail in
    (compiled_expr @ compiled_tail, final_env)
  

(* -----------------------------------
  |                MAIN              |                
  ------------------------------------*)

(* THE MAIN compiler function *)
let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n   ->  [ IMov ((Reg ret_reg), Const (encode_int n)) ]
  | Bool p  ->  [ IMov ((Reg ret_reg), Const (encode_bool p)) ]
  | UnOp (op, e)  -> compile_unop (compile_expr e env) op
  | Id x    ->  [ IMov (Reg ret_reg, let_lookup x env) ]
  | Let (vals,b) -> 
      let compiled_vals, new_env    = compile_let_exprs compile_expr env vals in
      compiled_vals @ (compile_expr b new_env)
  | BinOp (op, l, r) -> compile_binop op l r env compile_expr
  | If (c, t, f) -> (compile_expr c env) 
      @ type_checking (Reg ret_reg) BoolT 
      @ compile_if compile_expr t f env
  | Sys (fname, args) -> compile_sys_call fname args env compile_expr
  | App (fname, args) -> compile_fof_call fname args env compile_expr
  | Tuple exprs -> compile_tuple exprs env compile_expr
  | Get (tup, index) -> compile_get tup index env compile_expr
  | Set (tup, index, value) -> compile_set tup index value env compile_expr
  | Length tup -> compile_expr tup env @ type_checking (Reg ret_reg) TupleT @
    [ ISub ((Reg ret_reg), tuple_tag); IMov ((Reg ret_reg), RegOffset (RAX, 0))]
  | Void -> [INop]


(* Counts the max number of expressions mantained in stack at the same time *)
let rec varcount (e:expr): int =
  match e with
  | Num _ -> 0
  | Bool _ -> 0
  | Id _ -> 0
  | UnOp (_, sub_ex) -> varcount sub_ex
  | BinOp (_, l_ex, r_ex) -> 1 + max (varcount l_ex) (varcount r_ex)
  | Let (defs, body_ex) -> 1 + max (List.fold_left max 0 (List.map (fun (_,y)-> varcount y) defs)) (varcount body_ex)
  | If (_, t_ex, f_ex) -> max (varcount t_ex) (varcount f_ex)
  | App (_, args_ex) -> List.fold_left max 0 (List.map varcount args_ex) + List.length args_ex
  | Sys (_, args_ex)-> List.fold_left max 0 (List.map varcount args_ex) + List.length args_ex
  | Tuple exprs -> List.fold_left max 0 (List.map varcount exprs) + List.length exprs
  | Get (tuple, index) -> 1 + max (varcount tuple) (varcount index)
  | Set (tuple, index, value) -> 2 + max (max (varcount tuple) (varcount index)) (varcount value)
  | Length tuple -> varcount tuple
  | Void -> 0

(* 
  Gets the necesary space to save local vars 
  Uses a sound overaproximate.
*)
let stack_offset_for_local (exp: expr): int =
  let vars = varcount exp in
  if vars mod 2 = 0 then vars * 8 else (vars * 8 + 8)

(* Label for handling errors *)
let error_type_handler =
 [ILabel "error_type_handler";
  IMov (Reg RSI, (Reg ret_reg));
  IMov (Reg RDI, (Reg err_code_reg));
  ICall "error"]

let error_index_handler =
  [ILabel "error_index_handler";
    IMov (Reg RSI, (Reg type_code_reg));
    IMov (Reg RDI, (Reg err_code_reg));
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

(* Compiles definitions for first order functions *)
let compile_fundef (fname: string) (params: string list) (body: expr) (env: env): 
  instruction list =
  let _, fenv, senv = env in
  let flbl, _ = fun_lookup fname env in
  let lenv = let_env_from_params params empty_env in
  let stack_offset = Int64.of_int (stack_offset_for_local body) in
  [IEmpty ; ILabel flbl] @ callee_prologue 
  @ [ISub (Reg RSP, Const stack_offset)]
  @ compile_expr body (lenv, fenv, senv) @ callee_epilogue @ [IRet]

(* compiles the constructor function for a record *)
let compile_rec_constructor (id: string) (id_code: int64) (arity: int) 
  (env: env) : instruction list =
  let flbl, _ = fun_lookup id env in
  let heap_size = Const (Int64.of_int (8 + arity * 8)) in
  let rec move_args n =
    if n <= 0 then []
    else if n > 6 then 
      IMov(RegOffset(heap_reg, n), RegOffset(RBP, -2 -(n-6)))::(move_args @@ n-1)
    else (IMov(RegOffset(heap_reg, n), List.nth arg_regs (n-1)))
      ::(move_args @@ n-1)
  in
  [IEmpty; ILabel flbl ;(* constructor name *)
  (* rsp does not move because this function does not use the stack.
  rbp is not backed up because it is not modified. *)
  IMov ((Reg ret_reg), Const id_code); (* rax <- id_code *)
  IMov (RegOffset(heap_reg, 0), (Reg ret_reg))] (* write id_code to heap *) 
  @ move_args arity               (* write values to heap *)
  @ [IMov ((Reg ret_reg), Reg heap_reg) ; (* get pointer value *)
  IAdd ((Reg ret_reg), record_tag) ;    (* tag value *)
  IAdd (Reg heap_reg, heap_size) ;(* bump heap *)
  IRet]                           (* return *)

(* compiles the get function for a record field *)
let compile_rec_get (rec_id: string) (id_code: int64) 
  (field_id: string) (field_pos: int) (env:env) =
  let flbl, _ =
    let fun_name = String.concat "-" [rec_id ; field_id] in
    fun_lookup fun_name env
  in
  [ILabel flbl ;                      (* function label *)
  IMov ((Reg ret_reg), Reg RDI)]            (* rax <- val *)
  @ type_checking (Reg ret_reg) RecordT @   (* type check rax *)
  [ISub (Reg RDI, record_tag) ;             (* untag value to obtain pointer *)
  IMov ((Reg aux_reg), Const id_code) ;     (* r11 <- id_code *)
  IMov (Reg err_code_reg, record_type_error) ; (* r <- error_code *)
  ICmp (RegOffset(RDI, 0), (Reg aux_reg)) ; (* ID CHECK! *)
  IJne rax_error_handler;             (* if != then error_handler *)
  IMov (Reg RAX, RegOffset(RDI, field_pos)) ; (* rax <- [rax + 8 * i]*)
  IRet] (* return *)

let rec compile_getters (rec_id: string) (rec_code: int64) 
  (fields: string list) (pos: int) (env: env) =
  match fields with
  | [] -> []
  | field_id::tail ->
    (compile_rec_get rec_id rec_code field_id pos env)
    @ (compile_getters rec_id rec_code tail (pos+1) env)

(* Compiles a record type checker. Type checkers have the name of the form
"[record_id]?" and return true if the passed argument is of type record_id *)
let compile_rec_typechecker (rec_id: string) (id_code: int64) (env: env)
  : instruction list =
  let label, _ = 
    let fname = String.concat "" [rec_id ; "?"] in
    fun_lookup fname env
  in
  let return_label = String.concat "_" [label ; "return"] in
  [ILabel label ;
  IMov ((Reg ret_reg), Const false_encoding) ;  (* rax <- false *)
  IMov ((Reg aux_reg), Reg RDI) ;               (* r11 <- val *)
  IAnd ((Reg aux_reg), tag_bitmask) ;           (* get type tag *)
  ICmp ((Reg aux_reg), record_tag) ;            (* compare to record type tag *)
  IJne return_label ;                     (* if != return false *)
  IMov ((Reg aux_reg), Const id_code) ;         (* r11 <- id_code *)
  ISub (Reg RDI, record_tag) ;            (* untag to obtain pointer *)
  ICmp (RegOffset(RDI, 0), (Reg aux_reg)) ;     (* check record type *)
  IJne return_label ;                     (* if != return false *)
  IMov ((Reg ret_reg), Const true_encoding) ;   (* rax <- false*)
  ILabel return_label ;
  IRet]

(* Compiles constructors and getters for a record type *)
let compile_recdef (id: string) (fields: string list) (env: env) : instruction list =
  (* This tag allows for record type checking at runtime.
    tag = (record_counter() << 32) | length(fields) *)
  let rec_id_code = Int64.add
    (Int64.shift_left (Int64.of_int @@ record_counter ()) 32)
    (Int64.of_int @@ List.length fields)
  in 
  let constructor = 
    compile_rec_constructor id rec_id_code (List.length fields) env
  in
  let getters = compile_getters id rec_id_code fields 1 env in
  let type_checker = compile_rec_typechecker id rec_id_code env in
  constructor @ getters @ type_checker

(* Compiles a declaration for a function. Returns a instr list for compiled
functions and another for foreign function "extern"s *)
let compile_declaration (dec: decl) (env: env) 
(funs: instruction list) (exts: instruction list): instruction list * instruction list =
  match dec with 
  | FunDef (fname, params, body) -> 
    compile_fundef fname params body env @ funs , exts
  | SysFunDef (fname, _, _) -> funs, [IExtern fname] @ exts
  | RecDef (id, fields) -> compile_recdef id fields env @ funs, exts

(* Compiles declarations for functions *)  
let rec compile_declarations (decls: decl list) (env: env) :
instruction list * instruction list =
  match decls with
  | [] -> [], []
  | dec::tail ->
    let funs, exts = compile_declarations tail env in
    compile_declaration dec env funs exts

(* default system functions *)
let default_sys_env : sys_env =
  ["print", ([AnyT], AnyT) ;
  "raw_print", ([AnyT], AnyT)]

(* Generates the compiled program *)
let compile_prog : prog Fmt.t =
  fun fmt p ->
    match p with Program (decs, exp) ->
      let fenv = fun_env_from_decls decs empty_fun_env in
      let senv = sys_env_from_decls decs default_sys_env in
      let env  = empty_env, fenv, senv in
      let functions, externs = compile_declarations decs env in
      let instrs = compile_expr exp env in
      let stack_offset = Int64.of_int (stack_offset_for_local exp) in
      Fmt.pf fmt 
"section .text
extern error
extern print
extern raw_print
%a
global our_code_starts_here
our_code_starts_here:
%a
  mov  R15, RDI                 ;; load R15 with the passed HEAP
  add  R15, 7                   ;; Add 7 to the next multiple of 7
  mov  R11, 0xfffffffffffffff8  ;; R11 is now 11111...1000
  and  R15, R11                 ;; Round back down
%a" 
      pp_instrs externs pp_instrs callee_prologue pp_instrs (
      [ISub (Reg RSP, Const stack_offset)] @ instrs @ callee_epilogue @ [IRet]
      @ error_type_handler @ error_index_handler @ functions)

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse_prog (sexp_list_from_string src)) compile_prog
