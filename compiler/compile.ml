open Ast
open Asm
open Env
open Encode
open Gensym
open Utils
open Errors


(* Returns the instruction list to backup the necesary registers before a function call *)
let push_regs (n: int) (regs: arg list): instruction list =
  List.map (fun r -> IPush r) (take n regs)

(* Returns the instruction list to restore the necesary registers after a function call *)
let pop_regs (n: int) (regs: arg list): instruction list =
  List.map (fun r -> IPop r) (take n regs)

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
  | Fun (_, _) -> 2
  | App (_, args_ex) -> List.fold_left max 0 (List.map varcount args_ex) + List.length args_ex + 2
  | Sys (_, args_ex)-> List.fold_left max 0 (List.map varcount args_ex) + List.length args_ex
  | Tuple exprs -> List.fold_left max 0 (List.map varcount exprs) + List.length exprs
  | Get (tuple, index) -> 1 + max (varcount tuple) (varcount index)
  | Set (tuple, index, value) -> 2 + max (max (varcount tuple) (varcount index)) (varcount value)
  | Length tuple -> varcount tuple
  | Void -> 0

let rec varcount_funs (ds: decl list): int =
  match ds with 
  | [] -> 0
  | d::ds ->
    match d with
    | FunDef (_, _, _) -> 4 + varcount_funs ds
    | _ -> varcount_funs ds

let varcount_prog (p: prog): int =
  let Program (decs, exp) = p in
  varcount exp + varcount_funs decs

(* 
  Gets the necesary space to save local vars 
  Uses a sound overaproximate.
*)

(* This function is applied to the main program *)
let stack_offset_for_prog (p: prog): int =
  let vars = varcount_prog p in
  if vars mod 2 = 0 then vars * 8 else (vars * 8 + 8)

(* This function is applied for function calling *)
let stack_offset_for_local (exp: expr): int =
  let vars = varcount exp in
  if vars mod 2 = 0 then vars * 8 else (vars * 8 + 8)

(*
  FREE VARS - FUNCTION CALLS
  Counts the number of free vars in the body of a function
  Used to create closures.
*)
let rec free_vars (exp: expr) (saved: string list): string list =
  match exp with
  | Num _   ->  []
  | Bool _  ->  []
  | UnOp (_, e)  -> free_vars e saved
  | Id x    ->  if List.mem x saved then [] else [x]
  | Let (vals,b) -> free_vars b (saved @ (List.map (fun (x,_) -> x) vals))
  | BinOp (_, l, r) -> free_vars l saved @ free_vars r saved
  | If (c, t, f) -> free_vars c saved @ free_vars t saved @ free_vars f saved
  | Fun (ids, body) -> free_vars body (saved @ ids)
  | Sys (_, args) -> List.concat_map (fun x -> free_vars x saved) args
  | App (fexp, args) -> free_vars fexp saved @ List.concat_map (fun x -> free_vars x saved) args
  | Tuple exprs -> List.concat_map (fun x -> free_vars x saved) exprs
  | Get (tup, index) -> free_vars tup saved @ free_vars index saved
  | Set (tup, index, value) -> free_vars tup saved @ free_vars index saved @ free_vars value saved
  | Length tup -> free_vars tup saved
  | Void -> []


(* Compiles a function closure *)
let compile_closure (label: string) (arity: int64) (free_vars: string list) (env: let_env) =
  let size = 3 + List.length free_vars in
  let num_fv = Int64.of_int (List.length free_vars) in 
  let rec store_free_vars (vars: string list) =
    match vars with 
    | [] -> [] 
    | hd::tl ->
      [ IMov (Reg aux_reg, let_lookup hd (env,[]));
        IMov (RegOffset (heap_reg, 3 + (List.length free_vars - List.length vars)), Reg aux_reg)] @
      store_free_vars tl
  in
  [ IMov (RegOffset (heap_reg, 0), Const (encode_int arity))(* arity *)
  ; IMov (RegOffset (heap_reg, 1), Label label)             (* code pointer *)
  ; IMov (RegOffset (heap_reg, 2), Const num_fv)]            (* free var count*)
  @ store_free_vars free_vars @ (* free variable values *)
  [ IMov (Reg ret_reg, Reg heap_reg)  (* obtain closure pointer *)
  ; IAdd (Reg ret_reg, function_tag)] (* tag pointer *)
  @ if (size mod 2 = 0) then
    [IAdd (Reg heap_reg, Const (Int64.of_int(size * 8)))]  (* update heap ptr *)
  else 
    [IAdd (Reg heap_reg, Const (Int64.of_int ((size + 1) * 8)))]  (* update heap ptr *)

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
    IJe (Label lbl)
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
      @ compile_binop_comparator less_lbl (IJl (Label less_lbl))
    | Eq -> (* Doesn't need a type checking *)
      let eq_lbl =  equal_gensym () in
      compile_binop_preamble l AnyT r AnyT env compiler
      @ compile_binop_comparator eq_lbl (IJe (Label eq_lbl))
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
    IJe  (Label false_lbl)
  ] in
  preamble
  @ (compile t env)
  @ [ IJmp (Label done_lbl); ILabel (false_lbl) ]
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
  | ClosureT -> [ISub (Reg ret_reg, function_tag)]
  | AnyT    -> []

let encode (t: dtype): instruction list =
  match t with
  | IntT    -> [ISal (Reg ret_reg, Const 1L)]
  | BoolT   -> [IShl (Reg ret_reg, Const 63L); IAdd (Reg ret_reg, Const 1L)]
  | TupleT  -> [IAdd (Reg ret_reg, tuple_tag)]
  | RecordT -> [IAdd (Reg ret_reg, record_tag)]
  | ClosureT -> [IAdd (Reg ret_reg, function_tag)]
  | AnyT    -> []

(* THIS IS WRONG! This must be done inside compile_args. To evaluate the 
  n-th argument it is necessary to know the location of the previous n-1 args
  (i.e. have them in the environment) to avoid overwriting them. *)
let rec save_args ?(types= []) (comp_exprs: instruction list list) (env: env): instruction list =
  let new_env, loc_arg = extend_env (tmp_gensym ()) env in
  match comp_exprs, types with
  | [], _ -> []
  | compiled::tail, [] -> compiled @ [IMov (loc_arg, Reg ret_reg)] @ save_args tail new_env
  | compiled::tail, t::ts -> compiled @ type_checking (Reg ret_reg) t @ decode t @
    [IMov (loc_arg, Reg ret_reg)] @ save_args tail new_env ~types:ts

(* 
  PASS ARGS - FUNCTION CALL
  Passes args from stack to calling convention register
  (or pushing into stack)
*)
let rec pass_args (num_args: int) (regs: arg list) (env: env): instruction list =
  let new_env, loc_arg = extend_env (tmp_gensym ()) env in
  match regs with
    | [] -> 
      if num_args > 0 then 
        pass_args (num_args - 1) [] new_env 
        @ [IMov (Reg aux_reg, loc_arg); IPush (Reg aux_reg)]
      else []
    | x::xs -> 
      if num_args > 0 then 
        pass_args (num_args - 1) xs new_env 
        @ [IMov (Reg aux_reg, loc_arg); IMov (x, Reg aux_reg)] 
      else []


(* compiles a system function call*)
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
    saved_args @ pushed_regs @ passed_args @ [ICall (Label fname)] @ restore_rsp @ popped_regs @ encode type_return
    (* @ type_checking (Reg ret_reg) type_return *)


(* -----------------------------------
  |       FIRST ORDER FUNCTIONS      |                
  ------------------------------------*)

(* compiles arguments for a function call *)
let rec compile_args (args: expr list) (env: env) 
  (compiler: expr -> env -> instruction list) : instruction list =
  match args with
  | [] -> []
  | hd::tl -> 
    let new_env, loc = extend_env (tmp_gensym ()) env in
    compiler hd env
    @ IMov (loc, Reg ret_reg)::compile_args tl new_env compiler

(* 
  Compiles the call of a First Order Function
  Uses the closure generated.
*)
let compile_fof_call  (funexpr: expr) (args: expr list) (env: env)
  (compiler: expr -> env -> instruction list) : instruction list = 
  let modified_arg_regs = [
   Reg RSI; Reg RDX; Reg RCX; Reg R8; Reg R9
  ] in
  (* let closure = let_lookup fname env in *)
  let argc = 1 + List.length args in (* NEW *)
  let compiled_clos = compiler funexpr env in
  let label = gensym "fun_clos" in
  let new_env, slot_clos = extend_env label env in
  let eval_args   = compile_args args new_env compiler in
  let pushed_args = List.rev (push_regs argc arg_regs) in
  let passed_args = pass_args (argc - 1) modified_arg_regs new_env in
  let restore_rsp = if argc > 6 
    then [IAdd (Reg RSP, Const (Int64.of_int (8 * (argc - 6))))]
    else [] in
  let restore_regs   = pop_regs argc arg_regs in
  compiled_clos @ [ IComment "Saving the closure"; IMov (slot_clos, Reg ret_reg)] @
  type_checking (Reg ret_reg) ClosureT @ eval_args
  @ [IMov (Reg ret_reg, slot_clos);                   (* The Closure *)
    ISub (Reg ret_reg, function_tag);               (* Untag *)
    IMov (Reg aux_reg, RegOffset (ret_reg, 0))]     (* Arity *) 
  @ check_arity (Reg aux_reg) (List.length args) 
  @ pushed_args 
  @ [IMov (Reg RDI, slot_clos)] (* NEW *)
  @ passed_args 
  @ [IMov (Reg ret_reg, RegOffset (ret_reg, 1));     (* Label *)
    ICall (Reg ret_reg)] 
  @ restore_rsp @ restore_regs    

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
  

(*------------------------------------
  |                MAIN              |                
  ------------------------------------*)

(* THE MAIN compiler function *)
let rec compile_expr (e : expr) (env: env) : instruction list =
  match e with 
  | Num n   ->  
      [ IComment "Num (value)" ;
      IMov ((Reg ret_reg), Const (encode_int n)) ]
  | Bool p  ->  
      [ IComment "Bool (value)" ;
      IMov ((Reg ret_reg), Const (encode_bool p)) ]
  | UnOp (op, e)  -> 
    [IComment "UnOp: unary operator"] @
    compile_unop (compile_expr e env) op
    @ [IComment "end unOp"]
  | Id x    ->  
    [ IComment ("Id: identifier lookup "^ x);
    IMov (Reg ret_reg, let_lookup x env) ;
    IComment "end Id"]
  | Fun (ids, body) ->
    (* CLOSURE CREATION *)
    let arity = (Int64.of_int (List.length ids))      in  (* Arity of function *)
    let label = (gensym "lambda")                     in  (* Label of function *)
    let free  = remove_repeated @@ free_vars body ids in  (* Free Ids *)
    let (lenv, senv) = env                            in  (* Split lenv and senv *)
    let closure_definition =
      [IComment (label^" Closure")] 
      @ compile_closure label arity free lenv @
      [IComment "end Closure" ; IEmpty ]   
    (* Now the closure is in RAX *)
    in
    (* BODY COMPILATION *)
    let new_lenv = let_env_from_params (["self"]@ids) empty_env in    (* Adding params definitions *)
    let rec add_freevars_stack (fv: string list) (env: env): instruction list * env =
      let nro_arg = List.length free - List.length fv in
      match fv with
      | [] -> [], env
      | id::ids -> 
        let new_env, slot = extend_env id env in
        let instrs, ret_env = add_freevars_stack ids new_env in
        ([IMov (Reg aux_reg, RegOffset (RDI, 3 + nro_arg));
          IMov (slot, Reg aux_reg)] 
        @ instrs, ret_env)
    in
    let add_fv, fun_env   = add_freevars_stack free (new_lenv, senv) in
    let stack_offset = Int64.of_int (stack_offset_for_local body) in  (* Stack Offset needed *)
    [
      IJmp (Label ("end_of_"^label));
      IEmpty;
      IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
      IComment ("==> LAMBDA: "^label);
      IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
      ILabel label
    ] 
    @ callee_prologue 
    @ [ISub (Reg RSP, Const stack_offset)]
    @ [ISub (Reg RDI, function_tag)]
    @ add_fv
    @ [IAdd (Reg RDI, function_tag)]
    @ compile_expr body fun_env
    @ callee_epilogue 
    @ [ IRet; IEmpty; IComment "End of Lambda Definition"; ILabel ("end_of_"^label) ]
    @ closure_definition
  | Let (vals,b) -> 
      let compiled_vals, new_env    = compile_let_exprs compile_expr env vals in
      IComment "let-binding"::compiled_vals @ (compile_expr b new_env)
      @ [IComment "end let-binding"]
  | BinOp (op, l, r) -> 
    IComment "BinOp application"::
    compile_binop op l r env compile_expr
    @ [IComment "end BinOp"]
  | If (c, t, f) -> IComment "If statement"::(compile_expr c env) 
      @ type_checking (Reg ret_reg) BoolT 
      @ compile_if compile_expr t f env
      @ [IComment "end If"]
  | Sys (fname, args) -> 
    [IComment (Printf.sprintf "Sys: %s" fname)]
    @ compile_sys_call fname args env compile_expr
    @ [IComment (Printf.sprintf "end Sys (%s)" fname)]
  | App (fname, args) -> 
    [IComment (Printf.sprintf "App")]
    @ compile_fof_call fname args env compile_expr
    @ [IComment (Printf.sprintf "end App")]
  | Tuple exprs -> 
    [IComment "Tuple: constructor"]
    @ compile_tuple exprs env compile_expr
    @ [IComment "end Tuple"]
  | Get (tup, index) -> 
    [IComment "Get: tuple element access"]
    @ compile_get tup index env compile_expr
    @ [IComment "end Get"]
  | Set (tup, index, value) -> 
    [IComment "Set: tuple element mutation"]
    @ compile_set tup index value env compile_expr
    @ [IComment "end Set"]
  | Length tup -> 
    [IComment "Length: tuple"]
    @ compile_expr tup env @ type_checking (Reg ret_reg) TupleT
    @ [ ISub ((Reg ret_reg), tuple_tag) ;
      IMov ((Reg ret_reg), RegOffset (RAX, 0)) ;
      IComment "end Length"]
  | Void -> [IComment "(void)" ; INop]


(* Label for handling errors *)
let error_type_handler =
 [ILabel "error_type_handler";
  IMov (Reg RSI, (Reg ret_reg));
  IMov (Reg RDI, (Reg err_code_reg));
  ICall (Label "error")]

let error_index_handler =
  [ILabel "error_index_handler";
    IMov (Reg RSI, (Reg type_code_reg));
    IMov (Reg RDI, (Reg err_code_reg));
    ICall (Label "error")]
  
let error_arity_handler =
  [ILabel "error_arity_handler";
    IMov (Reg RSI, (Reg type_code_reg));
    IMov (Reg RDI, (Reg err_code_reg));
    ICall (Label "error")]


(* Compiles definitions for first order functions *)
let compile_fundef (fname: string) (params: string list) (body: expr) 
  (env: env): instruction list =
  let _, senv = env in
  let flbl = fname in
  let lenv = let_env_from_params ([fname]@params) empty_env in
  let stack_offset = Int64.of_int (stack_offset_for_local body) in
  [IJmp (Label ("end_function_"^flbl)) ;IEmpty;
   IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
   IComment ("==> "^fname);
   IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
  ILabel flbl ]
  @ callee_prologue 
  @ [ISub (Reg RSP, Const stack_offset)]
  @ compile_expr body (lenv, senv) @ callee_epilogue 
  @ [IRet; IEmpty; ILabel ("end_function_"^flbl)]


(* Compiles a declaration for a function. Returns a instr list for compiled
functions and another for foreign function "extern"s *)
let compile_declaration (dec: decl) (env: env) 
(funs: instruction list) (exts: instruction list): instruction list * instruction list =
  match dec with 
  | FunDef (fname, params, body) -> 
    compile_fundef fname params body env @ funs , exts
  | SysFunDef (fname, _, _) -> funs, [IExtern fname] @ exts
  | RecDef (_, _) -> (*compile_recdef id fields env @*) funs, exts

(* Compiles declarations for functions *)  
let rec compile_declarations (decls: decl list) (env: env) :
instruction list * instruction list =
  match decls with
  | [] -> [], []
  | dec::tail ->
    let funs, exts = compile_declarations tail env in
    compile_declaration dec env funs exts


(*
  ADD CLOSURES TO ENV - FUNCTIONS
  Since the function is allocated in heap, its reference
  can be stored in stack to be called without problems :)
*)
let rec add_closures_to_env (ds: decl list) (env: let_env) : instruction list * let_env =
  match ds with
  | [] -> [], env
  | d::tail ->
    match d with
    | FunDef (fname, params, body) ->
      let fv_raw = remove_repeated @@ free_vars body [] in
      let fv = List.filter (fun x -> not (List.mem x @@ params@[fname])) fv_raw in
      if (List.mem_assoc fname env) then 
        (Fmt.failwith "Duplicate function name: %s" fname)
      else
        let new_env, loc = extend_let_env fname env in
        let arity = (Int64.of_int (List.length params)) in
        let instrs, updated_env = add_closures_to_env tail new_env in
        [IComment (fname^"Closure")]
        @ compile_closure fname arity fv new_env @
        [ IMov (loc, Reg RAX)
        ; IComment "end Closure"
        ; IEmpty ]
        @ instrs, updated_env
    | _ -> add_closures_to_env tail env


let rec let_env_from_decls (decls: decl list) (env: let_env) : let_env =
  match decls with
  | [] -> []
  | hd::tl ->
    match hd with
    | FunDef (fname, _, _) -> 
      let new_env, _ = extend_let_env fname env in
      let_env_from_decls tl new_env
    | _ -> let_env_from_decls tl env

(* default system functions *)
let default_sys_env : sys_env =
  ["print", ([AnyT], AnyT) ;
  "raw_print", ([AnyT], AnyT)]

(* Generates the compiled program *)
let compile_prog : prog Fmt.t =
  fun fmt p ->
    match p with Program (decs, exp) ->
      let closures, fenv  = add_closures_to_env decs empty_env in
      let senv = sys_env_from_decls decs default_sys_env in
      let env  = fenv, senv in
      let functions, externs = compile_declarations decs env in
      let instrs = compile_expr exp env in
      let stack_offset = Int64.of_int (stack_offset_for_prog p) in
      Fmt.pf fmt 
"section .text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      EXTERNAL C FUNCTIONS     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Also called system functions
extern error
extern print
extern raw_print
%a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          MAIN PROGRAM         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
global our_code_starts_here
our_code_starts_here:
%a
  mov  R15, RDI                 ;; load R15 with the passed HEAP
  add  R15, 7                   ;; Add 7 to the next multiple of 7
  mov  R11, 0xfffffffffffffff8  ;; R11 is now 11111...1000
  and  R15, R11                 ;; Round back down
%a" 
      pp_instrs externs pp_instrs callee_prologue 
      pp_instrs (
      [ISub (Reg RSP, Const stack_offset)] @ functions @ closures
      @ instrs @ callee_epilogue @ [IRet]
      @ error_type_handler @ error_index_handler @ error_arity_handler) 

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse_prog (sexp_list_from_string src)) compile_prog
