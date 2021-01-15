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

let try_gc (n: int): instruction list = [

  IEmpty;
  IComment "Trying GC for allocating memory";

  IPush (Reg R10);
  IPush (Reg RDI);
  IPush (Reg RSI);
  IPush (Reg RDX);
  IPush (Reg RCX);

  IMov (Reg RDI, Reg heap_reg);           (* Alloc Pointer *)
  IMov (Reg RSI, Const (Int64.of_int n)); (* Words Needed *)
  IMov (Reg RDX, Reg RBP);  (* Current Frame *)
  IMov (Reg RCX, Reg RSP);  (* Current Stack Pointer *)
  ICall (Label "try_gc");
  IMov (Reg heap_reg, Reg ret_reg); (*Returns the new Alloc pointer*)

  IPop (Reg RCX);
  IPop (Reg RDX);
  IPop (Reg RSI);
  IPop (Reg RDI);
  IPop (Reg R10);

  IComment "END of GC";
  IEmpty;
]

(* Counts the max number of expressions mantained in stack at the same time *)
let rec varcount (e:expr): int =
  match e with
  | Num _ -> 0
  | Bool _ -> 0
  | Id _ -> 0
  | UnOp (_, sub_ex) -> varcount sub_ex
  | BinOp (_, l_ex, r_ex) -> 1 + max (varcount l_ex) (varcount r_ex)
  | Let (defs, body_ex) ->
    1 + max (List.fold_left max 0 (List.map (fun (_,y)-> varcount y) defs)) (varcount body_ex)
  | LetRec (ds, b) ->
    1 + max (List.fold_left max 0 (List.map (fun (_,y) -> varcount y) ds)) (varcount b)
  | If (_, t_ex, f_ex) -> max (varcount t_ex) (varcount f_ex)
  | Fun (_, _) -> 2
  | App (_, args_ex) -> List.fold_left max 0 (List.map varcount args_ex) + List.length args_ex + 2
  | Sys (_, args_ex)-> List.fold_left max 0 (List.map varcount args_ex) + List.length args_ex
  | Tuple exprs -> List.fold_left max 0 (List.map varcount exprs) + List.length exprs
  | Get (tuple, index) -> 1 + max (varcount tuple) (varcount index)
  | Set (tuple, index, value) -> 2 + max (max (varcount tuple) (varcount index)) (varcount value)
  | Length tuple -> varcount tuple
  | Void -> 0

(* This varcount also considers defined functions (1st order) *)
let rec varcount_funs (ds: decl list): int =
  match ds with 
  | [] -> 0
  | d::ds ->
    match d with
    | FunDef (_, _, _) -> 1 + varcount_funs ds
    | RecDef (_, flds) -> 2 + List.length flds + varcount_funs ds
    | _ -> varcount_funs ds

(* Varcount for an entire program *)
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
  | LetRec (ids, b) -> free_vars b (saved @ (List.map (fun (x,_) -> x) ids))
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

(*
  Free ids
  Another way to determine unbound identifiers in an expression.
*)
let rec free_ids (e: expr) (bound_ids: string list) : string list =
  let rec recmap bids elist =
    match elist with
    | [] -> []
    | hd::tl -> 
      let ids = free_ids hd bids in
      ids @ recmap (ids @ bids) tl
  in
  let map = 
    recmap bound_ids
  in
  match e with
  | Num _ -> []
  | Bool _ -> []
  | Id id -> if List.mem id bound_ids then [] else [id]
  | UnOp (_, x) -> free_ids x bound_ids
  | BinOp (_, x, y) -> map [x ; y]
  | Let (bindings, e) ->
    let ids, vals = List.split bindings in 
    recmap (ids @ bound_ids) (vals @ [e])
  | LetRec (bindings, e) ->
    let ids, lambdas = List.split bindings in
    recmap (ids @ bound_ids) (lambdas @ [e])
  | If (c, t, e) -> map [c ; t ; e]
  | Fun (params, body) -> free_ids body (params @ bound_ids)
  | App (f, args) -> map (f::args)
  | Sys (_, args) -> map args
  | Tuple elems -> map elems
  | Get (t, i) -> map [t ; i]
  | Set (t, i, e) -> map [t ; i ; e]
  | Length t -> free_ids t bound_ids
  | Void -> []


(**
  Usage:
    compile_closure label arity free_id_list env
  Compiles a function closure.

  Closure structure:
    [+0] arity, encoded
    [+8] code pointer
    [+16] free id count
    [+24] free id 0
    ...
    [+(16 + 8*k)] free id k
 *)
let compile_closure (label: string) (arity: int64) (free_vars: string list)
(outer_env: let_env) : instruction list =
  let size = 3 + List.length free_vars in
  let num_fv = Int64.of_int (List.length free_vars) in 

  (* stores free ids in the closure *)
  let bind_free_ids (ids : string list) =
    let instructions offset id = 
      [IComment ("Enclosing "^id) ;
      IMov (Reg aux_reg, let_lookup id (outer_env, [])) ;
      IMov (RegOffset (heap_reg, 3 + offset), Reg aux_reg)]
    in
    List.concat @@ List.mapi instructions ids
  in
  try_gc size @
  [ IMov (RegOffset (heap_reg, 0), Const (encode_int arity)) (* arity *)
  ; IMov (RegOffset (heap_reg, 1), Label label)       (* code pointer *)
  ; IMov (RegOffset (heap_reg, 2), Const num_fv)]    (* free var count*)
  @ bind_free_ids free_vars @         (* store values of enclosed ids *)
  [ IMov (Reg ret_reg, Reg heap_reg)        (* obtain closure pointer *)
  ; IAdd (Reg ret_reg, function_tag)]                  (* tag pointer *)
  (* update heap ptr *)
  @ [IAdd (Reg heap_reg, Const (Int64.of_int(size * 8)))]


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
  let argc = 1 + List.length args in                  (* Also considering self reference *)
  let compiled_clos = compiler funexpr env in         (* The Body *)
  let label = gensym "fun_clos" in    
  let new_env, slot_clos = extend_env label env in
  let eval_args   = compile_args args new_env compiler in             (* Function Args *)
  let pushed_args = List.rev (push_regs argc arg_regs) in             (* Push of required registers*)
  let passed_args = pass_args (argc - 1) modified_arg_regs new_env in (* Passing args (not self ref) *)
  let restore_rsp = if argc > 6 
    then [IAdd (Reg RSP, Const (Int64.of_int (8 * (argc - 6))))]
    else [] in
  let restore_regs   = pop_regs argc arg_regs in (* Pop calling registers *)
  compiled_clos 
  @ [ IComment "Saving the closure"; IMov (slot_clos, Reg ret_reg)] (* Passing the closure *)
  @ type_checking (Reg ret_reg) ClosureT          (* Typechecking *)
  @ eval_args  
  @ [IMov (Reg ret_reg, slot_clos);               (* The Closure *)
    ISub (Reg ret_reg, function_tag);             (* Untag *)
    IMov (Reg aux_reg, RegOffset (ret_reg, 0))]   (* Arity *) 
  @ check_arity (Reg aux_reg) (List.length args) 
  @ pushed_args 
  @ [IMov (Reg RDI, slot_clos)] (* NEW *)
  @ passed_args 
  @ [IMov (Reg ret_reg, RegOffset (ret_reg, 1));     (* Label *)
    IPush (Reg closure_reg) ; (* backup closure register *)
    ICall (Reg ret_reg) ;
    IPop (Reg closure_reg)] (* restore closure register *) 
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
  try_gc (List.length elems + 1) @  (* Garbage Collector *)
  [IMov (RegOffset (heap_reg, 0), Const (encode_int (Int64.of_int size)))] @ 
  assign_tuple @
  [
    IMov ((Reg ret_reg), Reg heap_reg);     (* Start creating the tuple value itself *)
    IAdd ((Reg ret_reg), tuple_tag);         (* Tag the tuple *)
    IAdd (Reg heap_reg, Const (Int64.of_int (size*8 + 8 ))) (* Bump the heap pointer *)
  ]
  (* @ if (size+1) mod 2 = 1 then
    [IAdd (Reg heap_reg, Const 8L)]
  else [] *)
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


(* -----------------------------------
  |             LAMBDAS               |
  ------------------------------------*)
(* Compiles Lambda expressions *)
let compile_lambda (ids: string list) (body: expr) (env: env) 
  (compile_expr: expr -> env -> instruction list) : instruction list = 
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
  (* 
    ADD FREE VARS TO STACK
    Bounds to stack the references of free ids contained in
    the closure, to bring access by the regular way.
    Returns the instruction list and the environment with lodaded ids.
  *)
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
  (* Adding free vars into stack *)
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
  @ [ISub (Reg RSP, Const stack_offset)]  (* Offset for defining locals*)
  @ [ISub (Reg RDI, function_tag)]        (* Untagging Closure*)
  @ add_fv                                (* Free vars into stack *)
  @ [IAdd (Reg RDI, function_tag)]        (* Tagging again *)
  @ compile_expr body fun_env             (* Compiling Body *)
  @ callee_epilogue 
  @ [ IRet; IEmpty; IComment "End of Lambda Definition"; ILabel ("end_of_"^label) ]
  @ closure_definition

(*
  #===========================#
  #           LETREC          #
  #===========================#
*)
(* recursive lambda. it can reference itself with the proper id *)
let compile_reclambda (id: string) (params: string list) (body: expr) (env: env) 
(compile_expr: expr -> env -> instruction list) : instruction list = 
  (* CLOSURE CREATION *)
  let arity = (Int64.of_int (List.length params)) in  (* Arity of function *)
  let label = "lambda_"^(get_fun_label id) in  (* Label of function *)
  let free  = remove_repeated @@ free_vars body (id::params) in  (* Free Ids *)
  let (lenv, senv) = env in  (* Split lenv and senv *)
  let closure_definition =
    [IComment (label^" Closure")] 
    @ compile_closure label arity free lenv @
    [IComment "end Closure" ; IEmpty ]   
  (* Now the closure is in RAX *)
  in
  (* BODY COMPILATION *)
  let new_lenv = 
    (id, MReg RDI)::(let_env_from_params (id::params) empty_env)
  in (* parameters *)
  (* 
    ADD FREE VARS TO STACK
    Bounds to stack the references of free ids contained in
    the closure, to bring access by the regular way.
    Returns the instruction list and the environment with lodaded ids.
  *)
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
  (* Adding free vars into stack *)
  let add_fv, fun_env = add_freevars_stack free (new_lenv, senv) in
  let stack_offset = Int64.of_int (stack_offset_for_local body) in  
  (* Stack Offset needed *)
  [
    IJmp (Label ("end_of_"^label));
    IEmpty;
    IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
    IComment ("==> REC LAMBDA: "^label);
    IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
    ILabel label
  ] 
  @ callee_prologue 
  @ [ISub (Reg RSP, Const stack_offset)]  (* Offset for defining locals*)
  @ [ISub (Reg RDI, function_tag)]        (* Untagging Closure*)
  @ add_fv                                (* Free vars into stack *)
  @ [IAdd (Reg RDI, function_tag)]        (* Tagging again *)
  @ compile_expr body fun_env             (* Compiling Body *)
  @ callee_epilogue 
  @ [ IRet; IEmpty; IComment "End of Lambda Definition"; ILabel ("end_of_"^label) ]
  @ closure_definition


(* compiles a recursive let binding *)
let compile_letrec (compiler: expr->env->instruction list) (env:env) 
  (bindings: (string * expr) list) : instruction list * env =
  let closure_size e params = (3 + List.length (free_ids e params)) in
  let total_size =
    let size f =
      let id, e = f in
      match e with
      | Fun (params, body) -> closure_size body (id::params)
      | _ -> Fmt.failwith "letrec expects a lambda, but '%s' is bound to %a" id pp_expr e
    in
    List.fold_right (fun f s -> size f + s) bindings 0
  in

  (* this preallocates the heap space closures will occupy.
  Returns:
    compiled preallocation : instruction list
    environment containing new closures : env
    lambdas : (string * (string list) * expr) list *)
  let rec preallocate env bindings :
   instruction list * env * (string * (string list) * expr) list =
    match bindings with
    | [] -> [], env, []
    | (id, expr)::tl ->
      begin
        match expr with
        | Fun (params, fbody) -> 
          let new_env, loc = extend_env id env in
          let closure_size = Int64.of_int (closure_size fbody (id::params) * 8) in
          let instr_tl, final_env, binding_tl = preallocate new_env tl in
          [IMov (Reg aux_reg, Reg ret_reg) ;
           IAdd (Reg aux_reg, function_tag) ;
           IMov (loc, Reg aux_reg) ;
           IAdd (Reg ret_reg, Const closure_size)] 
          @ instr_tl,
          final_env,
          ((id, params, fbody)::binding_tl)
        | _ -> 
          Fmt.failwith "letrec expects a lambda, but '%s' is bound to %a" id pp_expr expr
      end
  in

  let preallocation, final_env, processed_bindings = 
    preallocate env bindings 
  in

  let rec compile_lambdas (bindings: (string * (string list) * expr) list ) =
    match bindings with
    | [] -> []
    | (id, params, body)::tl ->
      (* heap space should have already been preallocated *)
      compile_reclambda id params body final_env compiler
      @ compile_lambdas tl 
  in

  try_gc total_size @
  [IPush (Reg ret_reg)
  ;IMov (Reg ret_reg, Reg heap_reg)] @
  preallocation @
  [IPop (Reg ret_reg)] @
  compile_lambdas processed_bindings,
  final_env


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
  | Fun (ids, body) -> compile_lambda ids body env compile_expr    
  | Let (vals,b) -> 
      let compiled_vals, new_env    = compile_let_exprs compile_expr env vals in
      IComment "let-binding"::compiled_vals @ (compile_expr b new_env)
      @ [IComment "end let-binding"]
  | LetRec (bindings, body) ->
    let cexprs, new_env = compile_letrec compile_expr env bindings in
    [IComment "letrec"] @ 
    cexprs @
    [IComment "end letrec"] @
    (compile_expr body new_env)
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


(* Compiles definitions for first order functions *)
let compile_fundef (fname: string) (params: string list) (body: expr) 
(env: env): instruction list =
  let stack_offset = Int64.of_int (stack_offset_for_local body) in
  let lenv, senv = env in
  let flbl = get_fun_label fname in
  let argenv = let_env_from_params (fname::params) empty_env in
  let fids = 
    let bound_ids, _ = List.split argenv in free_ids body bound_ids 
  in
  let closed_lenv = enclose_lenv fids argenv in
  [IJmp (Label ("end_function_"^flbl)) ;IEmpty;
   IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
   IComment ("==> "^fname);
   IComment ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;";
  ILabel flbl ]
  @ callee_prologue 
  @ [ISub (Reg RSP, Const stack_offset) (* allocate stack space *)
  ; IMov (Reg closure_reg, Reg RDI) (* closure is in first arg *)
  ; ISub (Reg closure_reg, function_tag)] (* untag closure *)
  @ compile_expr body (closed_lenv, senv)
  @ callee_epilogue 
  @ [IRet ; ILabel ("end_function_"^flbl) ; IEmpty]
  @ compile_closure flbl (Int64.of_int @@ List.length params) fids lenv

(* Compiles constructor, getter, setter and type-checker for a record *)
(* let compile_recdef (id: string) (fields: string list) (env: env) = *)

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

(* default system functions *)
let default_sys_env : sys_env =
  ["print", ([AnyT], AnyT) ;
  "raw_print", ([AnyT], AnyT)]

(* compiles functions for a record *)
let compile_record (id: string) (fields: string list) (env: env) :
  instruction list * env =

  let lenv, senv = env in
  (* size in bytes of the record (header and fields) *)
  let rec_size = Int64.of_int @@ (List.length fields + 1) * 8 in 
  (* tag of this record type *)
  let rec_tag = (* (len(fields) << 32) + record_counter() *)
    Int64.add 
      (Int64.shift_left (Int64.of_int @@ record_counter ()) 32) 
      (Int64.of_int @@ List.length fields)
    (* tag includes record size for debugging purposes *)
  in

  let constructor_label = id^"_constructor" in
  let constructor : instruction list =
    let cons_env = let_env_from_params ("self"::fields) empty_env, senv in
    let rec write_flds (fs: string list) (offset: int) : instruction list =
      match fs with
      | [] -> []
      | hd::tl -> 
        [IMov ((RegOffset (heap_reg, offset)), let_lookup hd cons_env)]
        @ write_flds tl (offset + 1)
    in
    [ ILabel constructor_label]
    @ callee_prologue @
    (* record tag word = rectag|size *)
    [ IMov (Reg ret_reg, Const rec_tag)
    ; IMov (RegOffset (heap_reg, 0), Reg ret_reg)]
    (* move values to heap *)
    @ write_flds fields 1 @
    (* get and tag pointer *)
    [ IMov (Reg ret_reg, Reg heap_reg)
    ; IAdd (Reg ret_reg, record_tag)
    (* bump heap pointer *)
    ; IAdd (Reg heap_reg, Const rec_size)]
    (* return *)
    @ callee_epilogue @
    [ IRet ]
  in

  let getter_labels = List.map (fun fld -> id^"_"^fld) fields in
  let getter (index: int) (name: string) : instruction list =
    let rec_reg = RSI in
    [ILabel (id^"_"^name)]
    @ callee_prologue 
    @ type_checking (Reg rec_reg) RecordT
    (* untag pointer and check type *)
    @ check_rectag rec_reg rec_tag @
    (* retrieve element at index *)
    [ IMov (Reg ret_reg, RegOffset (rec_reg, index + 1))]
    @ callee_epilogue @ 
    [ IRet ]
  in

  let checker_label = id^"?" in
  let checker : instruction list =
    let rec_reg = RSI in
    let end_lbl = id^"?_end" in
    
    [ ILabel checker_label ]
    @ callee_prologue @
    [ IMov (Reg ret_reg, Const false_encoding)
    (* check pointer has correct type *)
    ; IMov (Reg aux_reg, Reg rec_reg)
    ; IAnd (Reg aux_reg, tag_bitmask)
    ; ICmp (Reg aux_reg, record_tag)
    ; IJne (Label end_lbl)
    (* check value has correct rectag *)
    ; ISub (Reg rec_reg, record_tag)
    ; IMov (Reg aux_reg, Const rec_tag)
    ; ICmp (RegOffset(rec_reg, 0), Reg aux_reg)
    ; IJne (Label end_lbl)
    (* return true if both tests pass *)
    ; IMov (Reg ret_reg, Const true_encoding)
    ; ILabel end_lbl]
    @ callee_epilogue @ 
    [ IRet ]
  in

  let cons_id = check_function_name id env in
  let getter_ids = 
    List.map (fun fld -> check_function_name (id^"-"^fld) env) fields 
  in
  let checker_id = check_function_name checker_label env in

  let new_env = 
    let cons_env, _ = extend_env cons_id env in
    let get_env, _ = multi_extend_env getter_ids cons_env in
    let check_env, _ = extend_env checker_id get_env in
    check_env
  in
  
  let move_getter_closures (index: int) _ : instruction list =
    compile_closure (List.nth getter_labels index) 1L [] lenv
    @ [IMov (let_lookup (List.nth getter_ids index) new_env, Reg ret_reg)]
  in

  let end_lbl = id^"_end" in
  [ IComment "======================================"
  ; IComment ("Record definition: "^id)
  ; IComment (Printf.sprintf "tag: 0x%Lx" rec_tag)
  ; IComment "======================================"
  ; IJmp (Label end_lbl)]
  @ constructor
  @ (List.concat @@ List.mapi getter fields)
  @ checker
  @ [ILabel end_lbl]
  @ compile_closure 
    constructor_label (Int64.of_int @@ List.length fields) [] lenv
  @ [IMov (let_lookup cons_id new_env, Reg ret_reg)]
  @ (List.concat @@ List.mapi move_getter_closures getter_ids)
  @ compile_closure checker_label 1L [] lenv
  @ [IMov (let_lookup checker_id new_env, Reg ret_reg)]

  , new_env

(* compiles a program to asm *)
let rec assemble_prog (prog: prog) (env: env): 
  instruction list * instruction list =
  let Program (decls, main) = prog in
  match decls with
  | [] -> [], 
    [IEmpty; IComment "<<========# Main #========>>"; IEmpty; ILabel "__main__"] 
    @ compile_expr main env
  | hd::tl ->
    begin
      match hd with

      | FunDef (id, params, body) ->
        (* This semantically equivalent to 
          (letrec (id (lambda (params...) body))
            ...) 
        *)
        ignore @@ check_function_name id env ;
        let new_env, loc = extend_env id env in
        let externs, instrs = assemble_prog (Program (tl, main)) new_env in
        externs,
        compile_fundef id params body env
        @ [IMov (loc, Reg ret_reg)]
        @ instrs

      | SysFunDef (label, ptypes, rtype) ->
        let lenv, senv = env in
        let new_sysenv = extend_sys_env label ptypes rtype senv in
        let externs, instrs = 
          assemble_prog (Program (tl, main)) (lenv, new_sysenv) 
        in
        (IExtern label)::externs, instrs

      | RecDef (id, fields) ->
        let instructions, new_env = compile_record id fields env in
        let externs, inst_tail = assemble_prog (Program (tl, main)) new_env in
        externs, instructions @ inst_tail
      
    end

(* Sets the value of Stack bottom *)
let set_stack_bottom : instruction list =
  [
    IPush (Reg RDI);
    IMov (Reg RDI, Reg RBP);
    ICall (Label "set_stack_bottom");
    IPop (Reg RDI);
  ]

(* Generates the compiled program *)
let compile_prog : prog Fmt.t =
  fun fmt p ->
    let env  = empty_env, default_sys_env in
    (* let functions, externs = compile_declarations decs env in *)
    let externs, instrs = assemble_prog p env in
    let stack_offset = Int64.of_int (stack_offset_for_prog p) in
    Fmt.pf fmt 
"section .text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      EXTERNAL C FUNCTIONS     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Also called system functions
extern error
extern print
extern try_gc
extern raw_print
extern set_stack_bottom

%a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          MAIN PROGRAM         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
global our_code_starts_here
our_code_starts_here:
%a
  mov  R15, RDI                 ;; load R15 with the passed HEAP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting Stack Bottom in RTSYS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%a

%a" 
      pp_instrs externs 
      pp_instrs callee_prologue 
      pp_instrs set_stack_bottom
      pp_instrs (
        [ISub (Reg RSP, Const stack_offset)]
        @ instrs 
        @ callee_epilogue 
        @ [IRet] 
        @ error_handler) 

(* The Pipeline *)
let compile_src = 
  let open Parse in
  Fmt.using (fun src -> parse_prog (sexp_list_from_string src)) compile_prog
