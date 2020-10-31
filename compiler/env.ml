open Asm
open Ast
(** Compiler Environment **)

(* Memory location *)
type memloc =
| MReg of reg
| StackOffset of int

let memloc_to_arg (m: memloc) : arg =
  match m with
  | MReg r -> Asm.Reg r
  | StackOffset n -> Asm.RegOffset (RBP, n)

let pp_memloc fmt loc =
  match loc with
  | MReg r -> pp_arg fmt (Reg r)
  | StackOffset n -> pp_arg fmt (RegOffset (RBP, n))


(*---------------------------
| Environments (namespaces) |
---------------------------*)
(* 
  Environment as lists of pairs (i.e. association lists) 
  First field  -> Symbol
  Second field -> Memory location
*)
type let_env = (string * memloc) list
(* An "fenv" contains function definitions *)
type fun_env = (string * int) list
(* sys_env contains foreign function definitions *)
type sys_env = (string * (dtype list * dtype)) list
(* 
An environment is composed of three separate namespaces:
  > let-bound variables
  > functions
  > foreign functions
*)
type env = let_env * fun_env * sys_env

(* Alias for empty environment *)
let empty_env : let_env = []
let empty_fun_env : fun_env = []
let empty_sys_env : sys_env = []

(* pretty printers *)
let pp_fun_env fmt lenv =
  (Fmt.list (Fmt.pair Fmt.string Fmt.int)) fmt lenv 

let pp_let_env fmt fenv =
  (Fmt.list (Fmt.pair Fmt.string pp_memloc)) fmt fenv

let pp_env fmt env =
  match env with lenv, fenv ->
    Fmt.pf fmt "variables=%a, functions=%a" pp_let_env lenv pp_fun_env fenv


(*------------------
| lookup functions |
------------------*)

(* looks up a let-bound variable in an environment. Returns its memory
location if present, fails with an "Unbound identifier" error if not.*)
let let_lookup (name: string) (env: env) : arg =
  match env with lenv, _ , _->
    if List.mem_assoc name lenv then memloc_to_arg @@ List.assoc name lenv
    else Fmt.failwith "Unbound identifier: %s" name

(* looks up a function definition in an environment. Returns its arity
if present; fails with an "Undefined function" error if not. *)
let fun_lookup (name: string) (env: env) : int =
  match env with _, fenv, _ -> 
    if List.mem_assoc name fenv then List.assoc name fenv
    else Fmt.failwith "Undefined function: %s" name

(* looks up a system (foreign) function in an env. Returns its parameter
and return types; fails with and "Undefined system function" error if not.*)
let rec sys_lookup (name: string) (env: env) : (dtype list * dtype) =
  match env with _ , _, senv ->
    if List.mem_assoc name senv then List.assoc name senv 
    else Fmt.failwith "Undefined system function: %s" name


(*--------------
| Slot getters |
--------------*)

(* Returns the next available memory slot in an environment. Memory slots
for let-bound variables are stack positions, represented as an offset 
from RBP. *)
let get_let_slot (env: let_env) : memloc =
  match env with 
  | [] -> StackOffset 1
  | (_, loc)::_ -> 
    begin
      match loc with
      | MReg _ -> StackOffset 1
      | StackOffset n -> StackOffset (n + 1)
    end

(* Returns the memory location for a function arguments. In the x64 calling
  convention the first 6 arguments go into registers, and the rest are pushed
  into the stack. 
  
  Unlike stack offsets for let-bound identifiers, stack offsets for arguments
  are "below" RBP, since this is how a function accesses the arguments it
  received from the caller. *)
  let get_arg_slot (env: let_env) : memloc =
    let arg_regs = [MReg RSI; MReg RDI; MReg RDX; MReg RCX; MReg R8; MReg R9] in
    let n = List.length env in
    if n < 6 
      then List.nth arg_regs (n + 1) 
      else StackOffset (4 - n) (*  arg_n = RBP + 8 * ((n - 6) + 2)  *)


(*-----------------------
| Environment extenders |
-----------------------*)

(* Extends the environment with a new id and its respective offset in stack *)
let extend_let_env (name: string) (env: let_env) : (let_env * arg) =
  let loc = get_let_slot env in
  ((name, loc)::env, memloc_to_arg loc)

(* Extends an environment with a new let-bound variable *)
let extend_env (name: string) (env: env) : (env * arg) =
  match env with lenv, fenv, senv ->
  let new_lenv, arg = extend_let_env name lenv in
  ((new_lenv, fenv, senv), arg)

(* Extends a let_env with function parameters *)
let extend_arg_env (name: string) (env: let_env) : (let_env * arg) =
  let loc = get_arg_slot env in
  ((name, loc)::env, memloc_to_arg loc)

let rec existsInEnv elem lst =
  match lst with
  | [] -> false
  | (name, _)::tl -> elem = name || existsInEnv elem tl

(* Extends a fun_env with a new function *)
let extend_fun_env (fname: string) (arity: int) (fenv: fun_env) : fun_env =
  if existsInEnv fname fenv then
    Fmt.failwith "Duplicated function name: %s" fname
  else (fname, arity)::fenv

(* Extends a sys_env with a new function *)
let extend_sys_env (fname: string) (params: dtype list) (return_type: dtype)
  (senv: sys_env) : sys_env = 
  if existsInEnv fname senv then
    Fmt.failwith "Duplicated system function name: %s" fname
  else (fname, (params, return_type))::senv


(*--------------------------
| Environment constructors |
--------------------------*)

(* Returns an environment to compile a function with k parameters *)
let rec let_env_from_params (params: string list) (env: let_env) : let_env =
  match params with
  | [] -> env
  | name::tail -> 
    let new_env, _ = extend_arg_env name env in
    let_env_from_params tail new_env


(* construct a fun_env from function declarations *)
let rec fun_env_from_decls (ds: decl list) (fenv: fun_env) : fun_env =
  match ds with
  | [] -> fenv
  | d::tail -> 
    match d with 
    | FunDef (fname, params, _) -> fun_env_from_decls tail 
      (extend_fun_env fname (List.length params) fenv)
    | SysFunDef (_, _, _) -> fun_env_from_decls tail fenv

(* construct a sys_env from function declarations *)
let rec sys_env_from_decls (ds: decl list) (senv: sys_env) : sys_env =
  match ds with
  | [] -> senv
  | d::tail ->
    match d with
    | FunDef (_, _, _) -> sys_env_from_decls tail senv
    | SysFunDef (fname, params, return) -> sys_env_from_decls tail 
      (extend_sys_env fname params return senv)
