open Asm

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

(* 
  Environment as lists of pairs (i.e. association lists) 
  First field  -> Symbol
  Second field -> Memory location
*)
type let_env = (string * memloc) list
(* An "fenv" contains function definitions*)
type fun_env = (string * int) list
type env = let_env * fun_env

let pp_fun_env fmt lenv =
  (Fmt.list (Fmt.pair Fmt.string Fmt.int)) fmt lenv 

let pp_let_env fmt fenv =
  (Fmt.list (Fmt.pair Fmt.string pp_memloc)) fmt fenv

let pp_env fmt env =
  match env with lenv, fenv ->
    Fmt.pf fmt "variables=%a, functions=%a" pp_let_env lenv pp_fun_env fenv

(* Alias for empty environment *)
let empty_env : let_env = []
let empty_fun_env : fun_env = []

(* This function makes the lookup for an id in environment
  returnint its offset in RSP. *)
let rec let_lookup (name: string) (env: let_env) : arg =
  match env with
  | [] -> failwith (Format.sprintf "Unbound identifier: %s" name)
  | (n, loc)::rest -> if n = name then memloc_to_arg loc else (let_lookup name rest)

let lookup (name: string) (env: env) : arg =
  match env with lenv, _ -> let_lookup name lenv

(* Returns the next available memory slot in an environment. Memory slots for
  let-bound variables are stack positions, represented as an offset from RBP.  *)
let get_next_slot (env: let_env) : memloc =
  match env with 
  | [] -> StackOffset 1
  | (_, loc)::_ -> 
    begin
      match loc with
      | MReg _ -> StackOffset 1
      | StackOffset n -> StackOffset (n + 1)
    end

(* Extends the environment with a new id and its respective offset in stack *)
let extend_let_env (name: string) (env: let_env) : (let_env * arg) =
  let loc = get_next_slot env in
  ((name, loc)::env, memloc_to_arg loc)

let extend_env (name: string) (env: env) : (env * arg) =
  match env with lenv, fenv ->
  let new_lenv, arg = extend_let_env name lenv in
  ((new_lenv, fenv), arg)

(* Returns the memory location for a function arguments. In the x64 calling
  convention the first 6 arguments go into registers, and the rest are pushed
  into the stack. 
  
  Unlike stack offsets for let-bound identifiers, stack offsets for arguments
  are "below" RBP, since this is how a function accesses the arguments it
  received from the caller. *)
let get_next_arg_slot (env: let_env) : memloc =
  let arg_regs = [MReg RDI; MReg RSI; MReg RDX; MReg RCX; MReg R8; MReg R9] in
  let n = List.length env in
  if n < 6 
    then List.nth arg_regs (n + 1) 
    else StackOffset (4 - n) (*  arg_n = RBP + 8 * ((n - 6) + 2)  *)

let extend_arg_env (name: string) (env: let_env) : (let_env * arg) =
  let loc = get_next_arg_slot env in
  ((name, loc)::env, memloc_to_arg loc)

(* Returns an environment to compile a function with k parameters *)
let rec make_function_let_env (params: string list) (env: let_env) : let_env =
  match params with
  | [] -> env
  | name::tail -> 
    let new_env, _ = extend_arg_env name env in
    make_function_let_env tail new_env

let extend_fun_env (fname: string) (arity: int) (fenv: fun_env) : fun_env =
  (fname, arity)::fenv

(* Finds the arity of a function, if defined *)
let rec fun_lookup (name: string) (fenv: fun_env) : int =
  match fenv with
  | [] -> Fmt.failwith "Undefined function: %s" name
  | (n, a)::tail -> if n = name then a else (fun_lookup name tail)

let fun_lookup (name: string) (env: env) : int =
  match env with _, fenv -> fun_lookup name fenv

(* construct a fun_env from function declarations *)
open Ast
let rec fun_env_from_decls (ds: decl list) (fenv: fun_env) : fun_env =
  match ds with
  | [] -> fenv
  | d::tail -> 
    match d with FunDef (fname, params, _) ->
      fun_env_from_decls tail (extend_fun_env fname (List.length params) fenv)
