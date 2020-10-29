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

(* 
  Environment as lists of pairs (i.e. association lists) 
  First field  -> Symbol
  Second field -> Memory location
*)
type env = (string * memloc) list

(* Alias for empty environment *)
let empty_env : env = []

(* This function makes the lookup for an id in environment
  returnint its offset in RSP. *)
let rec lookup (name: string) (env: env) : arg =
  match env with
  | [] -> failwith (Format.sprintf "Unbound identifier: %s" name)
  | (n, loc)::rest -> if n = name then memloc_to_arg loc else (lookup name rest)

(* Returns the next available memory slot in an environment. Memory slots for
  let-bound variables are stack positions, represented as an offset from RBP.  *)
let get_next_slot (env: env) : memloc =
  match env with 
  | [] -> StackOffset 1
  | (_, loc)::_ -> 
    begin
      match loc with
      | MReg _ -> StackOffset 1
      | StackOffset n -> StackOffset (n + 1)
    end

(* Extends the environment with a new id and its respective offset in stack *)
let extend_env (name: string) (env: env) : (env * arg) =
  let loc = get_next_slot env in
  ((name, loc)::env, memloc_to_arg loc)

(* Returns the memory location for a function arguments. In the x64 calling
  convention the first 6 arguments go into registers, and the rest are pushed
  into the stack. 
  
  Unlike stack offsets for let-bound identifiers, stack offsets for arguments
  are "below" RBP, since this is how a function accesses the arguments it
  received from the caller. *)
let get_next_arg_slot (env: env) : memloc =
  let arg_regs = [MReg RDI; MReg RSI; MReg RDX; MReg RCX; MReg R8; MReg R9] in
  let n = List.length env in
  if n < 6 
    then List.nth arg_regs (n + 1) 
    else StackOffset (4 - n) (*  arg_n = RBP + 8 * ((n - 6) + 2)  *)

let extend_arg_env (name: string) (env: env) : (env * arg) =
  let loc = get_next_arg_slot env in
  ((name, loc)::env, memloc_to_arg loc)

(* Returns an environment to compile a function with k parameters *)
let rec make_function_env (params: string list) (env: env) : env =
  match params with
  | [] -> env
  | name::tail -> 
    let new_env, _ = extend_arg_env name env in
    make_function_env tail new_env
