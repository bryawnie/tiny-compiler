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
  | StackOffset n -> Asm.RegOffset (RBP, -n)

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
(* An "fenv" contains function definitions.
                env_name * (label * arity) *)
(* type fun_env = (string * (string  * int)) list *)
(* sys_env contains foreign function definitions *)
type sys_env = (string * (dtype list * dtype)) list
(* rec_env contains record definition data *)
(* type rec_env = ((*id*)string * ((*tag*)int * (*fields*)string list)) list *)
(* 
An environment is composed of three separate namespaces:
  > let-bound variables
  > functions
  > foreign functions
*)
type env = let_env * (* fun_env * *) sys_env (* * rec_env *)

(* Alias for empty environment *)
let empty_env : let_env = []
(* let empty_fun_env : fun_env = [] *)
let empty_sys_env : sys_env = []

(* pretty printers *)
(* let pp_fun_env fmt lenv =
  (Fmt.list (Fmt.pair Fmt.string Fmt.int)) fmt lenv  *)

let pp_env fmt fenv =
  (Fmt.list (Fmt.pair Fmt.string pp_memloc)) fmt fenv

(* let pp_env fmt env =
  match env with lenv, fenv ->
    Fmt.pf fmt "variables=%a, functions=%a" pp_let_env lenv pp_fun_env fenv *)


(*------------------
| lookup functions |
------------------*)

(* looks up a let-bound variable in an environment. Returns its memory
location if present, fails with an "Unbound identifier" error if not.*)
let let_lookup (name: string) (env: env) : arg =
  match env with lenv , _->
    if List.mem_assoc name lenv then memloc_to_arg @@ List.assoc name lenv
    else Fmt.failwith "Unbound identifier: %s" name

(* looks up a system (foreign) function in an env. Returns its parameter
and return types; fails with and "Undefined system function" error if not.*)
let sys_lookup (name: string) (env: env) : (dtype list * dtype) =
  match env with _ , senv ->
    if List.mem_assoc name senv then List.assoc name senv 
    else Fmt.failwith "Undefined system function: %s" name

(* looks up a record identifier in an env. The record envornment associates
the identifier of a record type with an integer tag and a field name list.

If the identifier is not found, the tag -1 and and empty list is returned,
rather than raise an exception. *)
(*
let rec_lookup (id: string) (env: env) : (int * string list) =
  match env with _, _, _, renv -> 
    if List.mem_assoc id renv then List.assoc id renv
    else Fmt.failwith "Undefined record type: %s" id
*)

(*--------------
| Slot getters |
--------------*)

(*
 Returns the next available memory slot in an environment. Memory slots
for let-bound variables are stack positions, represented as an offset 
from RBP.

A let_env may contain function arguments. Arguments are bound either to
registers or a negative stack offset. Since they are always bound by the
caller they precede values bound by the callee in the env. For this reason
this function assumes there won't be registers or negative stack offsets
after the first let-bound value (positive stack offset.
*)
let get_let_slot (env: let_env) : memloc =
  match env with 
  | [] -> StackOffset 1 (*env is empty*)
  | (_, loc)::_ -> 
    begin
      match loc with
      | MReg _ -> StackOffset 1 (*there are function arguments in registers*)
      | StackOffset n -> 
        if n < 0 then StackOffset 1 (*negative offsets are arguments*)
        else StackOffset (n + 1) 
    end

(* Returns the memory location for a function argument. In the x64 calling
  convention the first 6 arguments go into registers, and the rest are 
  pushed into the stack. 
  
  Unlike stack offsets for let-bound identifiers, stack offsets for arguments
  are "below" RBP, since this is how a function accesses the arguments it
  received from the caller. 
  
  Arguments ALWAYS precede let-bound values in the environment.
  *)
let get_arg_slot (env: let_env) : memloc =
  let arg_regs = [MReg RDI; MReg RSI; MReg RDX; MReg RCX; MReg R8; MReg R9] in
  let n = List.length env in
  if n < 6 
    then List.nth arg_regs (n) 
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
  match env with lenv, senv ->
  let new_lenv, arg = extend_let_env name lenv in
  ((new_lenv, senv), arg)

(* Extends a let_env with function parameters *)
let extend_arg_env (name: string) (env: let_env) : (let_env * arg) =
  let loc = get_arg_slot env in
  ((name, loc)::env, memloc_to_arg loc)

(* Extends a fun_env with a new function *)
(* let extend_fun_env (fname: string) (flabel: string) (arity: int) (fenv: fun_env) : fun_env =
  if List.mem_assoc fname fenv then
    Fmt.failwith "Duplicate function name: %s" fname
  else (fname, (flabel, arity))::fenv *)

(* Extends a sys_env with a new function *)
let extend_sys_env (fname: string) (params: dtype list) (return_type: dtype)
  (senv: sys_env) : sys_env = 
  if List.mem_assoc fname senv then
    Fmt.failwith "Duplicate system function name: %s" fname
  else (fname, (params, return_type))::senv

(* Extends a rec_env with a new record type *)(* 
let extend_rec_env (id: string) (fields: string list) (renv: rec_env) :
  rec_env =
  if List.mem_assoc id renv then
    Fmt.failwith "Duplicate record definition: %s" id
  else (id, (List.length renv, fields))::renv *)

(* let rec multi_extend_fun_env (defs: (string * string * int) list) (fenv: fun_env) : fun_env =
  match defs with
  | [] -> fenv
  | (name, label, arity)::tail -> 
    multi_extend_fun_env tail (extend_fun_env name label arity fenv) *)


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
(* let rec fun_env_from_decls (ds: decl list) (fenv: fun_env) : fun_env =
  match ds with
  | [] -> fenv
  | d::tail -> 
    match d with 
    | FunDef (fname, params, _) -> fun_env_from_decls tail 
      (extend_fun_env fname (Gensym.get_fun_label fname) (List.length params) fenv)
    | RecDef (id, fields) -> 
      let cons = id, Gensym.get_fun_label id, List.length fields in
      let get =
         List.map (fun fld -> let fname = String.concat "-" [id;fld] in
          fname, Gensym.get_fun_label fname, 1) fields 
        in
      let type_checker = 
        let fname = String.concat "" [id; "?"] in 
        fname, Gensym.get_fun_label fname, 1 
      in
      fun_env_from_decls tail (
        multi_extend_fun_env ([cons;type_checker]@get) fenv) 
    | _-> fun_env_from_decls tail fenv *)

(* construct a sys_env from function declarations *)
let rec sys_env_from_decls (ds: decl list) (senv: sys_env) : sys_env =
  match ds with
  | [] -> senv
  | d::tail ->
    match d with
    | SysFunDef (fname, params, return) -> sys_env_from_decls tail 
      (extend_sys_env fname params return senv)
    | _ -> sys_env_from_decls tail senv
