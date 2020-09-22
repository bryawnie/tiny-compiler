(* registers *)
type reg = 
| RAX
| RSP

(** Environment **)
type env = (string * int) list

let empty_env : env = []

let rec lookup (name: string) (env: env) : int =
  match env with
  | [] -> failwith (Format.sprintf "Identifier %s not found in environment" name)
  | (n, i)::rest -> if n = name then i else (lookup name rest)

let extend_env (name: string) (env: env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot) 


(* arguments for instructions *)
type arg =
| Const of int64
| Reg of reg
| RegOffset of reg * int

(* asm instructions *)
type instruction =
| IMov of arg * arg
| IAdd of arg * arg
| IRet

let pp_reg : reg Fmt.t =
  fun fmt r ->
    match r with
    | RAX -> Fmt.string fmt "RAX"
    | RSP -> Fmt.string fmt "RSP"

let pp_arg : arg Fmt.t =
  fun fmt arg ->
    match arg with
    | Const n         -> Fmt.pf fmt "%#Lx" n
    | Reg r           -> pp_reg fmt r
    | RegOffset (r,i) -> Fmt.pf fmt "[%a - %a]" pp_reg r Fmt.int (8*i)

let pp_instr : instruction Fmt.t =
  fun fmt instr ->
  match instr with
  | IMov (a1, a2) -> Fmt.pf fmt "  mov %a, %a" pp_arg a1 pp_arg a2
  | IAdd (a1, a2) -> Fmt.pf fmt "  add %a, %a" pp_arg a1 pp_arg a2
  | IRet -> Fmt.pf fmt "  ret" 

let pp_instrs : (instruction list) Fmt.t =
  Fmt.list ~sep:Format.pp_force_newline pp_instr
