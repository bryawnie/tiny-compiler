(** Compiler Environment **)

(* 
  Environment as lists of pairs (i.e. association lists) 
  First field  -> Symbol
  Second field -> Offset in Stack
*)
type env = (string * int) list


(* Alias for empty environment *)
let empty_env : env = []


(* This function makes the lookup for an id in environment
  returnint its offset in RSP. *)
let rec lookup (name: string) (env: env) : int =
  match env with
  | [] -> failwith (Format.sprintf "Identifier %s not found in environment" name)
  | (n, i)::rest -> if n = name then i else (lookup name rest)


(* Extends the environment with a new id and its respective offset in stack *)
let extend_env (name: string) (env: env) : (env * int) =
  let slot = 1 + (List.length env) in
  ((name, slot)::env, slot) 