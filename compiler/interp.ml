(** Values *)

type value =
| NumV of int64
| BoolV of bool

(** Pretty printing **)

(* printing values *)
let pp_value : value Fmt.t =
  fun fmt e -> 
    match e with 
    | NumV n -> Fmt.int64 fmt n
    | BoolV p -> Fmt.bool fmt p

(** Value lifters **)

(* Lifting functions on int to values *)
let liftNumV : (int64 -> int64 -> int64) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    | _ -> Fmt.failwith "Error: Numeric binop applied to non numeric values"

(* Lifting functions on bool to values *)
let liftBoolV : (bool -> bool -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | BoolV p, BoolV q -> BoolV (op p q)
    | _ -> Fmt.failwith "TypeError: expected boolean values but got %a and %a"
            pp_value e1 pp_value e2

(* Extracts the boolean value of a BoolV *)
let unpackBoolV : value -> bool =
  function
  | BoolV p -> p
  | _ -> Fmt.failwith "TypeError: not a boolean"

(* Handles if expressions *)
let liftIf : value -> value -> value -> value =
  fun c tb fb ->
    match c with
    | BoolV cond -> if cond then tb else fb
    | _ -> Fmt.failwith "Error: Non boolean condition in If sentence"

(* Handles the comparations of inequality between integers *)
let liftCompV : (int64 -> int64 -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> BoolV (op n1 n2)
    | _ -> Fmt.failwith "Error: Numeric comparator applied to non numeric values"

(* Handles the comparations of equality between booleans or integers *)
let liftEqV : value -> value -> value =
  fun e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> BoolV (n1 = n2)
    | BoolV n1, BoolV n2 -> BoolV (n1 = n2)
    | _ -> BoolV false

open Ast

type lenv = (string * value) list
type fenv = (string * (string list * expr)) list

(** Interpreter Environment **)
type ienv = lenv * fenv

let mt_ienv : ienv = [], []
let mt_lenv : lenv = []
let mt_fenv : fenv = []

let extend_lenv (id: string) (v: value) (env: ienv): ienv =
  let lenv, fenv = env in (id, v)::lenv, fenv

let extend_fenv (id: string) (params: string list) (body: expr) (env: ienv) : ienv =
  let lenv, fenv = env in lenv, (id, (params, body))::fenv


let rec multi_extend_lenv (vars: string list) (vals: value list) (env: ienv) 
: ienv =
  match vars, vals with
  | [], [] -> env
  | var::tail_vars, value::tail_vals -> 
    extend_lenv var value (multi_extend_lenv tail_vars tail_vals env)
  | _, _ -> Fmt.failwith "Error: Arity mismatch"
  

let rec lookup_decl (name: string) (decls: decl list): string * string list * expr =
  match decls with 
  | [] -> Fmt.failwith "Error: Function %s not found" name
  | dec::tail_decls -> 
    match dec with
    | FunDef (fname, args, body) -> if fname = name then (fname, args, body) else lookup_decl name tail_decls
    | _ -> Fmt.failwith "Error: System functions not supported in Interpreter."

let let_lookup (id: string) (env: ienv) : value =
  let lenv, _ = env in
  if List.mem_assoc id lenv
    then List.assoc id lenv
  else
    Fmt.failwith "Unbound identifier: %s" id

let fun_lookup (id: string) (env: ienv) : string list * expr =
  let _, fenv = env in
  if List.mem_assoc id fenv
    then List.assoc id fenv 
  else
    Fmt.failwith "Undefined function name: %s" id


(** Interpreter **)
let rec interp ?(env=mt_ienv) (e : expr) : value =
  match e with 
  | Num n -> NumV n
  | Bool p -> BoolV p
  | Id x -> let_lookup x env
  | App (_, _) -> Fmt.failwith "Error: Not implemented"
  | UnOp (op, e) ->
      begin match op with
      | Add1 -> liftNumV (Int64.add) (interp ~env:env e) (NumV 1L)
      | Sub1 -> liftNumV (Int64.sub) (interp ~env:env e) (NumV 1L)
      | Not ->
        begin match (interp ~env:env e) with 
        | BoolV p -> BoolV (not p)
        | _ -> Fmt.failwith "Error: Non boolean expr in Not sentence"
        end
      end
  | Let (defs, b) -> 
    let ids = List.map (fun (id,_) -> id) defs in
    let vals = List.map (fun (_,v) -> (interp ~env:env v)) defs in
    interp ~env:(multi_extend_lenv ids vals env) b
  | BinOp (op,l,r) -> 
      begin match op with 
      | Add -> liftNumV (Int64.add) (interp l ~env:env) (interp r ~env:env)
      | Sub -> liftNumV (Int64.sub) (interp l ~env:env) (interp r ~env:env)
      | Mul -> liftNumV (Int64.mul) (interp l ~env:env) (interp r ~env:env)
      | Div -> liftNumV (Int64.div) (interp l ~env:env) (interp r ~env:env)
      | Less -> liftCompV (<) (interp ~env:env l) (interp ~env:env r)
      | Eq -> liftEqV (interp ~env:env l) (interp ~env:env r)
      | Or -> BoolV (unpackBoolV (interp ~env:env l) || unpackBoolV (interp ~env:env r))
      | And -> BoolV (unpackBoolV (interp ~env:env l) && unpackBoolV (interp ~env:env r))
      end
  | If (c, t, f) ->
    begin
    match interp ~env:env c with
    | BoolV cond -> if cond then interp ~env:env t else interp ~env:env f
    | _ -> Fmt.failwith "Error: Non boolean condition in If sentence"
    end
  | Void -> NumV 0L (* This is supposed to do nothing *)
  | Sys (_,_) -> Fmt.failwith "Error: System functions not supported in Interpreter."
  | Tuple _ -> Fmt.failwith "Error: Not implemented"
  | Get (_,_) -> Fmt.failwith "Error: Not implemented"
  | Set (_,_,_) -> Fmt.failwith "Error: Not implemented"
  | Length (_) -> Fmt.failwith "Error: Not implemented"

let rec fenv_from_decls (ds: decl list) (fenv: fenv): fenv =
  match ds with
  | [] -> fenv
  | FunDef(fname, params, body)::tail ->
    if List.mem_assoc fname fenv
      then Fmt.failwith "Duplicate function name: %s" fname
    else
      fenv_from_decls tail @@ (fname, (params, body))::fenv
  | SysFunDef (_, _, _):: _ ->
    Fmt.failwith "Interpreter does not support foreign functions (defsys)."
  | RecDef(_,_) :: _ -> 
    Fmt.failwith "Interpreter does not support records."


let interp_prog p : value = 
  let (Program (decls, exp)) = p in
  let fenv = fenv_from_decls decls mt_fenv in
   interp exp ~env: (mt_lenv, fenv)
