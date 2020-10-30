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

(** Interpreter Environment **)
type ienv = (string * value) list

let mt_ienv : ienv = []
  
let extend_ienv : string -> value -> ienv -> ienv =
    fun x v ienv -> (x, v) :: ienv

(** Interpreter **)
let rec interp ?(env=mt_ienv) (e : expr)  : value =
  match e with 
  | Num n -> NumV n
  | Bool p -> BoolV p
  | Id x -> List.assoc x env
  | App (_, _) -> NumV 5L (* FIX *)
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
  | Let (id, v, b) -> interp ~env:(extend_ienv id (interp ~env:env v) env) b
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
  | If (c, t, f)  -> liftIf (interp ~env:env c) (interp ~env:env t) (interp ~env:env f)
  | Void -> NumV 0L (* This is supposed to do nothing *)

let interp_prog p : value = 
  let (Program (_,exp)) = p in interp exp
