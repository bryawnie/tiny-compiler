(** Values *)

type value =
| NumV of int64
| BoolV of bool

(** Pretty printing **)

(* printing values and expressions *)
let pp_value : value Fmt.t =
  fun fmt e -> 
    match e with 
    | NumV n -> Fmt.int64 fmt n
    | BoolV p -> Fmt.bool fmt p
(* 
(* Lifting functions on int to values *)
let liftNumV ( op : int64 -> int64) : value -> value =
  function
  | NumV n -> NumV (op n)
  | x -> Fmt.failwith "(liftNumV) type error: expected int64, got: %a" pp_value x

(* Lift boolean functions to values*)
let liftBoolV (op: bool -> bool) : value -> value =
  function
  | BoolV p -> BoolV (op p)
  | q -> Fmt.failwith "(liftBoolV) type error: expected bool, got: %a" pp_value q
 *)
(* Lifting functions on int to values *)
let liftNumV : (int64 -> int64 -> int64) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    | _ -> Fmt.failwith "Error: Numeric binop applied to non numeric values"

let liftBoolV : (bool -> bool -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | BoolV p, BoolV q -> BoolV (op p q)
    | _ -> Fmt.failwith "TypeError: expected boolean values but got %a and %a"
            pp_value e1 pp_value e2

open Expr

(** Environment **)
type env = (string * value) list

let empty_env : env = []
  
let extend_env : string -> value -> env -> env =
    fun x v env -> (x, v) :: env

(** Interpreter *)

(* THIS IS IN DIRE NEED OF REFACTORING *)
let rec interp ?(env=empty_env) (e : expr)  : value =
  match e with 
  | Num n -> NumV n
  | Bool p -> BoolV p
  | Id x -> List.assoc x env
  | Add1 e -> liftNumV (Int64.add) (NumV 1L) (interp ~env:env e) 
  | Sub1 e -> liftNumV (Int64.sub) (NumV 1L) (interp ~env:env e)
  | Not e -> (match (interp ~env:env e) with BoolV p -> BoolV (not p)
              | _ -> Fmt.failwith "TypeError: not a boolean value") 
  | Or (p, q) -> liftBoolV (||) (interp ~env:env p) (interp ~env:env q)
  | And (p, q) -> liftBoolV (&&) (interp ~env:env p) (interp ~env:env q)
  | Let (id, v, b) -> interp ~env:(extend_env id (interp ~env:env v) env) b
  | BinOp (op,l,r) -> 
      begin match op with 
      | Add -> liftNumV (Int64.add) (interp l ~env:env) (interp r ~env:env)
      | Sub -> liftNumV (Int64.sub) (interp l ~env:env) (interp r ~env:env)
      | Mul -> liftNumV (Int64.mul) (interp l ~env:env) (interp r ~env:env)
      | Div -> liftNumV (Int64.div) (interp l ~env:env) (interp r ~env:env)
      end
