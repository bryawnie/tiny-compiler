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

(* Should probably add a generalized lifter *)
(*let liftValue (op: 'a -> 'b) : value -> value = ...*)


open Expr

(** Environment **)
type env = (string * value) list

let empty_env : env = []
  
let extend_env : string -> value -> env -> env =
    fun x v env -> (x, v) :: env

(** Interpreter *)

let rec interp ?(env=empty_env) (e : expr)  : value =
  match e with 
  | Num n -> NumV n
  | Bool p -> BoolV p
  | Id x -> List.assoc x env
  | Add1 e -> liftNumV (Int64.add 1L) (interp ~env:env e) 
  | Sub1 e -> liftNumV (Int64.add (-1L)) (interp ~env:env e)
  | Not e -> liftBoolV (not) (interp ~env:env e)
  | Or (p, q) -> BoolV false (* FIXME *)
  | And (p, q) -> BoolV false (* FIXME *)
  | Let (id, v, b) -> interp ~env:(extend_env id (interp ~env:env v) env) b
