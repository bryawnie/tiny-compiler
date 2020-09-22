(** Values *)

type value = NumV of int64

(* Lifting functions on int to values *)
let liftNumV ( op : int64 -> int64) : value -> value =
  function | NumV n -> NumV (op n)


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
  | Id x -> List.assoc x env
  | Add1 e -> liftNumV (Int64.add 1L) (interp ~env:env e) 
  | Sub1 e -> liftNumV (Int64.add (-1L)) (interp ~env:env e)
  | Let (id, v, b) -> interp ~env:(extend_env id (interp ~env:env v) env) b


(** Pretty printing **)

(* printing values and expressions *)
let pp_value : value Fmt.t =
  fun fmt e -> 
    match e with 
    | NumV n -> Fmt.int64 fmt n