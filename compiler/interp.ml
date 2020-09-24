(** Values *)

type value = NumV of int64

(* Lifting functions on int to values *)
(* let liftNumV ( op : int64 -> int64) : value -> value =
  function | NumV n -> NumV (op n) *)


(* Lifting functions on int to values *)
let liftNumV : (int64 -> int64 -> int64) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    (* | _ -> Fmt.failwith "Error: Binop applied to non numeric value"  *)

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
  | Add1 e -> liftNumV (Int64.add) (NumV 1L) (interp ~env:env e) 
  | Sub1 e -> liftNumV (Int64.add) (NumV (-1L)) (interp ~env:env e) 
  | Let (id, v, b) -> interp ~env:(extend_env id (interp ~env:env v) env) b
  | BinOp (op,l,r) -> 
      begin match op with 
      | Add -> liftNumV (Int64.add) (interp l ~env:env) (interp r ~env:env)
      | Sub -> liftNumV (Int64.sub) (interp l ~env:env) (interp r ~env:env)
      | Mul -> liftNumV (Int64.mul) (interp l ~env:env) (interp r ~env:env)
      end


(** Pretty printing **)

(* printing values and expressions *)
let pp_value : value Fmt.t =
  fun fmt e -> 
    match e with 
    | NumV n -> Fmt.int64 fmt n