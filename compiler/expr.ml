type singOp =
  | Add1
  | Sub1

type binOp =
  | Add
  | Sub
  | Mul
  | Div

(* 
  <expr> ::= number
        | id 
        | (add1 <expr>)
        | (sub1 <expr>)
        | (let (<id> <expr>) <expr>)
        | (+ <expr> <expr>)
*)

type expr = 
  | Num of int64
  | Bool of bool
  | Id of string
  | SingOp of singOp * expr
  | Let of string * expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | BinOp of binOp * expr * expr
  | If of expr * expr * expr
open Fmt

let rec pp_expr fmt = function
  | Num n -> int64 fmt n
  | Bool b -> bool fmt b
  | Id x -> string fmt x
  | SingOp (op, e) ->
      begin match op with
      | Add1 -> pf fmt "(add1 %a)" pp_expr e
      | Sub1 -> pf fmt "(sub1 %a)" pp_expr e
      end
  | Let (x,v,b) -> pf fmt "(let (%a %a) %a)" string x pp_expr v pp_expr b
  | Not p -> pf fmt "(not %a)" pp_expr p
  | And (p, q) -> pf fmt "(and %a %a)" pp_expr p pp_expr q
  | Or (p, q) -> pf fmt "(or %a %a)" pp_expr p pp_expr q
  | BinOp (op, x1, x2) -> 
      begin match op with
      | Add -> pf fmt "(+ %a %a)"  pp_expr x1 pp_expr x2
      | Sub -> pf fmt "(- %a %a)"  pp_expr x1 pp_expr x2
      | Mul -> pf fmt "(* %a %a)"  pp_expr x1 pp_expr x2
      | Div -> pf fmt "(/ %a %a)"  pp_expr x1 pp_expr x2
      end
  | If (c, t, f) -> pf fmt "(if %a %a %a)" pp_expr c pp_expr t pp_expr f