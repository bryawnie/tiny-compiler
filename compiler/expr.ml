
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
  | Add1 of expr
  | Sub1 of expr
  | Let of string * expr * expr
  | BinOp of binOp * expr * expr

open Fmt

let rec pp_expr fmt = function
  | Num n -> int64 fmt n
  | Bool b -> bool fmt b
  | Id x -> string fmt x
  | Add1 e -> pf fmt "(add1 %a)" pp_expr e
  | Sub1 e -> pf fmt "(sub1 %a)" pp_expr e
  | Let (x,v,b) -> pf fmt "(let (%a %a) %a)" string x pp_expr v pp_expr b
  | BinOp (op, x1, x2) -> 
      begin match op with
      | Add -> pf fmt "(+ %a %a)"  pp_expr x1 pp_expr x2
      | Sub -> pf fmt "(- %a %a)"  pp_expr x1 pp_expr x2
      | Mul -> pf fmt "(* %a %a)"  pp_expr x1 pp_expr x2
      | Div -> pf fmt "(/ %a %a)"  pp_expr x1 pp_expr x2
      end