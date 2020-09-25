(* 
  <expr> ::= number
        | id 
        | (add1 <expr>)
        | (sub1 <expr>)
        | (let (<id> <expr>) <expr>)
*)

type expr = 
  | Num of int64
  | Bool of bool
  | Id of string
  | Add1 of expr
  | Sub1 of expr
  | Let of string * expr * expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr

open Fmt

let rec pp_expr fmt = function
  | Num n -> int64 fmt n
  | Bool b -> bool fmt b
  | Id x -> string fmt x
  | Add1 e -> pf fmt "(add1 %a)" pp_expr e
  | Sub1 e -> pf fmt "(sub1 %a)" pp_expr e
  | Let (x,v,b) -> pf fmt "(let (%a %a) %a)" string x pp_expr v pp_expr b
  | Not p -> pf fmt "(not %a)" pp_expr p
  | And (p, q) -> pf fmt "(and %a %a)" pp_expr p pp_expr q
  | Or (p, q) -> pf fmt "(or %a %a)" pp_expr p pp_expr q
