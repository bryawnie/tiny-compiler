(* Unary operators *)
type unOp =
  | Add1
  | Sub1
  | Not

(* Binary operators *)
type binOp =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Less 
  | Eq

(* 
  <expr> ::= 
  | number
  | bool
  | id 
  | (add1 <expr>)
  | (sub1 <expr>)
  | (not <expr>)
  | (let (<id> <expr>) <expr>)
  | ( + <expr> <expr>)
  | ( - <expr> <expr>)
  | ( * <expr> <expr>)
  | ( / <expr> <expr>)
  | (and <expr> <expr>)
  | (or  <expr> <expr>)
  | (if  <expr> <expr> <expr> )
*)

type expr = 
  | Num of int64
  | Bool of bool
  | Id of string
  | UnOp of unOp * expr
  | BinOp of binOp * expr * expr
  | LazyBinOp of binOp * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr

open Fmt

(* Pretty printer for operators *)
let pp_unop fmt = function op ->
  let str = match op with
  | Add1 -> "add1"
  | Sub1 -> "sub1"
  | Not -> "not"
  in string fmt str

let pp_binop fmt = function op ->
  let str = match op with
  | Add  -> "+"
  | Sub  -> "-"
  | Mul  -> "*"
  | Div  -> "/"
  | And  -> "and"
  | Or   -> "or"
  | Less -> "<"
  | Eq   -> "="
  in string fmt str

(* Pretty printer for expresions *)
let rec pp_expr fmt = function
  | Num n -> int64 fmt n
  | Bool b -> bool fmt b
  | Id x -> string fmt x
  | UnOp (op, e) -> pf fmt "(%a %a)" pp_unop op pp_expr e
  | BinOp (op, x1, x2) -> pf fmt "(%a %a %a)" pp_binop op pp_expr x1 pp_expr x2
  | LazyBinOp (op, x1, x2) -> pf fmt "(%a %a %a)" pp_binop op pp_expr x1 pp_expr x2
  | Let (x,v,b) -> pf fmt "(let (%a %a) %a)" string x pp_expr v pp_expr b
  | If (c, t, f) -> pf fmt "(if %a %a %a)" pp_expr c pp_expr t pp_expr f
