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
  | Let of string * expr * expr
  | If of expr * expr * expr
  (* | FunApp of string * expr list (* function application *) *)
  | App of string * expr list (* function application *)
  | Sys of string * expr list (* foreign function application *)
  | Void

(* data type *)
type dtype =
  | IntT
  | BoolT
  | AnyT

type decl =
  | FunDef of string * string list * expr (* function definition *)
  | SysFunDef of string * dtype list * dtype(* foreign function definition *)

type prog =
  | Program of decl list * expr

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

let rec pp_expr_list  (pp_exp: expr Fmt.t): (expr list) Fmt.t =
  fun fmt l ->
    match l with
    | [] -> pf fmt ""
    | e::rest -> pf fmt "%a %a" pp_exp e (pp_expr_list pp_exp) rest

(* Pretty printer for expresions *)
let rec pp_expr fmt = 
  function
  | Num n -> int64 fmt n
  | Bool b -> bool fmt b
  | Id x -> string fmt x
  | UnOp (op, e) -> pf fmt "(%a %a)" pp_unop op pp_expr e
  | BinOp (op, x1, x2) -> pf fmt "(%a %a %a)" pp_binop op pp_expr x1 pp_expr x2
  | Let (x,v,b) -> pf fmt "(let (%a %a) %a)" string x pp_expr v pp_expr b
  | If (c, t, f) -> pf fmt "(if %a %a %a)" pp_expr c pp_expr t pp_expr f
  | App (fname, exprs) -> pf fmt "(%a %a)" string fname (pp_expr_list pp_expr) exprs
  | Sys (fname, exprs) -> pf fmt "(@sys %s %a)" fname (pp_expr_list pp_expr) exprs
  | Void -> pf fmt "<void>"
  (*
  | FunDef (fname, params, body) ->
    pf fmt "(def (%a %a) %a)" string fname (pp_expr_list pp_expr) params pp_expr body
  | FunApp (fname, args) -> 
    pf fmt "(%a %a)" string fname (pp_expr_list pp_expr) args
  *)

let pp_dtype fmt =
  function
  | IntT -> pf fmt "int"
  | BoolT -> pf fmt "bool"
  | AnyT -> pf fmt  "any"

let pp_decl fmt =
  function
  | FunDef (name, params, body) ->
    pf fmt "(def (%s %s) %a)" name (String.concat " " params) pp_expr body
  | SysFunDef (name, params, ret) ->
    pf fmt "(defsys %s %a -> %a)" name (list ~sep:sp pp_dtype) params pp_dtype ret

let rec pp_decl_list fmt =
  function
  | [] -> pf fmt ""
  | d::tail -> pf fmt "%a\n%a" pp_decl d pp_decl_list tail

let pp_prog fmt =
  function
  | Program (decls, expr) -> pf fmt "%a%a" pp_decl_list decls pp_expr expr
