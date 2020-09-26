(** Types *)

(* Base types: [string], [int] (31 or 63 bits), pairs ['a * 'b], lists ['a list] *)

(* Type alias *)
type id = string

(* Algebraic datatypes *)
type exp =
  | Var : id -> exp
  (* Integer *)
  | Num : int -> exp
  | Plus : exp * exp -> exp
  | Times : exp * exp -> exp
  | Minus : exp * exp -> exp
  | Divide : exp * exp -> exp
  (* Boolean *)
  | Bool : bool -> exp
  | Not : exp -> exp
  | Or : exp * exp -> exp
  | And : exp * exp -> exp
  | If : exp * exp * exp -> exp
  | Equals : exp * exp -> exp
  | Less : exp * exp -> exp
  (* Functions *)
  | Function : id * exp -> exp
  | Apply : exp * exp -> exp


(** Values *)

type value =
| NumV of int
| BoolV of bool
| ClosureV of id * exp * (id * value) list

(** Pretty printing **)

(* simple aliases to avoid confusion and use consistent naming *)
let pp_str = Fmt.string 
let pp_int = Fmt.int
let pp_bool = Fmt.bool

(* printing values and expressions *)
let pp_value : value Fmt.t =
  fun fmt e -> 
    match e with 
    | NumV n -> pp_int fmt n
    | BoolV b -> pp_bool fmt b
    | ClosureV (id, _, _) -> Fmt.pf fmt "<closure:var=%a>" pp_str id 

let rec pp_exp : exp Fmt.t =
  fun fmt e ->
    let pp_binop =
      fun op e1 e2 ->
        Fmt.pf fmt "(%a %a %a)" pp_str op pp_exp e1 pp_exp e2 in
    match e with
    | Var s -> pp_str fmt s
    | Num n -> pp_int fmt n
    | Bool b -> pp_bool fmt b
    | Not p -> Fmt.pf fmt "(not %a)" pp_exp p
    | Plus (e1, e2) -> pp_binop "+" e1 e2
    | Minus (e1, e2) -> pp_binop "-" e1 e2
    | Times (e1, e2) -> pp_binop "*" e1 e2
    | Divide (e1, e2) -> pp_binop "/" e1 e2
    | Or (e1, e2) -> pp_binop "or" e1 e2
    | And (e1, e2) -> pp_binop "and" e1 e2
    | Less (e1, e2) -> pp_binop "<" e1 e2
    | Equals (e1, e2) -> pp_binop "=" e1 e2
    | If (cond, then_e, else_e) -> 
      Fmt.pf fmt "(if %a %a %a)" pp_exp cond pp_exp then_e pp_exp else_e
    | Function (id, exp) -> Fmt.pf fmt "(fun (%a) %a)" pp_str id pp_exp exp
    | Apply (fexp, aexp) -> Fmt.pf fmt "(%a %a)" pp_exp fexp pp_exp aexp


(* Lifting functions on int to values *)
(* let liftNumV : (int -> int -> int) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)  *)

let value_to_int : value -> int =
  fun v -> match v with
  | NumV n -> n
  | _ -> Fmt.failwith "Error: not a number: %a" pp_value v

let value_to_bool : value -> bool =
  fun v -> match v with
  | BoolV b -> b
  | _ -> Fmt.failwith "Error: not a boolean: %a" pp_value v

(** Environments **)

(* Type constructors
   ['a list] is the type of expressions returning a list of some type 'a
  Here environment as lists of pairs (i.e. association lists)
*)
type env = (id * value) list

let empty_env : env = []

let extend_env : id -> value -> env -> env =
  fun x v env -> (x, v) :: env


(** Interpreter *)

let rec interp : env -> exp -> value =
  fun env e ->
  match e with
  | Var x ->
    (* Functions on list can be found in the [List] module and accessed 
    with the [List.function] syntax *)
    List.assoc x env
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Not e ->
    begin
      match interp env e with 
      | BoolV p -> BoolV (not p)
      | v -> Fmt.failwith "type error: expected boolean, got %a" pp_value v
    end
  | Plus (e1, e2) -> NumV (value_to_int (interp env e1)
                            + value_to_int (interp env e2))
  | Minus (e1, e2) -> NumV (value_to_int (interp env e1)
                            - value_to_int(interp env e2))
  | Times (e1, e2) -> NumV (value_to_int (interp env e1)
                            * value_to_int (interp env e2))
  | Divide (e1, e2) -> NumV (value_to_int (interp env e1)
                            / value_to_int (interp env e2))
  | Or (e1, e2) -> BoolV ( value_to_bool (interp env e1)
                           || value_to_bool (interp env e2))
  | And (e1, e2) -> BoolV (value_to_bool (interp env e1)
                           && value_to_bool (interp env e2))
  | Less (e1, e2) -> BoolV (value_to_int (interp env e1)
                            < value_to_int (interp env e2))
  | Equals (e1, e2) -> BoolV (value_to_int (interp env e1)
                            = value_to_int (interp env e2))
  | If (cond, then_exp, else_exp) ->
    begin
      match interp env cond with 
      | BoolV true -> interp env then_exp
      | BoolV false -> interp env else_exp
      | value ->
        Fmt.failwith "expected a boolean but got %a" pp_value value
    end
  | Function (id, body) -> ClosureV (id, body, env)
  | Apply (f, arg) -> 
    begin
      match interp env f with
      | ClosureV (id, body, fenv) ->
        interp (extend_env id (interp env arg) fenv) body
      | _ -> Fmt.failwith "interp error: not a function: %a" pp_exp f
    end


(** Parsing **)

(* we use CCsexp as a library for s-expressions *)
open CCSexp

(*
  Instead of using a standard algebraic data types for sexps, such as:
  
      type sexp = Atom of string | List of sexp list 
 
  this library uses a feature known as "polymorphic variants".
  This is a flexible mechanism where data is built by tagging it with symbols,
  instead of using pre-declared constructors. These symbols are written with ticks `.
  
  Then sexp is just an alias for a (recursive) polymorphic variant:

    type sexp = [ `Atom of id | `List of 'a list ] as 'a

  When matching such an sexp, we look at the tag. See the parse function below for an example.
  You can also look at the definition and implementation of the CCsexp module for more details.
*)

let rec parse (sexp : sexp) : exp = (* it's not a problem to have a variable [sexp] of type [sexp] (separate namespaces) *)
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom s ->
    begin match int_of_string_opt s with (* A frequent trouble: when nesting [match] the inner match must *)
      | Some n -> Num n                  (* be enclosed in [( ... )] or [begin ... end] *)
      | None -> Var s
    end
  | `List [`Atom "not" ; sexpr] -> Not (parse sexpr)
  | `List [`Atom "fun" ; `List [`Atom id] ; body] -> Function (id, parse body)
  (* "let" expressions are converted to functions when parsed! *)
  | `List [`Atom "let" ; `List bindings ; body] ->
    begin
      match bindings with
      | `List [`Atom id; expr] :: rest ->
        Apply (Function (id, parse (`List [`Atom "let"; `List rest; body])),
        parse expr)
      | [] -> parse body
      | x -> Fmt.failwith "parse error: invalid syntax: %a" CCSexp.pp (`List x)
    end
  | `List [`Atom s ; e1 ; e2] ->
    begin match s with
    | "+" -> Plus (parse e1, parse e2)
    | "-" -> Minus (parse e1, parse e2)
    | "*" -> Times (parse e1, parse e2)
    | "/" -> Divide (parse e1, parse e2)
    | "=" -> Equals (parse e1, parse e2)
    | "<" -> Less (parse e1, parse e2)
    | "or" -> Or (parse e1, parse e2)
    | "and" -> And (parse e1, parse e2)
    | _ -> Fmt.failwith "parse error: not a valid operation: %s" s
    end
  | `List [`Atom "if" ; c ; t ; f ] -> If (parse c, parse t, parse f)
  | `List [fun_expr ; arg_expr] -> Apply (parse fun_expr, parse arg_expr)
  | e -> Fmt.failwith "parse error: not a valid exp: %a" CCSexp.pp e

let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse file %s: %s" filename msg


(** Expression simplification **)
let rec simplify (exp : exp) : exp = 
match exp with
  | Plus (e1, e2) ->
    begin
      match simplify e1, simplify e2 with
      | Num 0, y -> y
      | x, Num 0 -> x
      | x, y -> Plus (x, y)
    end
  | Times (e1, e2) ->
    begin
      match simplify e1, simplify e2 with
      | Num 1, y -> y
      | x, Num 1 -> x
      | Num 0, _ -> Num 0
      | _, Num 0 -> Num 0
      | x, y -> Times (x, y)
    end
  | Minus (e1, e2) -> 
    begin 
      match simplify e1, simplify e2 with
      | x, Num 0 -> x
      | x, y -> Minus (x, y)
    end
  | Divide (e1, e2) ->
    begin 
      match simplify e1, simplify e2 with
      | x, Num 1 -> x
      | x, y when x = y -> Num 1
      | x, y -> Divide (x, y)
    end
  | Or (e1, e2) ->
    begin
      match simplify e1, simplify e2 with
      | p, q when p = q -> p
      | _,  Bool true -> Bool true
      | Bool true, _ -> Bool true
      | Bool false, q -> q
      | p, Bool false -> p
      | p, q -> Or (p, q)
    end
  | And (e1, e2) ->
    begin
      match simplify e1, simplify e2 with
      | p, q when p = q -> p
      | _, Bool false -> Bool false
      | Bool false, _ -> Bool false
      | p, Bool true -> p
      | Bool true, q -> q
      | p, q -> And (p, q)
    end
  | Equals (e1, e2) ->
    begin
      match simplify e1, simplify e2 with
      | x, y when x = y -> Bool true
      | x, y -> Equals (x, y)
    end
  | Less (e1, e2) ->
    begin
      match simplify e1, simplify e2 with
      | x, y when x = y -> Bool false
      | x, y -> Less (x, y)
    end
  | _ -> exp


let sexp_from_string str =
  match CCSexp.parse_string str with
  | Ok s -> s
  | Error msg -> Fmt.failwith "%s" msg


let run s_str =
  interp empty_env (simplify (parse (sexp_from_string s_str)))

