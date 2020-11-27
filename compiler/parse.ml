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

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse src %s: %s" src msg

let sexp_list_from_string (src : string) : CCSexp.sexp list =
  match CCSexp.parse_string_list src with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse src %s: %s" src msg

let sexp_from_file (filename : string) : CCSexp.sexp =
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse file %s: %s" filename msg

  let sexp_list_from_file (filename : string) : CCSexp.sexp list=
    match CCSexp.parse_file_list filename with
    | Ok l -> l
    | Error msg -> Fmt.failwith "Unable to parse file %s: %s" filename msg


open Ast

let notFuns = [
  "true"; "false"; "add1"; "sub1"; "not"; "or"; "and"; "+"; "-"; "*";
  "/"; "if"; "let"; "<"; "="; "def"]



let rec range (init: int) (finish: int) : int list =
  if init = finish then [] else init::range (init+1) (finish)

let parse_tup_matching  (tup: expr) (ids: t list): (string * expr) list=
  let strids = List.map 
  (fun id -> 
    match id with 
    |`Atom i -> i
    |_-> Fmt.failwith "Patter matching failed, not an id to assign") ids in
  let idsxindex = List.map2 (fun id idx -> (id, idx)) strids (range 0 @@ List.length ids) in
  List.map (fun (id, idx) -> (id, Get (tup, Num (Int64.of_int idx)))) idsxindex


let parse_def (parse: sexp -> expr): 'a -> (string * expr) list  =
fun def ->
  match def with
  | `List [`Atom id; v] -> [(id, parse v)]
  | `List [ `List (`Atom "tup"::args); e] ->  parse_tup_matching (parse e) args
  | _ -> Fmt.failwith "Unable to parse defs in Let sentence."


let rec parse_defs (parse: sexp -> expr): sexp list -> (string * expr) list =
  fun defs ->
    match defs with
    | [] -> []
    | def::t -> (parse_def parse) def @ (parse_defs parse) t

let rec parse_expr (sexp : sexp) : expr = 
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom "void" -> Void
  | `Atom s ->
    begin match Int64.of_string_opt s with
      | Some n -> Num n
      | None -> Id s 
    end
  | `List [`Atom "add1" ; e ] -> UnOp (Add1, parse_expr e) 
  | `List [`Atom "sub1" ; e ] -> UnOp (Sub1, parse_expr e)
  | `List [`Atom "not" ; e ] -> UnOp (Not, parse_expr e)
  | `List [`Atom "or" ; p ; q] -> BinOp (Or, parse_expr p, parse_expr q)
  | `List [`Atom "and" ; p ; q] -> BinOp (And, parse_expr p, parse_expr q)
  | `List [`Atom "+" ; l ; r ] -> BinOp (Add, parse_expr l, parse_expr r)
  | `List [`Atom "-" ; l ; r ] -> BinOp (Sub, parse_expr l, parse_expr r)
  | `List [`Atom "*" ; l ; r ] -> BinOp (Mul, parse_expr l, parse_expr r)
  | `List [`Atom "/" ; l ; r ] -> BinOp (Div, parse_expr l, parse_expr r)
  | `List [`Atom "<" ; l ; r ] -> BinOp (Less, parse_expr l, parse_expr r)
  | `List [`Atom "=" ; l ; r ] -> BinOp (Eq, parse_expr l, parse_expr r)
  | `List [`Atom "let" ; `List [ `List (`Atom "tup"::args); e]; body] -> 
    let parsed_tuple = parse_expr e in
    let parsed_defs = parse_tup_matching parsed_tuple args in
    Let (parsed_defs, parse_expr body) 
  | `List [`Atom "let" ; `List [`Atom id; e]; body] -> (* Simple Let *)
    Let ([(id, parse_expr e)], parse_expr body)
  | `List [`Atom "let" ; `List defs; body] -> (* Multiple Let *)
    Let ((parse_defs parse_expr) defs, parse_expr body)
  | `List [`Atom "if" ; c; t; f] -> 
    If (parse_expr c, parse_expr t, parse_expr f)
  | `List [`Atom "get" ; t; i] -> 
    Get (parse_expr t, parse_expr i)
  | `List [`Atom "set" ; t; i; v] -> 
    Set (parse_expr t, parse_expr i, parse_expr v)
  | `List [`Atom "len" ; t] -> 
    Length (parse_expr t)
  | `List (`Atom "tup"::args) -> 
    Tuple (List.map parse_expr args)
  | `List (`Atom "@sys"::(`Atom fname::args)) -> 
    Sys (fname, List.map parse_expr args)
  | `List (`Atom fname::args) -> 
    if List.mem fname notFuns
      then Fmt.failwith "Not a valid function: %s" fname else
    begin match Int64.of_string_opt fname with
      | Some _ -> Fmt.failwith "Not a valid function: %s" fname
      | None -> App (fname, List.map parse_expr args) (* FIX *)
    end 
  | e -> Fmt.failwith "Not a valid exp: %a" CCSexp.pp e

let parse_param (sexp : sexp) : string =
  match sexp with
  | `Atom id -> id
  | _ -> Fmt.failwith "Not a valid parameter name: %a" CCSexp.pp sexp

let parse_type (sexp : sexp) : dtype =
  match sexp with
  | `Atom "int" -> IntT
  | `Atom "bool" -> BoolT
  | `Atom "tup" -> TupleT
  | `Atom "rec" -> RecordT
  | `Atom "any" -> AnyT
  | _ -> Fmt.failwith "Not a valid type: %a" CCSexp.pp sexp

let parse_field (sexp: sexp) : string =
  match sexp with
  | `Atom id -> id
  | _ -> Fmt.failwith "Not a valid field name: %a" CCSexp.pp sexp

let rec find_duplicates (l: 'a list) : 'a = 
  match l with 
    | [] -> ""
    | head::tail -> if List.mem head tail then head else find_duplicates tail

let parse_decl (sexp : sexp) : decl = 
  match sexp with
  | `List [`Atom "def" ; `List (`Atom name :: params) ; body] ->
    let ps = List.map parse_param params in
    begin match find_duplicates ps with
      |"" -> FunDef (name, ps, parse_expr body)
      | dup ->
        Fmt.failwith "Duplicate parameter name in function %s: %s" name dup
    end
  | `List (`Atom "defsys"::(`Atom name::rest)) ->
    begin
      match List.rev rest with
      | (ret::(`Atom "->"::params)) ->
        SysFunDef (name, List.map parse_type params, parse_type ret)
      | _ -> Fmt.failwith "Syntax error: %a" CCSexp.pp sexp
    end
  | `List (`Atom "record"::(`Atom id::fields)) ->
    let fs = List.map parse_field fields in
    begin 
      match find_duplicates fs with
      | "" -> RecDef(id, fs)
      | dup -> Fmt.failwith "Duplicate field name in record %s: %s" id dup
    end
  | _ -> Fmt.failwith "Not a valid declaration: %a" CCSexp.pp sexp

let parse_prog (sexps : sexp list) : prog = 
  match List.rev sexps with
  | expr::decls -> Program (List.map parse_decl decls, parse_expr expr)
  | [] -> Program ([], Void)
