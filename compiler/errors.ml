open Asm
open Ast
open Encode
open Gensym
open Env

(* INTEGER ERROR CODES *)
let not_a_number = Const 1L
let not_a_boolean = Const 2L 
let not_a_tuple = Const 3L
let neg_index = Const 4L
let index_overflow = Const 5L
let not_a_record = Const 6L
let record_type_error = Const 7L
let arity_mismatch_error = Const 8L
let not_a_closure = Const 9L



(* other constants *)

let err_code_reg = R12
let err_val_reg = R13
let err_aux_reg = R14
let error_handler_label = "error_handler"

(* COMPILE TIME ERRORS *)

(*  check_function_name
  Raises an appropiate error if the env already contains another value 
bound to the same identifier. This is meant to be used with functions 
defined with the 'def' keyword. 
*)
let check_function_name (fname: string) (env: env) : string =
  let lenv, _ = env in
  if List.mem_assoc fname lenv then 
    Fmt.failwith "Duplicate function name: %s" fname;
  fname


(* RUNTIME ERRORS *)

(* ERROR HANDLER *)
let error_handler =
  [ILabel error_handler_label;
   IMov (Reg RSI, (Reg err_val_reg));
   IMov (Reg RDI, (Reg err_code_reg));
   ICall (Label "error")]

(* 
  TYPE CHECKING - GENERAL
  Checks the type of a register content. Content of register is unchanged.
  This function handles the request for a type checking
*)
let type_checking (register: arg) (expected_type: dtype): instruction list =
  let lbl = typechk_gensym () in
  let tag_check error_code type_tag =
    [ILabel lbl
    ;IMov ((Reg err_val_reg), register) 
    ;IAnd ((Reg err_val_reg), tag_bitmask) 
    ;ICmp ((Reg err_val_reg), type_tag) 
    ;IMov ((Reg err_code_reg), error_code) 
    ;IMov ((Reg err_val_reg), register)
    ;IMov (Reg err_aux_reg, Label lbl)
    ;IJnz (Label error_handler_label)]
  in
  let beg_comm = 
    [IComment (Fmt.str "Begin Type Checking %s" @@ str_type expected_type)] 
  in
  let end_comm = 
    [IComment (Fmt.str "End Type Checking %s" @@ str_type expected_type) ] 
  in
  beg_comm @
  begin match expected_type with
  | IntT -> [ILabel lbl ;
    IMov ((Reg err_val_reg), register) ;
    IMov ((Reg err_code_reg), not_a_number) ;
    ITest (Reg err_val_reg, Const 1L) ;
    IMov (Reg err_aux_reg, Label lbl) ;
    IJnz (Label error_handler_label);
  ]
  | BoolT -> tag_check not_a_boolean bool_tag
  | TupleT -> tag_check not_a_tuple tuple_tag
  | RecordT -> tag_check not_a_record record_tag
  | ClosureT -> tag_check not_a_closure function_tag
  | AnyT -> [] (* AnyT == no typecheck *)
  end
  @ end_comm


(* 
  CHECK ARITY - FUNCTIONS
  Checks the number of passed args is
  effectively the required number of args.
*)
let check_arity (arity: arg) (args: int) : instruction list =
  let lbl = (errorchk_gensym ()) in
  [ ILabel lbl;
    IComment "INIT Function arity check";
    IMov ((Reg err_code_reg), arity_mismatch_error) ; 
    IMov ((Reg err_val_reg), arity) ;
    ICmp ((Reg err_val_reg), Const (encode_int (Int64.of_int args)));  
    IMov (Reg err_aux_reg, Label lbl) ;   
    IJne  (Label error_handler_label);
    IComment "END  Function arity check";
  ]
 
  
(* 
  CHECK INDEX - TUPLES
  Checks the access for positive indexes and
  avoid overflows with bigger indexes
*)
let check_index (index: arg) : instruction list =
  let lbl = errorchk_gensym () in
  [ (* Negative Index Error *)
    ILabel lbl ;
    IComment "Begin Check: Positive Index for Tuples";
    IMov ((Reg err_code_reg), neg_index) ; 
    IMov ((Reg err_val_reg), index) ;
    ICmp (index, Const 0L);     
    IJl  (Label error_handler_label);
    IComment "End Check: Positive Index for Tuples";
    (* Index out of bounds *)
    IComment "Begin Check: Index out of Bounds";
    IMov ((Reg err_code_reg), index_overflow) ; 
    ICmp (index, RegOffset (RAX, 0)); 
    IMov (Reg err_aux_reg, Label lbl) ;
    IJge (Label error_handler_label);
    IComment "End Check: Index out of Bounds";
  ]

(**
  CHECK RECORD TYPE - RECORDS
  Verifies that the record pointed by the given register has the correct 
  tag, which is a 64bit word where the first 32 bits are an arbitrary 
  integer and the last 32 bits indicate the size of the record.

  value is returned untagged in the same register it was pased in.
*)
let check_rectag (rec_reg: reg) (expected_tag: int64) : instruction list =
  let lbl = errorchk_gensym () in
  [ ILabel lbl
  ; IComment "tag check" 
  ; IMov (Reg err_val_reg, Reg rec_reg)
  ; IMov (Reg err_code_reg, record_type_error)
  ; ISub (Reg rec_reg, record_tag)
  ; IMov (Reg err_aux_reg, Const expected_tag)
  ; ICmp (RegOffset (rec_reg, 0), Reg err_aux_reg)
  ; IMov (Reg err_aux_reg, Label lbl)
  ; IJne (Label error_handler_label)
  ; IComment "end tag check"]
