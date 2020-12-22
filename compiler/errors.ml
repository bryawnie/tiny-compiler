open Asm
open Ast
open Encode

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
let rax_error_handler = "error_type_handler"
let rcx_error_handler = "error_index_handler"

(* 
  TYPE CHECKING - GENERAL
  Checks the type of a register content
  This function handles the request for a type checking
*)
let type_checking (register: arg) (expected_type: dtype): instruction list =
  let tag_check error_code type_tag =
    [IMov ((Reg err_code_reg), error_code) ; IMov ((Reg type_code_reg), register) ;
    IAnd ((Reg type_code_reg), tag_bitmask) ; ICmp ((Reg type_code_reg), type_tag) ;
    IJnz (Label "error_type_handler")]
  in
  let beg_comm = [IComment (Fmt.str "Begin Type Checking %s" @@ str_type expected_type)] in
  let end_comm = [IComment (Fmt.str "End Type Checking %s" @@ str_type expected_type) ] in
  beg_comm @
  begin match expected_type with
  | IntT -> [
    ITest (register, Const 1L) ; 
    IMov ((Reg err_code_reg), not_a_number) ;
    IJnz (Label "error_type_handler");
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
  [ 
    IComment "INIT Function arity check";
    IMov ((Reg err_code_reg), arity_mismatch_error) ; 
    IMov ((Reg type_code_reg), arity) ;
    ICmp ((Reg type_code_reg), Const (encode_int (Int64.of_int args)));     
    IJne  (Label "error_arity_handler");
    IComment "END  Function arity check";
  ]
 
  
(* 
  CHECK INDEX - TUPLES
  Checks the access for positive indexes and
  avoid overflows with bigger indexes
*)
let check_index (index: arg) : instruction list =
  [ (* Negative Index Error *)
    IComment "Begin Check: Positive Index for Tuples";
    IMov ((Reg err_code_reg), neg_index) ; 
    IMov ((Reg type_code_reg), index) ;
    ICmp (index, Const 0L);     
    IJl  (Label "error_index_handler");
    IComment "End Check: Positive Index for Tuples";
  ] @ [ (* Index out of bounds *)
    IComment "Begin Check: Index out of Bounds";
    IMov ((Reg err_code_reg), index_overflow) ; 
    ICmp (index, RegOffset (RAX, 0)); 
    IJge (Label "error_index_handler");
    IComment "End Check: Index out of Bounds";
  ]