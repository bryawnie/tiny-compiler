(* RECORD COMPILER START

(* compiles the constructor function for a record *)
let compile_rec_constructor (id: string) (id_code: int64) (arity: int) 
  (env: env) : instruction list =
  let flbl, _ = fun_lookup id env in
  let heap_size = Const (Int64.of_int (8 + arity * 8)) in
  let rec move_args n =
    if n <= 0 then []
    else if n > 6 then 
      IMov(RegOffset(heap_reg, n), RegOffset(RBP, -2 -(n-6)))::(move_args @@ n-1)
    else (IMov(RegOffset(heap_reg, n), List.nth arg_regs (n-1)))
      ::(move_args @@ n-1)
  in
  [IEmpty; ILabel flbl ;(* constructor name *)
  (* rsp does not move because this function does not use the stack.
  rbp is not backed up because it is not modified. *)
  IMov ((Reg ret_reg), Const id_code); (* rax <- id_code *)
  IMov (RegOffset(heap_reg, 0), (Reg ret_reg))] (* write id_code to heap *) 
  @ move_args arity               (* write values to heap *)
  @ [IMov ((Reg ret_reg), Reg heap_reg) ; (* get pointer value *)
  IAdd ((Reg ret_reg), record_tag) ;    (* tag value *)
  IAdd (Reg heap_reg, heap_size) ;(* bump heap *)
  IRet]                           (* return *)

(* compiles the get function for a record field *)
let compile_rec_get (rec_id: string) (id_code: int64) 
  (field_id: string) (field_pos: int) (env:env) =
  let flbl, _ =
    let fun_name = String.concat "-" [rec_id ; field_id] in
    fun_lookup fun_name env
  in
  [ILabel flbl ;                      (* function label *)
  IMov ((Reg ret_reg), Reg RDI)]            (* rax <- val *)
  @ type_checking (Reg ret_reg) RecordT @   (* type check rax *)
  [ISub (Reg RDI, record_tag) ;             (* untag value to obtain pointer *)
  IMov ((Reg aux_reg), Const id_code) ;     (* r11 <- id_code *)
  IMov (Reg err_code_reg, record_type_error) ; (* r <- error_code *)
  ICmp (RegOffset(RDI, 0), (Reg aux_reg)) ; (* ID CHECK! *)
  IJne rax_error_handler;             (* if != then error_handler *)
  IMov (Reg RAX, RegOffset(RDI, field_pos)) ; (* rax <- [rax + 8 * i]*)
  IRet] (* return *)

let rec compile_getters (rec_id: string) (rec_code: int64) 
  (fields: string list) (pos: int) (env: env) =
  match fields with
  | [] -> []
  | field_id::tail ->
    (compile_rec_get rec_id rec_code field_id pos env)
    @ (compile_getters rec_id rec_code tail (pos+1) env)

(* Compiles a record type checker. Type checkers have the name of the form
"[record_id]?" and return true if the passed argument is of type record_id *)
let compile_rec_typechecker (rec_id: string) (id_code: int64) (env: env)
  : instruction list =
  let label, _ = 
    let fname = String.concat "" [rec_id ; "?"] in
    fun_lookup fname env
  in
  let return_label = String.concat "_" [label ; "return"] in
  [ILabel label ;
  IMov ((Reg ret_reg), Const false_encoding) ;  (* rax <- false *)
  IMov ((Reg aux_reg), Reg RDI) ;               (* r11 <- val *)
  IAnd ((Reg aux_reg), tag_bitmask) ;           (* get type tag *)
  ICmp ((Reg aux_reg), record_tag) ;            (* compare to record type tag *)
  IJne return_label ;                     (* if != return false *)
  IMov ((Reg aux_reg), Const id_code) ;         (* r11 <- id_code *)
  ISub (Reg RDI, record_tag) ;            (* untag to obtain pointer *)
  ICmp (RegOffset(RDI, 0), (Reg aux_reg)) ;     (* check record type *)
  IJne return_label ;                     (* if != return false *)
  IMov ((Reg ret_reg), Const true_encoding) ;   (* rax <- false*)
  ILabel return_label ;
  IRet]

(* Compiles constructors and getters for a record type *)
let compile_recdef (id: string) (fields: string list) (env: env) : instruction list =
  (* This tag allows for record type checking at runtime.
    tag = (record_counter() << 32) | length(fields) *)
  let rec_id_code = Int64.add
    (Int64.shift_left (Int64.of_int @@ record_counter ()) 32)
    (Int64.of_int @@ List.length fields)
  in 
  let constructor = 
    compile_rec_constructor id rec_id_code (List.length fields) env
  in
  let getters = compile_getters id rec_id_code fields 1 env in
  let type_checker = compile_rec_typechecker id rec_id_code env in
  constructor @ getters @ type_checker

RECORD COMPILER END *)