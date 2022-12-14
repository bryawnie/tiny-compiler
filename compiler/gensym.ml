(* A gensym for standard symbols and specific instructions *)
let create_gensym s = 
  let r = ref 0 in 
  (fun () -> incr r; s ^ "_" ^ string_of_int !r)  

(* A simple gensym *)
let gensym  =
  let counter = ref 0 in  
  (fun basename ->
    counter := !counter + 1;
      Format.sprintf "%s_%d" basename !counter)

let if_gensym  = create_gensym "if"
let if_false_gensym = create_gensym "if_false"
let done_gensym = create_gensym "done"
let less_gensym = create_gensym "less"
let equal_gensym = create_gensym "equal"
let tmp_gensym = create_gensym "tmp"
let and_gensym = create_gensym "and"
let or_gensym = create_gensym "or"
let fun_gensym = create_gensym "fun"
let typechk_gensym = create_gensym "type_check"(* type check *)
let errorchk_gensym = create_gensym "error_check" (* other error checks *)

(* returns a label that can be used in assembly *)
let get_fun_label fun_name =
  let clean_name = String.split_on_char '-' fun_name in
  String.concat "_" @@ fun_gensym()::clean_name

(* create a simple counter *)
let create_counter () =
  let r = ref 0 in (fun () -> incr r; !r)

let record_counter = create_counter ()
