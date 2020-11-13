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