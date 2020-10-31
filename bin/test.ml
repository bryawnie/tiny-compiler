open Compiler
open Ast
open Compile
open Alcotest

let decl : decl testable =
  testable pp_decl (=)
let prog : prog testable =
  testable pp_prog (=)
let expr : expr testable =
  testable pp_expr (=)

let parse_tests =
  let open Parse in

  (* Tests for our [parse] function *)
  let test_parse_int () =
    check expr "same int" (parse_expr (`Atom "5")) (Num 5L)
  in


  let test_parse_compound () =
    check expr "same expr"
      (parse_expr (`List [`Atom "add1" ; `List [`Atom "sub1" ; `Atom "3"; ]]))
      (UnOp (Add1, UnOp (Sub1, (Num 3L))))
  in

  let test_parse_expressions () =
    check expr "Same expr"
      (parse_expr (sexp_from_string "true"))
      (Bool true) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "false"))
      (Bool false) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "x"))
      (Id "x") ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(add1 5)"))
      (UnOp (Add1, Num 5L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(sub1 3)"))
      (UnOp (Sub1, Num 3L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(not false)"))
      (UnOp (Not, Bool false)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(+ 3 4)"))
      (BinOp (Add, Num 3L, Num 4L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(- 7 6)"))
      (BinOp (Sub, Num 7L, Num 6L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(* 2 5)"))
      (BinOp (Mul, Num 2L, Num 5L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(/ 12 6)"))
      (BinOp (Div, Num 12L, Num 6L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(and true false)"))
      (BinOp (And, Bool true, Bool false)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(or true false)"))
      (BinOp (Or, Bool true, Bool false)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(< 8 7)"))
      (BinOp (Less, Num 8L, Num 7L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(= 1 1)"))
      (BinOp (Eq, Num 1L, Num 1L)) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(let (x 5) x)"))
      (Let ("x", Num 5L, Id "x")) ;
    check expr "Same expr"
      (parse_expr (sexp_from_string "(if true 5 6)"))
      (If (Bool true, Num 5L, Num 6L)) ;
  in

  (* An example of a test catching an error *)
  let test_parse_error () =
    check_raises "Should fail"
      (Failure (Fmt.strf "Not a valid function: %a" CCSexp.pp (sexp_from_string "true")))
      (fun () -> ignore @@ parse_expr (sexp_from_string "(if (true) 5 6)")) ;
    check_raises "Should fail"
      (Failure (Fmt.strf "Not a valid function: %a" CCSexp.pp (sexp_from_string "+")))
      (fun () -> ignore @@ parse_expr (sexp_from_string "(+ 4 5 6)")) ;
    check_raises "Should fail"
      (Failure (Fmt.strf "Not a valid function: %a" CCSexp.pp (sexp_from_string "4")))
      (fun () -> ignore @@ parse_expr (sexp_from_string "(4 * 8)")) ;
  in

  let test_parse_declarations () =
    check decl "Same declaration"
      (FunDef ("f", ["x" ; "y"], BinOp (Add, Id "x", Id "y")))
      (parse_decl @@ sexp_from_string "(def (f x y) (+ x y))") ;
    check_raises "Should fail"
      (Failure "Not a valid parameter name: (x y)")
      (fun () -> ignore @@
        parse_decl (sexp_from_string "(def (f (x y)) (+ x y))")) ;
    check_raises "Should fail"
      (Failure "Not a valid declaration: (def f (x) (+ x x))")
      (fun () -> ignore @@
        parse_decl (sexp_from_string "(def f (x) (+ x x))")) ;
  in

  let test_parse_program () =
    check prog "Same program"
      (Program ([FunDef ("f", ["x"], Id "x")], App ("f", [Num 1L])))
      (parse_prog @@ sexp_list_from_string "(def (f x) x)\n(f 1)") ;
    check_raises "Should fail. Declarations must precede program body."
      (Failure "Not a valid function: def")
      (fun () -> ignore @@
        parse_prog (sexp_list_from_string "(f 1)\n(def (f x) x)")) ;
    check_raises "Should fail. A program can't consist only of declarations"
      (Failure "Not a valid function: def")
      (fun () -> ignore @@
        parse_prog (sexp_list_from_string "(def (f) 1)\n(def (g x) (+ x 1))"));
  in

  "parse", [
    test_case "A number" `Quick test_parse_int ;
    test_case "A compound expression" `Quick test_parse_compound ;
    test_case "All expressions" `Quick test_parse_expressions;
    test_case "An invalid s-expression" `Quick test_parse_error ;
    test_case "Declarations" `Quick test_parse_declarations ;
    test_case "Full program with declarations" `Quick test_parse_program ;
  ]


let interp_tests =
  let open Interp in

  let value : value testable =
    testable pp_value (=)
  in

  (* Tests for our [interp] function *)
  let test_interp_num () =
    check value "same int" (interp (Num 42L)) (NumV 42L)
  in

  let test_interp_compound () =
    check value "same int"
      (interp (UnOp (Add1, (UnOp (Add1, (Num 40L))))))
      (NumV 42L)
  in

  let test_interp_not () =
    check value "not true = false"
      (BoolV false)
      (interp (UnOp (Not, Bool true))) ;
    check value "not false = true"
      (BoolV true)
      (interp (UnOp (Not, Bool false))) ;
    check value "(Not (Not (Not false))) = false " 
      (BoolV false)
      (interp (UnOp (Not, UnOp (Not , UnOp (Not ,(Bool true))))));
    check value "If-Not true 5 else 6"
      (NumV 6L)
      (interp (If ((UnOp (Not, (Bool true))), (Num 5L), (Num 6L))));      
  in

  let test_interp_and () =
    check value "true and true = true"
      (BoolV true)
      (interp (BinOp (And, Bool true, Bool true))) ;
    check value "true and false = false"
      (BoolV false)
      (interp (BinOp (And, Bool true, Bool false))) ;
    check value "false and true = false"
      (BoolV false)
      (interp (BinOp (And, Bool false, Bool true))) ;
    check value "false and false = false"
       (BoolV false)
       (interp (BinOp (And, Bool false, Bool false)))
  in

  let test_interp_or () = 
    check value "true or true = true"
      (BoolV true)
      (interp (BinOp (Or, Bool true, Bool true))) ;
    check value "true or false = true"
      (BoolV true)
      (interp (BinOp (Or, Bool true, Bool false))) ;
    check value "false or true = true"
      (BoolV true)
      (interp (BinOp (Or, Bool false, Bool true))) ;
    check value "false or false = false"
      (BoolV false)
      (interp (BinOp (Or, Bool false, Bool false)))
  in

  let test_interp_bool_ops () = 
    check value "and"
      (BoolV true)
      (interp (BinOp (And, Bool true, (BinOp (Or, Bool true, Bool false))))) ;
    check value "or" 
      (BoolV false)
      (interp (BinOp (Or, (BinOp (And, Bool true, Bool false)), Bool false))) ;
    check value "compound expression"
     (BoolV false)
     (interp (BinOp (And, Bool true, BinOp (Or, Bool false, UnOp (Not, Bool true)))))
  in

  let test_interp_bool_semantics () =
    (* or *)
    check value "(Or) Second expr should not execute"
      (BoolV true)
      (interp (BinOp (Or, Bool true, UnOp (Not, Num 1L)))) ;

    check_raises "(Or) Raises error"    
      (Failure (Fmt.strf "Error: Non boolean expr in Not sentence"))
      (fun () ->
        ignore @@ (interp (BinOp (Or, Bool false, UnOp (Not, Num 1L))))) ;
    
    (* and *)
    check value "(And) Second expr does not execute" 
      (BoolV false)
      (interp (BinOp (And, Bool false, UnOp (Not, Num 1L)))) ;

    check_raises "(And) Raises error"
      (Failure (Fmt.strf "Error: Non boolean expr in Not sentence"))
      (fun () ->
        ignore @@ (interp (BinOp (And, Bool true, UnOp (Not, Num 1L)))));
  in

  let  test_interp_unop () =
    check value "add1 5"
      (interp (UnOp (Add1, Num 5L)))
      (NumV 6L);
    check value "sub1 5"
      (interp (UnOp (Sub1, Num 5L)))
      (NumV 4L);
    check value "sub1 5"
      (interp (UnOp (Sub1, (UnOp (Add1, Num 5L)))))
      (NumV 5L);
  in

  let  test_interp_binop () =
    check value "+ 5 6"
      (interp (BinOp (Add, Num 5L, Num 6L)))
      (NumV 11L);
    check value "- 7 6"
      (interp (BinOp (Sub, Num 7L, Num 6L)))
      (NumV 1L);
    check value "* 5 3"
      (interp (BinOp (Mul, Num 5L, Num 3L)))
      (NumV 15L);
    check value "/ 18 6"
      (interp (BinOp (Div, Num 18L, Num 6L)))
      (NumV 3L);
    check value "< 5 6"
      (interp (BinOp (Less, Num 5L, Num 6L)))
      (BoolV true);
    check value "< 6 5"
      (interp (BinOp (Less, Num 6L, Num 5L)))
      (BoolV false);
    check value "= 5 5"
      (interp (BinOp (Eq, Num 5L, Num 5L)))
      (BoolV true);
    check value "= 5 6"
      (interp (BinOp (Eq, Num 5L, Num 6L)))
      (BoolV false);
    check_raises "Minor num bool" 
      (Failure (Fmt.strf "Error: Numeric comparator applied to non numeric values"))
      (fun () -> ignore @@ interp  (BinOp (Less, Num 5L, Bool true)))
  in

  let  test_interp_if () =
    check value "If true" 
      (interp (If (Bool true, Num 5L, Num 6L)) )
      (NumV 5L);
    check value "If false" 
      (interp (If (Bool false, Num 5L, Num 6L)) )
      (NumV 6L);
    check value "If true expr" 
      (interp (If (Bool true, BinOp (Add, Num 5L, Num 4L), Num 6L)) )
      (NumV 9L);
    check_raises "Should fail" 
      (Failure (Fmt.strf "Error: Non boolean condition in If sentence"))
      (fun () -> ignore @@ interp (If (Num 5L, Num 6L, Num 7L)))
  in

  let  test_interp_let () = 
    check value "let ( x 5 ) (+ x 6)" 
      (interp (Let ("x", Num 5L, BinOp (Add, Id "x", Num 6L))))
      (NumV 11L);
    check value "let (x (not true)) x" 
      (interp  (Let ("x", UnOp (Not, Bool true), Id "x")))
      (BoolV false);
    check value "let (x 5) (let (x (add1 x)) x)"
      (interp (Let ("x", Num 5L, (Let ("x", UnOp (Add1, Id "x"), Id "x")))))
      (NumV 6L);
  in


  "interp", [
    (* Use the `Slow parameter for tests that only need to be run with the full test suite
      The tests here only concern the interpreter, so we tag them as slow.
      Set the ALCOTEST_QUICK_TESTS environment variable (to =1 for instance) to disable slow tests. *)
    test_case "A number" `Slow test_interp_num ;
    test_case "A compound expression" `Slow test_interp_compound ;
    test_case "Boolean negation" `Slow test_interp_not ;
    test_case "Boolean disyunction" `Slow test_interp_or ;
    test_case "Boolean conjunction" `Slow test_interp_and ;
    test_case "All boolean operators" `Slow test_interp_bool_ops ;
    test_case "and/or lazy semantics" `Slow test_interp_bool_semantics ;
    test_case "Integer UnOps" `Slow test_interp_unop;
    test_case "Integer BinOps" `Slow test_interp_binop;
    test_case "If sentences" `Slow test_interp_if;
    test_case "Let-bindings" `Slow test_interp_let;
  ]

let interpreter (src : string) : string =
  let open Interp in
  let e = Parse.(parse_expr (sexp_from_string src)) in
  Fmt.to_to_string pp_value (interp e)

(* Entry point of tests
 * Beware that the [Alcotest] library takes control of all command line
 * arguments in [run].
 * See the documentation at https://github.com/mirage/alcotest *)
let () =
  run "Compiler" @@
    [ parse_tests ; interp_tests ]
    @ Bbctester__Test.tests_from_dir 
        ~runtime:"compiler/rtsys.c" 
        ~compiler:compile_src 
        ~interpreter "tests"
