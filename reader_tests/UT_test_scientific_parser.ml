#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

(* TESTER: test_scientific_parser.ml *)
let test_positive_int_signed1 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (120.)))] (Reader.read_sexprs "+12e1  ");;
let test_positive_int_signed2 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (10.)))] (Reader.read_sexprs "+1e1  ");;
let test_positive_int_signed3 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (10.)))] (Reader.read_sexprs "1E1 ");;
let test_positive_int_signed4 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (1230.)))] (Reader.read_sexprs "    +000000012.3E00000002");;


let test_positive_int_second_signed1 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (10.)))] (Reader.read_sexprs "1E+1 ");;
let test_positive_int_second_signed2 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (0.1)))] (Reader.read_sexprs "1E-1 ");;
let test_positive_int_second_signed3 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (13.4)))] (Reader.read_sexprs "0000134E-1 ");;
let test_positive_int_second_signed4 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 7200000000.))] (Reader.read_sexprs "00072E+00008 ");;
let test_positive_float_second_signed5 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 3140000000.))] (Reader.read_sexprs "  3.14e+9 ");;



let test_positive_int_unsigned () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (100.)))] (Reader.read_sexprs " 10e1  ");;


let test_negative_float_second_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 0.))] (Reader.read_sexprs "  3.14E-512 ");;


let test_negative_float_signed_both () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (-0.05)))] (Reader.read_sexprs "    -5.000000000e-2  ");;


(* let test_assert_raises_1 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "    -5.000000000eff-2  "));;
let test_assert_raises_2 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "    -540300@e-2  "));;
(* test 3 raises no match cause of @ doesn't exist in any symbol. *)
let test_assert_raises_3 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "5.eff-2"));;
let test_assert_raises_4 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "6.2ea"));;
let test_assert_raises_5 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "4e.5"));; *)


(* Name the test cases and group them together *)
let scientific_parser_tester_suite =
    [("test_positive_int_signed1", test_positive_int_signed1);
     ("test_positive_int_signed2", test_positive_int_signed2);
     ("test_positive_int_signed3", test_positive_int_signed3);
     ("test_positive_int_signed4", test_positive_int_signed4);

     ("test_positive_int_second_signed1", test_positive_int_second_signed1);
     ("test_positive_int_second_signed2", test_positive_int_second_signed2);
     ("test_positive_int_second_signed3", test_positive_int_second_signed3);
     ("test_positive_int_second_signed4", test_positive_int_second_signed4);
     ("test_positive_float_second_signed5", test_positive_float_second_signed5);

     ("test_positive_int_unsigned", test_positive_int_unsigned);

     ("test_negative_float_second_signed", test_negative_float_second_signed);

     ("test_negative_float_signed_both", test_negative_float_signed_both);

     (* "test_assert_raises_1", test_assert_raises_1;
     "test_assert_raises_2", test_assert_raises_2;
     "test_assert_raises_3", test_assert_raises_3;
     "test_assert_raises_4", test_assert_raises_4;
     "test_assert_raises_5", test_assert_raises_5; *)
    ];;

(* END TESTER: test_scientific_parser.ml *)
