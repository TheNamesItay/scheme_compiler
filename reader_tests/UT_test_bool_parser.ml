#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

(* TESTER: test_bool_parser.ml *)
let test_true_lower () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool true)] (Reader.read_sexprs "#t");;
let test_true_upper () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool true)] (Reader.read_sexprs "#T");;
let test_false_lower () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool false)] (Reader.read_sexprs "#f");;
let test_false_upper () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool false)] (Reader.read_sexprs "#F");;

let test_true_lower_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool true)]
  (Reader.read_sexprs "  #t ");;
let test_true_lower_whitespace_right_side () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool true)]
  (Reader.read_sexprs "#t   ");;
let test_true_lower_whitespace_left_side () = Alcotest.(check (list sexp_testable)) "same sexp?" [(Bool true)]
  (Reader.read_sexprs "   #t");;

(* Name the test cases and group them together *)
let bool_parser_tester_suite =
 [("test_true_lower", test_true_lower);
  ("test_true_upper", test_true_upper);
  ("test_false_lower", test_false_lower);
  ("test_false_upper", test_false_upper);
  ("test_true_lower_whitespace_both_sides", test_true_lower_whitespace_both_sides);
  ("test_true_lower_whitespace_right_side", test_true_lower_whitespace_right_side);
  ("test_true_lower_whitespace_left_side", test_true_lower_whitespace_left_side);
  ]
;;
