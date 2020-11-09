#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

(* TESTER: test_number_parser.ml *)
let test_negetive_int_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction ((-123), 1)))] (Reader.read_sexprs "-123");;
let test_positive_int_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction (123, 1)))] (Reader.read_sexprs "+123");;
let test_positive_int_not_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction (123, 1)))] (Reader.read_sexprs "123");;

let test_negetive_int_signed_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction ((-123), 1)))] (Reader.read_sexprs "  -123 ");;
let test_positive_int_signed_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction (123, 1)))] (Reader.read_sexprs "  +123 ");;
let test_positive_int_not_signed_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction (123, 1)))] (Reader.read_sexprs "   123 ");;

(* Edge cases of integers *)
let edge_int_leading_zeroes_positive () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction (123, 1)))] (Reader.read_sexprs "000123");;
let edge_int_leading_zeroes_negative () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction ((-123), 1)))] (Reader.read_sexprs "-000123");;
let edge_int_spaces_between () = Alcotest.(check (list sexp_testable)) "same number sexp" ([Number (Fraction (1, 1)); Number (Fraction (123, 1))]) (Reader.read_sexprs "1   123");;
let edge_int_space_to_float () = Alcotest.(check (list sexp_testable)) "same number sexp" ([Number (Fraction (1, 1)); Number(Float (3.6))]) (Reader.read_sexprs "5 3.6");;

(* Test positive & negative floats *)

let test_negetive_float_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (-123.1)))] (Reader.read_sexprs "-123.1");;
let test_negetive_float_signed_2 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (-0.4321)))] (Reader.read_sexprs "-0.4321");;
let test_positive_float_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.1))] (Reader.read_sexprs "+123.1");;
let test_positive_float_signed_2 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 0.4321))] (Reader.read_sexprs "+0.4321");;
let test_positive_float_not_signed () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.112))] (Reader.read_sexprs "123.112");;

let test_negative_float_signed_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (-123.112)))] (Reader.read_sexprs " -123.112  ");;
let test_positive_float_signed_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.112))] (Reader.read_sexprs " +123.112  ");;
let test_positive_float_not_signed_whitespace_both_sides () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.112))] (Reader.read_sexprs "  123.112  ");;

(* Edge cases of floats *)
let edge_float_leading_zeroes_positive () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.5))] (Reader.read_sexprs "000123.5");;
let edge_float_leading_zeroes_negative () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float (-123.5)))] (Reader.read_sexprs "-00123.5");;
let edge_float_leading_and_trailing_zeroes_positive () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.5))] (Reader.read_sexprs "000123.5000");;
let edge_float_spaces_between () = Alcotest.(check (list sexp_testable)) "same number sexp" ([Number (Float 1.1); Number (Fraction (123, 1))]) (Reader.read_sexprs "1.1 123");;

(* test numbers *)
let test_number_1 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.555))] (Reader.read_sexprs "000123.555");;
let test_number_2 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Float 123.00555))] (Reader.read_sexprs "000123.00555");;
let test_number_3 () = Alcotest.(check (list sexp_testable)) "same number sexp" [(Number (Fraction (1000, 1)))] (Reader.read_sexprs "0001000");;
let test_number_4 () = Alcotest.(check (list sexp_testable)) "same number sexp" ([Number (Float 1.2); Number(Float (3.45))]) (Reader.read_sexprs " 1.2 3.45");;
let test_number_5 () = Alcotest.(check (list sexp_testable)) "same number sexp" ([Number (Float 53.2); Number (Float 3.45); Number (Float 23.4)]) (Reader.read_sexprs "53.2 ;hello its a comment\n 3.45 ;another comment,,,, \n     23.4    ;another comment here.......\n");;
let test_number_6 () = Alcotest.(check (list sexp_testable)) "same number sexp" ([Number (Fraction ((-1), 1)); Number (Float 3.45); Number (Float 57.4)]) (Reader.read_sexprs "-001 ;hello its a comment\n 3.45;another comment,,,, \n   #;  64.4 57.4    ;another comment here.......\n");;


(* let test_assert_raises_1 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "0001@"));;
let test_assert_raises_2 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "0001.2@"));;
let test_assert_raises_3 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "- 234"));;
let test_assert_raises_4 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "+ 34"));; *)


(* Name the test cases and group them together *)
let number_parser_tester_suite =
 [("test_negetive_int_signed", test_negetive_int_signed);
  ("test_positive_int_signed", test_positive_int_signed);
  ("test_positive_int_not_signed", test_positive_int_not_signed);

  ("test_negetive_int_signed_whitespace_both_sides", test_negetive_int_signed_whitespace_both_sides);
  ("test_positive_int_signed_whitespace_both_sides", test_positive_int_signed_whitespace_both_sides);
  ("test_positive_int_not_signed_whitespace_both_sides", test_positive_int_not_signed_whitespace_both_sides);

  ("edge_int_leading_zeroes_positive", edge_int_leading_zeroes_positive);
  ("edge_int_leading_zeroes_negative", edge_int_leading_zeroes_negative);
  ("edge_int_spaces_between", edge_int_spaces_between);
  ("edge_int_space_to_float", edge_int_space_to_float);

  ("test_negetive_float_signed", test_negetive_float_signed);
  ("test_negetive_float_signed_2", test_negetive_float_signed_2);
  ("test_positive_float_signed", test_positive_float_signed);
  ("test_positive_float_signed_2", test_positive_float_signed_2);
  ("test_positive_float_not_signed", test_positive_float_not_signed);

  ("test_negative_float_signed_whitespace_both_sides", test_negative_float_signed_whitespace_both_sides);
  ("test_positive_float_signed_whitespace_both_sides", test_positive_float_signed_whitespace_both_sides);
  ("test_positive_float_not_signed_whitespace_both_sides", test_positive_float_not_signed_whitespace_both_sides);

  ("edge_float_leading_zeroes_positive", edge_float_leading_zeroes_positive);
  ("edge_float_leading_zeroes_negative", edge_float_leading_zeroes_negative);
  ("edge_float_leading_and_trailing_zeroes_positive", edge_float_leading_and_trailing_zeroes_positive);
  ("edge_float_spaces_between", edge_float_spaces_between);

  ("test_number_1", test_number_1);
  ("test_number_2", test_number_2);
  ("test_number_3", test_number_3);
  ("test_number_4", test_number_4);
  ("test_number_5", test_number_5);
  ("test_number_6", test_number_6);

  (* "test_assert_raises_1", test_assert_raises_1;
  "test_assert_raises_2", test_assert_raises_2;
  "test_assert_raises_3", test_assert_raises_3;
  "test_assert_raises_4", test_assert_raises_4; *)
  ]
;;
(* END TESTER: test_number_parser.ml *)
