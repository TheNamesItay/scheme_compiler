#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

(* TESTER: test_list_parser.ml *)

let test_number_list () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (5, 1)), Nil)))]
                                              (Reader.read_sexprs "(1 5)");;
let test_combined_list_1 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Number (Fraction (1, 1)), Pair (Symbol ("b"), Pair (Number (Fraction (5, 1)), Nil))))]
                                              (Reader.read_sexprs "(1 b 5)");;
let test_nested_list_1 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Pair (Number (Fraction (1, 1)), (Pair (Pair (Symbol ("b"), Nil), Nil))), Pair (Number (Fraction (5, 1)), Nil)))]
                                              (Reader.read_sexprs "((1 (b)) 5)");;

let test_empty_list () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Nil)] (Reader.read_sexprs "()");;
let test_comment_in_list_1 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (String "this", Nil))] (Reader.read_sexprs "(\"this\" ; hahahahahaa \n)");;
let test_comment_in_list_2 () = Alcotest.(check (list sexp_testable)) "same list sexp" ([Nil]) (Reader.read_sexprs "( ;sad\n )");;
let test_sexp_comment_in_list_1 () = Alcotest.(check (list sexp_testable)) "same list sexp" ([Nil]) (Reader.read_sexprs "( #; 1 )");;


let test_dotted_list_1 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (5, 1)), Nil)), Number (Fraction (6, 1))))]
                                              (Reader.read_sexprs "((1 5) . 6)");;
let test_dotted_list_2 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Number (Float 1.2), Number (Fraction(3, 1))))]
                                              (Reader.read_sexprs "(1.2 . 3)");;
let test_dotted_list_3 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Symbol ("a"), Symbol ("b")))]
                                              (Reader.read_sexprs "(a . b)");;
let test_dotted_list_4 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Symbol ("b"), Pair (Symbol ("a"), Symbol ("b"))))]
                                                (Reader.read_sexprs "(b a . b)");;

let test_dotted_list_nested_1 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Symbol ("a"), (Pair (Symbol ("b"), Symbol ("c")))))]
                                              (Reader.read_sexprs "(a . (b . c))");;
let test_dotted_list_nested_2 () = Alcotest.(check (list sexp_testable)) "same list sexp" [(Pair (Symbol ("a"), (Pair (Symbol ("b"), Pair (Symbol ("c"), (Pair (Symbol ("d"), Symbol ("e"))))))))]
                                              (Reader.read_sexprs "(a . (b . (c . (d . e))))");;

(* Name the test cases and group them together *)
let list_parser_tester_suite =
 [("test_number_list", test_number_list);
  ("test_combined_list_1", test_combined_list_1);
  ("test_nested_list_1", test_nested_list_1);
  ("test_empty_list", test_empty_list);
  ("test_comment_in_list_1", test_comment_in_list_1);
  ("test_comment_in_list_2", test_comment_in_list_2);
  
  ("test_sexp_comment_in_list_1", test_sexp_comment_in_list_1);

  ("test_dotted_list_1", test_dotted_list_1);
  ("test_dotted_list_2", test_dotted_list_2);
  ("test_dotted_list_3", test_dotted_list_3);
  ("test_dotted_list_4", test_dotted_list_4);

  ("test_dotted_list_nested_1", test_dotted_list_nested_1);
  ("test_dotted_list_nested_2", test_dotted_list_nested_2);
  ]
;;
