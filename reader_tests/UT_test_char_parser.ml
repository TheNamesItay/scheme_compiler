#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

(* TESTER: test_char_parser.ml *)
let test_newline () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char '\n')] (Reader.read_sexprs " #\\newline");;
let test_newline_with_spaces () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char '\n')] (Reader.read_sexprs "   #\\newline ");;
let test_newline_with_spaces2 () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char '\n')] (Reader.read_sexprs "   #\\nEwline ");;
let test_return_with_spaces () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char '\r')] (Reader.read_sexprs "   #\\return    ");;
let test_charS_with_spaces () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char 'S')] (Reader.read_sexprs "   #\\S  ");;
let test_chara () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char 'a')] (Reader.read_sexprs "   #\\a ");;
let test_page_with_spaces () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char '\012')] (Reader.read_sexprs "   #\\page    ");;
let test_page_with_spaces2 () = Alcotest.(check (list sexp_testable)) "same char sexp" [(Char '\012')] (Reader.read_sexprs "   #\\pAGE    ");;
let test_char_with_extras () = Alcotest.(check (list sexp_testable)) "same char sexp" [Char 'A'; Symbol "b"] (Reader.read_sexprs "#\\Ab");;

(* Name the test cases and group them together *)
let char_parser_tester_suite =
 [("test_newline", test_newline);
  ("test_newline_with_spaces", test_newline_with_spaces);
  ("test_newline_with_spaces2", test_newline_with_spaces2);
  ("test_return_with_spaces", test_return_with_spaces);
  ("test_charS_with_spaces", test_charS_with_spaces);
  ("test_chara", test_chara);
  ("test_page_with_spaces", test_page_with_spaces);
  ("test_page_with_spaces2", test_page_with_spaces2);
  ("test_char_with_extras", test_char_with_extras);
  ]
;;
