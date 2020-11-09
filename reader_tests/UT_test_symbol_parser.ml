#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;

(* TESTER: test_symbol_parser.ml *)
let test_symbol_0 () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "a")] (Reader.read_sexprs "a");;
let test_symbol_1 () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "!0$aa!1")] (Reader.read_sexprs "!0$aA!1");;
let test_symbol_2 () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "zzaa!:=9")] (Reader.read_sexprs "zZaA!:=9");;
let test_symbol_3 () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "123a")] (Reader.read_sexprs "123a");;
let test_symbol_3 () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "123a")] (Reader.read_sexprs "123A");;

let test_symbol_space_left () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "!0$aa!1")] (Reader.read_sexprs "!0$aA!1");;
let test_symbol_space_right () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "!0$aa!1")] (Reader.read_sexprs "!0$aA!1");;
let test_symbol_spaces_both_sides () = Alcotest.(check (list sexp_testable)) "same symbol sexp" [(Symbol "!0$aa!1")] (Reader.read_sexprs "!0$aA!1");;

(* let test_symbol_raise_1 () = assert_raises X_no_match (fun _ -> (Reader.read_sexprs "!0$aA!1@2"));; *)


(* Name the test cases and group them together *)
let symbol_parser_tester_suite =
 [("test_symbol_0", test_symbol_0);
  ("test_symbol_1", test_symbol_1);
  ("test_symbol_2", test_symbol_2);
  ("test_symbol_3", test_symbol_3);

  ("test_symbol_space_left", test_symbol_space_left);
  ("test_symbol_space_right", test_symbol_space_right);
  ("test_symbol_spaces_both_sides", test_symbol_spaces_both_sides);

  (* "test_symbol_raise_1", test_symbol_raise_1; *)
  ]
;;

(* END TESTER: test_symbol_parser.ml *)
