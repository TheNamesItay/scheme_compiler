#use "topfind";;

#use "reader.ml";;
#use "sexp_test_utils.ml";;
#require "alcotest";;


(* TESTER: test_string_parser.ml *)
let test_simple_string () = Alcotest.(check (list sexp_testable)) "same string sexp" [(String "abc")] (Reader.read_sexprs "\"abc\"");;
let test_simple_string1 () = Alcotest.(check (list sexp_testable)) "same string sexp" [(String "  dbc  ")] (Reader.read_sexprs " \"  dbc  \"    ");;

let test_special_string () = Alcotest.(check (list sexp_testable)) "same string sexp" [(String "\\''")] (Reader.read_sexprs "\"\\\\''\"");;
let test_complex_string () = Alcotest.(check (list sexp_testable)) "same string sexp" [(String "A4ge4...\\\"\\\"\\\\\\\\\\\\\\\\ \\t lo \\t \\r \\\\ \\n  tr3")]
  (Reader.read_sexprs "    \"A4ge4...\\\\\\\"\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ \\\\t lo \\\\t \\\\r \\\\\\\\ \\\\n  tr3\"     ");;
let test_complex_string1 () = Alcotest.(check (list sexp_testable)) "same string sexp" [(String "hello \t \r \\ \n  world!")]
  (Reader.read_sexprs "\"hello \\t \\r \\\\ \\n  world!\"");;
let test_complex_string2 () = Alcotest.(check (list sexp_testable)) "same string sexp" [(String "A4ge4...\"\"\\\\\\t\t\t\t\t\t\t\n\\\\\\\'\'\'\"\" \t lo \t \r \\ \n  tr3")]
  (Reader.read_sexprs "\"A4ge4...\\\"\\\"\\\\\\\\\\\\t\\t\\t\\t\\t\\t\\t\\n\\\\\\\\\\\\'''\\\"\\\" \\t lo \\t \\r \\\\ \\n  tr3\"");;
(* let test_complex_string1 () = Alcotest.(check (list sexp_testable)) "same string sexp" (String "hello \t \r \\ \n  world!")
  (fst (String.nt_string "\"hello \\t \\r \\\\ \\n  world!\"")));;
let test_complex_string1 () = Alcotest.(check (list sexp_testable)) "same string sexp" (String "hello \t \r \\ \n  world!")
  (fst (String.nt_string "\"hello \\t \\r \\\\ \\n  world!\"")));; *)

let string_parser_tester_suite =
   [("test_simple_string", test_simple_string);
    ("test_simple_string1", test_simple_string1);
    ("test_special_string", test_special_string);
    ("test_complex_string", test_complex_string);
    ("test_complex_string1", test_complex_string1);
    ("test_complex_string2", test_complex_string2);
   ]
(* END TESTER: test_string_parser.ml *)
