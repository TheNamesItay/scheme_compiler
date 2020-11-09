#use "topfind";;

#directory ".";;
#directory "./reader_tests";;
#use "reader.ml";;
#require "alcotest";;
#use "UT_test_bool_parser.ml";;
#use "UT_test_char_parser.ml";;
#use "UT_test_list_parser.ml";;
#use "UT_test_symbol_parser.ml";;
#use "UT_test_string_parser.ml";;
#use "UT_test_number_parser.ml";;
#use "UT_test_scientific_parser.ml";;

let boolean_test () =  Alcotest.(check (list sexp_testable)) "same sexp?" (Reader.read_sexprs "#t") [Bool true]

let string_with_meta_char_test () = 
  let test_str = "\"hello \\\"\"" in
  let expected_sexpr = String "hello \"" in
    Alcotest.(check bool) "same String?" true (sexpr_eq (List.nth (Reader.read_sexprs test_str) 0) expected_sexpr);;

let proper_list_test () = 
  let str_list = "(#t #f #\\h)" in
  let list_sexp = (Pair(Bool(true), Pair(Bool(false), Pair(Char('h'), Nil)))) in
    Alcotest.(check sexp_testable) "same proper list?" (List.nth (Reader.read_sexprs str_list) 0) list_sexp;;

let () =
  let open Alcotest in
  run "Test Parser" [
      "test boolean", (List.map (fun (desc, test) -> test_case desc `Quick test) bool_parser_tester_suite);
      "Test Char", (List.map (fun (desc, test) -> test_case desc `Quick test) char_parser_tester_suite);
      "Test symbol",  (List.map (fun (desc, test) -> test_case desc `Quick test) symbol_parser_tester_suite);
      "Test list",  (List.map (fun (desc, test) -> test_case desc `Quick test) list_parser_tester_suite);
      "Test string",  (List.map (fun (desc, test) -> test_case desc `Quick test) string_parser_tester_suite);
      "Test number",  (List.map (fun (desc, test) -> test_case desc `Quick test) number_parser_tester_suite);
      "Test scientific",  (List.map (fun (desc, test) -> test_case desc `Quick test) scientific_parser_tester_suite);
      (* add more tests here *)
    ]