#use "reader.ml";;

let rec string_of_sexp se = 
    let string_of_number n = match n with
    | Fraction((a, b)) -> "Fraction(" ^ (string_of_int a) ^ "/" ^ (string_of_int b) ^ ")"
    | Float(f) -> "Float(" ^ (string_of_float f) ^ ")" in  
    match se with
    | Bool(b) -> "Bool(" ^ (string_of_bool b) ^ ")"
    | Nil -> "Nil"
    | Number(n) -> string_of_number n
    | Char(c) ->  "Char(" ^ (String.make 1 c) ^")"
    | String(s) -> "String(" ^ s ^ ")"
    | Symbol(s) -> "Symbol(" ^ s ^ ")"
    | Pair((se1, se2)) -> "Pair(" ^ (string_of_sexp se1) ^ ", " ^ (string_of_sexp se2) ^ ")"

let sexp_testable = Alcotest.testable (fun ppf (se: sexpr) -> (Format.fprintf ppf "%s" (string_of_sexp se))) sexpr_eq;;
