open Lexer
open Printer
open Parser
open Printf

let rec simplify_expr (expr : expr) : int = 

  let match_op op expr1 expr2 = 
    match op with
    | Add -> simplify_expr expr1 + simplify_expr expr2
    | Sub -> simplify_expr expr1 - simplify_expr expr2
    | Mult -> simplify_expr expr1 - simplify_expr expr2 in

  match expr with
  | Binop (op, expr1, expr2) -> match_op op expr1 expr2
  | Num y -> y;;

let read str =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let parse = new parse_exp tokens in
          let ast = parse#parse_expr in 
            let newast = Num (simplify_expr ast) in
            printf "\n%s\n" (fexpr newast) 
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok)
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err;;

