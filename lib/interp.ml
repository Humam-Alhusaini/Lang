open Lexer
open Printer
open Parser
open Printf

let rec simplify_expr (expr : expr) : int = 

  let match_op op expr1 expr2 = 
    match op with
    | Add -> simplify_expr expr1 + simplify_expr expr2
    | Sub -> simplify_expr expr1 - simplify_expr expr2
    | Mult -> simplify_expr expr1 * simplify_expr expr2
    | Eq -> if simplify_expr expr1 = simplify_expr expr2 then 1 else 0 in

  match expr with
  | Binop (op, expr1, expr2) -> match_op op expr1 expr2
  | Num y -> y;;

let rec simplify_cf (cf : cf) : int =
  match cf with
  | If (cond, expr1, expr2) -> if simplify_expr cond > 0 then (simplify_cf expr1) else (simplify_cf expr2)
  | Nop expr -> simplify_expr expr

let read str =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let parse = new parse tokens in
          let ast = parse#parse_cf EOF in 
            let newast = Num (simplify_cf ast) in
            printf "\n%s\n" (fexpr newast) 
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok)
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err;;

