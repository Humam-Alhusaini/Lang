open Lexer
open Printer
open Parser
open Ctx

open Printf
  
let rec simplify_expr (ctx : expr_map) (expr : expr) : int =
  let match_op op expr1 expr2 = 
    match op with
    | Add -> simplify_expr ctx expr1 + simplify_expr ctx expr2
    | Sub -> simplify_expr ctx expr1 - simplify_expr ctx expr2
    | Mult -> simplify_expr ctx expr1 * simplify_expr ctx expr2
    | Eq -> if simplify_expr ctx expr1 = simplify_expr ctx expr2 then 1 else 0 in

  match expr with
  | Binop (op, expr1, expr2) -> match_op op expr1 expr2
  | Num y -> y
  | Var str -> simplify_expr ctx (find str ctx);;

(*let rec simplify_cf (ctx : expr_map) (cf : cf) : int =
  match cf with
  | If_Else (cond, expr1, expr2) -> if simplify_expr ctx cond > 0 then (simplify_cf ctx expr1) else (simplify_cf ctx expr2)
  | If (cond, expr) -> if simplify_expr ctx cond > 0 then (simplify_cf ctx expr) else 0
  | Nop expr -> simplify_expr ctx expr
*)
let rec simplify_ast (ctx : expr_map) (ast : ast) : ast  =
  match ast with
  | If (expr, ast1) -> if (simplify_expr ctx expr) > 0 then (simplify_ast ctx ast1) else Nop
  | Elif (expr, ast1, ast2) -> if (simplify_expr ctx expr) > 0 then (simplify_ast ctx ast1) else (simplify_ast ctx ast2)
  | Def (str, expr) -> Def (str, Num (simplify_expr ctx expr))
  | Ret expr -> Ret (Num (simplify_expr ctx expr)) 
  | Nop -> Nop

let rec do_stuff (ctx : expr_map) (ast : ast) : expr_map  =
  match ast with
  | If (expr, ast1) -> if (simplify_expr ctx expr) > 0 then (do_stuff ctx ast1) else ctx
  | Elif (expr, ast1, ast2) -> if (simplify_expr ctx expr) > 0 then (do_stuff ctx ast1) else (do_stuff ctx ast2)
  | Ret expr -> Num (simplify_expr ctx expr) |> print fexpr; ctx 
  | Nop -> ctx
  | Def (str, expr) -> let newexpr = Num (simplify_expr ctx expr) in
                       add str newexpr ctx

let read str (ctx : expr_map) : expr_map =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let parse = new parse tokens in
          let ast = parse#parse EOF in 
            let newctx = do_stuff ctx ast in newctx
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks; ctx
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok); ctx
  | Map_error err -> printf "\n"; printf "Value %s does not exist in context\n" err; ctx
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err; ctx;;

