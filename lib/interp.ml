
module CTX :
  sig
    type key = string
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
  end = Map.Make(String);;

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
  | If_Else (cond, expr1, expr2) -> if simplify_expr cond > 0 then (simplify_cf expr1) else (simplify_cf expr2)
  | If (cond, expr) -> if simplify_expr cond > 0 then (simplify_cf expr) else 0
  | Nop expr -> simplify_expr expr

let read str (ctx : expr CTX.t) : expr CTX.t =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let parse = new parse tokens in
          let (str, expr) = parse#parse_def in 
            let newexpr = Num (simplify_expr expr) in
              let _ = print fdef (str, newexpr) in
                let newctx = ctx |> CTX.add str newexpr in newctx
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks; ctx
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok); ctx
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err; ctx;;

