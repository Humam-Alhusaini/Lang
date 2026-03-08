
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
  
let rec simplify_expr (ctx : expr CTX.t) (expr : expr) : int =
  let match_op op expr1 expr2 = 
    match op with
    | Add -> simplify_expr ctx expr1 + simplify_expr ctx expr2
    | Sub -> simplify_expr ctx expr1 - simplify_expr ctx expr2
    | Mult -> simplify_expr ctx expr1 * simplify_expr ctx expr2
    | Eq -> if simplify_expr ctx expr1 = simplify_expr ctx expr2 then 1 else 0 in

  match expr with
  | Binop (op, expr1, expr2) -> match_op op expr1 expr2
  | Num y -> y
  | Var str -> 1;;

let rec simplify_cf (ctx : expr CTX.t) (cf : cf) : int =
  match cf with
  | If_Else (cond, expr1, expr2) -> if simplify_expr ctx cond > 0 then (simplify_cf ctx expr1) else (simplify_cf ctx expr2)
  | If (cond, expr) -> if simplify_expr ctx cond > 0 then (simplify_cf ctx expr) else 0
  | Nop expr -> simplify_expr ctx expr

let do_stuff (ctx : expr CTX.t) (ast : ast) : (expr CTX.t)  =
  match ast with
  | Expr expr ->  Num (simplify_expr ctx expr) |> print fexpr; ctx 
  | Cf cf -> Num (simplify_cf ctx cf) |> print fexpr; ctx
  | Def (str, expr) ->
            let newexpr = Num (simplify_expr ctx expr) in
              let _ = print fdef (str, newexpr) in
                let newctx = ctx |> CTX.add str newexpr in newctx

let read str (ctx : expr CTX.t) : expr CTX.t =
  try
    let lex = new lexer str in
      let tokens = lex#tokenize [] in 
        let parse = new parse tokens in
          let ast = parse#parse in 
            let newctx = do_stuff ctx ast in newctx
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks; ctx
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok); ctx
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err; ctx;;

