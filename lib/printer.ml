
open Printf
open Parser

let format_tok (tok : Tokens.t) = 
  match tok with
  | NUM i -> sprintf "NUM(%i)" i
  | VAR s -> sprintf "VAR(%s)" s
  | MULT -> "MULT"
  | PLUS -> "PLUS"
  | SUB -> "SUB"
  | EQ -> "EQ"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | AND -> "AND"
  | OR -> "OR"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | COMMA -> "COMMA"
  | PERIOD -> "PERIOD"
  | NAT -> "NAT"
  | THEN -> "THEN"
  | EOF -> "EOF";;

let rec toks_to_tokens toks : Tokens.t list =
  match toks with
  | [] -> []
  | (token, _) :: tl -> token :: toks_to_tokens tl;;

let rec print_tokens toks : unit =
  match toks with
  | [] -> ()
  | hd :: tl ->
      format_tok hd |> printf "%s\n";
    print_tokens tl;;

let fop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Eq -> "=";;

let rec fexpr expr = 
  match expr with 
  | Num n -> sprintf "%d" n
  | Binop (op, expr1, expr2) -> 
      sprintf "(%s %s %s)" (fexpr expr1) (fop op) (fexpr expr2)

let rec fcf cf = 
  match cf with 
  | If (cond, expr1, expr2) -> sprintf "If %s then %s else %s" (fexpr cond) (fcf expr1) (fcf expr2)
  | Nop expr -> sprintf "%s" (fexpr expr)

let print (func : 'a -> string) (obj : 'a) =
  let str = func obj in
  printf "%s\n" str;;
