
open Lexer
open Tokens
open Printf

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

let format_pos (pos : position) : string = 
  sprintf "line %d, offset %d" pos.line_num pos.bol_off;;

let format_token (token : token) : string = 
  let (tok, pos) = token in
    sprintf "%s at %s" (format_tok tok) (format_pos pos);;

let error_of_token (err: string) (token : token) : string =
  sprintf "%s, got %s" err (format_token token);;

exception Parsing_error of string * token
exception Fatal of string

type expr = 
  | Num of int
  | Binop of op * expr * expr

and op = 
  | Add
  | Sub
  | Mult

and cf = 
  | If of expr * expr * expr
  | Nop of expr

 let match_num n =
   match n with
   | (NUM y, _) -> Num y
   | (TRUE, _) -> Num 1
   | (FALSE, _) -> Num 0
   | _ -> Parsing_error ("Expected num", n) |> raise;;

 let match_op op = 
   match op with
   | (MULT, _) -> Mult
   | (PLUS, _) -> Add
   | (SUB, _) -> Sub
   | _ -> Parsing_error ("Expected operator", op) |> raise;;

class parse (tokens : token list) = object (self)
  
  val mutable toks = tokens

  method shift () =
   match toks with
   | [] -> Fatal "No more tokens" |> raise
   | _ :: ls -> toks <- ls

  method shift_n num =
    let rec loop num =
      if num > 0 then begin
        self#shift (); loop (num-1) end
      else () in loop num
  
  

  method parse_expr (endtok : Tokens.t) : expr = 
    
    let rec parse_binop (start : expr) : expr =
      match toks with 
      | [] -> Fatal "Empty" |> raise
      | _ -> (let (ftok, _) = List.hd toks in
                  if ftok = endtok then let _ = self#shift () in start else let _ = Printf.printf "Ftok: %s, Endtok: %s" (format_tok ftok) (format_tok endtok) in
                  match toks with
                  | op :: num :: _ -> self#shift_n 2; Binop(match_op op, start, match_num num) |> parse_binop
                  | hd :: _ -> Parsing_error ("Expected expression to either end or continue", hd) |> raise
                  | [] -> Fatal "Can't find EOF token" |> raise)
       in
    
    match toks with
    | hd :: _ -> self#shift (); (match_num hd) |> parse_binop 
    | [] -> Fatal "No tokens to parse" |> raise

  method parse_cf : cf =  
    match toks with
    | (IF, _) :: _ -> self#shift (); let cond = self#parse_expr THEN in
                                     let expr1 = self#parse_expr ELSE in
                                     let expr2 = self#parse_expr EOF in
                                     If (cond, expr1, expr2)
    | _ -> Nop (self#parse_expr EOF)

end
