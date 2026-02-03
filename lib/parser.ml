
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

 let match_num n =
   match n with
   | (NUM y, _) -> Num y
   | _ -> Parsing_error ("Expected num", n) |> raise;;

 let match_op op = 
   match op with
   | (MULT, _) -> Mult
   | (PLUS, _) -> Add
   | (SUB, _) -> Sub
   | _ -> Parsing_error ("Expected operator", op) |> raise;;

class parse_exp (tokens : token list) = object (self)
  
  val mutable toks = tokens

  method shift () =
   match toks with
   | [] -> Fatal "FATAL ERROR" |> raise
   | _ :: ls -> toks <- ls

  method shift_n num =
    let rec loop num =
      if num > 0 then begin
        self#shift (); loop (num-1) end
      else () in loop num

  method parse_expr : expr = 
    
    let rec parse_binop (start : expr) : expr =
      match toks with
      | op :: num :: _ -> self#shift_n 2; Binop(match_op op, start, match_num num) |> parse_binop
      | [] -> start
      | hd :: _ -> Parsing_error ("Expected expression to either end or continue", hd) |> raise
       in
    
    match toks with
    | (NUM y, _) :: _ -> self#shift (); parse_binop (Num y) 
    | hd :: _ -> Parsing_error ("Expected num", hd) |> raise
    | [] -> Fatal "Where the helly are the tokens" |> raise

end
