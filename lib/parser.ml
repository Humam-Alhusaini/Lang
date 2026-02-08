
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
  | Eq

and cf = 
  | If_Else of expr * cf * cf
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
   | (EQ, _) -> Eq
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
      | hd :: _ -> (let (ftok, _) = hd in
                  if ftok = endtok then 
                    let _ = self#shift () in start else 
                    match toks with
                    | ((MULT | SUB | PLUS | EQ) as op, p) :: num :: _ -> self#shift_n 2; Binop(match_op (op, p), start, match_num num) |> parse_binop
                    | hd :: _ -> Parsing_error ("Expected expression to either end or continue", hd) |> raise
                    | [] -> Fatal "Can't find end token" |> raise)
       in
    
    match toks with
    | [] -> Fatal "No tokens to parse" |> raise
    | num :: _ -> self#shift (); match_num num |> parse_binop
  
  method parse_nested_cf (endtok : Tokens.t): cf = 
    match toks with 
    | (LBRACE, _) :: _ -> self#shift (); let cf = self#parse_cf RBRACE in
                          (match toks with
                          | [] -> Fatal "No tokens to parse" |> raise
                          | (ftok, p) :: _ -> if endtok = ftok then
                                              let _ = self#shift () in cf else 
                                              Parsing_error ("Expected token to end if statement", (ftok, p)) |> raise)
    | _ -> Nop (self#parse_expr endtok)

  method parse_cf (endtok : Tokens.t) : cf =  
    match toks with
    | (IF, _) :: _ -> self#shift (); let cond = self#parse_expr THEN in
                      let expr1 = self#parse_nested_cf ELSE in
                      let expr2 = self#parse_nested_cf SEMICOLON in
                          (match toks with
                          | [] -> Fatal "No tokens to parse" |> raise
                          | (ftok, p) :: _ -> if endtok = ftok then
                                              let _ = self#shift () in If_Else (cond, expr1, expr2) else 
                                              Parsing_error ("Expected token to end if statement", (ftok, p)) |> raise)
    | _ -> Nop (self#parse_expr endtok)

end
