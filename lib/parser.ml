
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
| EOF -> "EOF"
| DEF -> "DEF"
| RETURN -> "RETURN"
| ELIF -> "ELIF";;

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
| Var of string

and op = 
| Add
| Sub
| Mult
| Eq

and def = string * expr

and ast =
| Def of def
| Elif of expr * ast * ast
| If of expr * ast
| Ret of expr
| Nop

let match_num n =
  match n with
  | (NUM y, _) -> Num y
  | (TRUE, _) -> Num 1
  | (FALSE, _) -> Num 0
  | (VAR y, _) -> Var y
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

method end_err (toks : token list) (endtok : Tokens.t) (ast : ast) = 
  match toks with
  | [] -> Fatal "No tokens to parse" |> raise
  | (ftok, p) :: _ -> if endtok = ftok then
                      let _ = self#shift () in ast else 
                        Parsing_error ("Expected token to end statement", (ftok, p)) |> raise

method parse_expr (endtok : Tokens.t) : expr = 
  
  let rec parse_binop (curr : expr) : expr =
    match toks with
    | ((MULT | SUB | PLUS | EQ) as op, p) :: num :: _ -> self#shift_n 2; Binop(match_op (op, p), curr, match_num num) |> parse_binop
    | [] -> Fatal "Can't find end token" |> raise
    | (ftok, pos) :: _ -> (if ftok = endtok then let _ = self#shift () in curr else 
                    Parsing_error ("Expected expression to either end or continue", (ftok, pos)) |> raise) in
  
  match toks with
  | [] -> Fatal "No tokens to parse" |> raise
  | num :: _ -> self#shift (); match_num num |> parse_binop

method parse_nested_cf (endtok : Tokens.t): ast = 
  match toks with 
  | (LBRACE, _) :: _ -> self#shift (); let cf = self#parse RBRACE in self#end_err toks endtok cf
  | _ -> Fatal "Error in Nested Cf" |> raise

method parse_def (endtok : Tokens.t) : ast = 
  match toks with
  | (DEF, _) :: (VAR str, _) :: (EQ, _) :: _ -> self#shift_n 3; 
                                                let expr = self#parse_expr SEMICOLON in 
                                                let ast = Def (str, expr) in self#end_err toks endtok ast
  | _ -> Fatal "Issues" |> raise

method parse_if (endtok : Tokens.t) : ast = 
  self#shift (); let cond = self#parse_expr THEN in
    let nested_ast = self#parse_nested_cf SEMICOLON in
      let ast = If (cond, nested_ast) in self#end_err toks endtok ast

method parse_elif (endtok : Tokens.t) : ast = 
  self#shift (); let cond = self#parse_expr THEN in
    let ast1 = self#parse_nested_cf ELSE in
      let ast2 = self#parse_nested_cf SEMICOLON in
        let ast = Elif (cond, ast1, ast2) in self#end_err toks endtok ast

method parse_ret (endtok : Tokens.t) : ast = 
  self#shift (); let ast = Ret (self#parse_expr SEMICOLON) in self#end_err toks endtok ast

method parse (endtok : Tokens.t) : ast =
  match toks with
  | (DEF, _) :: _ -> self#parse_def endtok
  | (IF, _) :: _ -> self#parse_if endtok
  | (ELIF, _) :: _ -> self#parse_elif endtok
  | (RETURN, _) :: _ -> self#parse_ret endtok
  | hd :: _ -> Parsing_error ("Expected def, num, or control flow", hd) |> raise
  | [] -> Fatal "Nothing here! Contact maintainers!" |> raise

end
