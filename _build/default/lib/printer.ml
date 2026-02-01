
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

